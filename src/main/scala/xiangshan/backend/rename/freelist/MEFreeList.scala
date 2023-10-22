/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.rename.freelist

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._


class MEFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) with HasPerfEvents {
  val freeList = RegInit(VecInit(
    // originally {1, 2, ..., size - 1} are free. Register 0-31 are mapped to x0.
    Seq.tabulate(size - 1)(i => (i + 1).U(PhyRegIdxWidth.W)) :+ 0.U(PhyRegIdxWidth.W))) //从1到i+1，在末尾添加0

  // head and tail pointer
  val headPtr = RegInit(FreeListPtr(false, 0))
  val headPtrOH = RegInit(1.U(size.W))
  XSError(headPtr.toOH =/= headPtrOH, p"wrong one-hot reg between $headPtr and $headPtrOH")
  val headPtrOHShift = CircularShift(headPtrOH) //循环左移
  // may shift [0, RenameWidth] steps
  val headPtrOHVec = VecInit.tabulate(RenameWidth + 1)(headPtrOHShift.left) //6个元素，每个元素都是headptrOHShift循环左移的结果
  val tailPtr = RegInit(FreeListPtr(false, size - 1))

  val doRename = io.canAllocate && io.doAllocate && !io.redirect && !io.walk

  /**
    * Allocation: from freelist (same as StdFreelist)
    */
  val phyRegCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, freeList))) //分配给6个元素的Freelist中的候选寄存器
  for (i <- 0 until RenameWidth) {
    // enqueue instr, is move elimination
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i))) //如果没有目的寄存器，则将上一个分配的物理寄存器给给到allocate PhyReg
  }
  // update head pointer
  val numAllocate = PopCount(io.allocateReq)
  val headPtrNext = headPtr + numAllocate
  headPtr := Mux(doRename, headPtrNext, headPtr)
  headPtrOH := Mux(doRename, headPtrOHVec(numAllocate), headPtrOH)


  /**
    * Deallocation: when refCounter becomes zero, the register can be released to freelist
    */
  for (i <- 0 until CommitWidth) {
    when (io.freeReq(i)) {
      val freePtr = tailPtr + PopCount(io.freeReq.take(i))
      freeList(freePtr.value) := io.freePhyReg(i)
    }
  }

  // update tail pointer
  val tailPtrNext = tailPtr + PopCount(io.freeReq)
  tailPtr := tailPtrNext

  val freeRegCnt = Mux(doRename, distanceBetween(tailPtrNext, headPtrNext), distanceBetween(tailPtrNext, headPtr))
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= RenameWidth.U //是否可以进行分配，空余的寄存器数量大于6才可以

  val perfEvents = Seq(
    ("me_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                     ),
    ("me_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg <= (size / 2).U    ),
    ("me_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg <= (size * 3 / 4).U),
    ("me_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                 ),
  )
  generatePerfEvent()
}
