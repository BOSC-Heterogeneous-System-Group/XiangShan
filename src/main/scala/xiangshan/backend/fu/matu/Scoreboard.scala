package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan.{MicroOp, _}
import xiangshan.backend.exu.ExuParameters
import xiangshan.backend.fu._

class dispatch_in(implicit p: Parameters) extends XSBundle {
  val uop_in = Input(Vec(2 * dpParams.IntDqDeqWidth, new MicroOp))
  val valid_in = Input(Vec(2 * dpParams.IntDqDeqWidth, Bool()))
}

class commits_scb_in(implicit p: Parameters) extends XSBundle {
  val commits_pc = Input(Vec(CommitWidth,  UInt(VAddrBits.W)))
  val commits_valid = Input(Vec(CommitWidth, Bool()))
}

class writeback_in(implicit p: Parameters) extends XSBundle {
  val wen = Input(Vec(2, Bool()))
  val waddr = Input(Vec(2, UInt(3.W)))
  val woffset = Input(Vec(2, UInt(2.W)))
}

class Scoreboard (implicit  p: Parameters) extends XSModule with HasXSParameter {
  val io = IO(new Bundle {
    val dpIn = new dispatch_in()
    val commitsIn = new commits_scb_in()
    val wbIn= new writeback_in()
  })

  val s_idle :: s_wait :: s_commit :: s_retire :: Nil = Enum(4)


  val OpType_w = Wire(Vec(2*dpParams.IntDqDeqWidth, FuOpType()))
  val rd_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(3.W)))
  val offset_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(2.W)))
  val pc_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(VAddrBits.W)))
  val robIdx_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(5.W)))
  val dp_valid_w = Wire(Vec(2*dpParams.IntDqDeqWidth, Bool()))


  for(i <- 0 until 2 * dpParams.IntDqDeqWidth) {
    OpType_w(i) := io.dpIn.uop_in(i).ctrl.fuOpType
    rd_w(i) := io.dpIn.uop_in(i).cf.instr(9, 7)
    offset_w(i) := io.dpIn.uop_in(i).cf.instr(11, 10)
    pc_w(i) := io.dpIn.uop_in(i).cf.pc
    robIdx_w(i) := io.dpIn.uop_in(i).robIdx.value
    dp_valid_w(i) := io.dpIn.valid_in(i) && io.dpIn.uop_in(i).cf.instr(6, 0) === "b0101011".U
  }

  /** reorder
   * the instrs that enter the array through  8 channels are out of order, need to be first reordered
   */

  val cmp = Seq.fill(8)(Wire(Vec(7, Bool())))
  val cmp_u = cmp.map(c => dontTouch(c.asUInt))

  val Idx_order = dontTouch(WireInit(VecInit(Seq.fill(8)(0.U(4.W)))))
  val OpType_order = dontTouch(WireInit(VecInit(Seq.fill(8)(0.U(7.W)))))
  val rd_order = dontTouch(WireInit(VecInit(Seq.fill(8)(0.U(3.W)))))
  val offset_order = dontTouch(WireInit(VecInit(Seq.fill(8)(0.U(2.W)))))
  val pc_order = dontTouch(WireInit(VecInit(Seq.fill(8)(0.U(VAddrBits.W)))))
  val robIdx_order = dontTouch(WireInit(VecInit(Seq.fill(8)(0.U(4.W)))))
  val valid_order = dontTouch(WireInit(VecInit(Seq.fill(8)(false.B))))

  val state_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_idle))))
  val OpType_array = dontTouch(Reg(Vec(32, FuOpType())))
  val rd_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val offset_array = dontTouch(Reg(Vec(32, UInt(2.W))))
  val pc_array = dontTouch(Reg(Vec(32, UInt(VAddrBits.W))))
  val robIdx_array = dontTouch(Reg(Vec(32, UInt(4.W))))

  val writePtr = RegInit(0.U(6.W))
  val next_writePtr = Wire(UInt(6.W))
  val readPtr = RegInit(0.U(6.W))
  next_writePtr := writePtr + PopCount(valid_order)
  writePtr := next_writePtr

  val enq_offset = dontTouch(Wire(Vec(8, UInt(6.W))))
  val offset = (0 until 8).map(i => PopCount(valid_order.take(i)))
  enq_offset := offset

  for (i <- 0 until 8) {
    for (j <- 0 until i) {
      cmp(i)(j) := robIdx_w(i) > robIdx_w(j)
    }
    for (j <- i until 7) {
      cmp(i)(j) := robIdx_w(i) > robIdx_w(j + 1)
    }

    Idx_order(PopCount(cmp_u(i))) := i.U
    OpType_order(i) := OpType_w(Idx_order(i))
    rd_order(i) := rd_w(Idx_order(i))
    offset_order(i) := offset_w(Idx_order(i))
    pc_order(i) := pc_w(Idx_order(i))
    robIdx_order(PopCount(cmp_u(i))) := robIdx_w(i)
    valid_order(i) := dp_valid_w(Idx_order(i))
  }

  /** enqueue
   * enqueue
   * state: s_idle -> s_wait
   * when the type of a valid instr is Matrix-extension, the instr can enqueue
   * the corresponding state is set to s_wait
   * Entry type: 1. OpType  2. rd_addr  3. rd_offset  4. pc  5. robIdx  6. state
   */
  for (i <- 0 until 8) {
    when(state_array(writePtr(4, 0) + enq_offset(i)) === s_idle && valid_order(i) === true.B) {
      OpType_array(writePtr(4, 0) + enq_offset(i)) := OpType_order(i)
      rd_array(writePtr(4, 0) + enq_offset(i)) := rd_order(i)
      offset_array(writePtr(4, 0) + enq_offset(i)) := offset_order(i)
      pc_array(writePtr(4, 0) + enq_offset(i)) := pc_order(i)
      robIdx_array(writePtr(4, 0) + enq_offset(i)) := robIdx_order(i)
      state_array(writePtr(4, 0) + enq_offset(i)) := s_wait
    }
  }

  /** state switch
   * state: s_wait -> s_commit
   * when the instr is commited in rob, the state of instr is set to s_commit
   * state: s_commit -> s_retire
   * when the instr finally writeback data to regfile, the state of instr is set to s_retire
   */
  for (i <- 0 until 31) {
    val commit_flag = Seq.tabulate(CommitWidth)(j =>
      state_array(i) === s_wait && io.commitsIn.commits_valid(j) && io.commitsIn.commits_pc(j) === pc_array(i)
    )
    when(state_array(i) === s_wait) {
      state_array(i) := Mux(commit_flag.reduce(_||_), s_commit, s_wait)
    }
  }

  val commitVec = Seq.tabulate(2)(i =>
    VecInit.tabulate(32)(j =>
      state_array(j) === s_commit &&
      io.wbIn.wen(i) &&
      io.wbIn.waddr(i) === rd_array(j) &&
      io.wbIn.woffset(i) === offset_array(j)
    )
  )


  for (i <- 0 until 2) {
    when(commitVec(i).asUInt.orR) {
      state_array(PriorityEncoder(commitVec(i))) := s_retire
    }
  }

  /** dequeue
   * state: s_retire -> s_idle
   *
   */
  when(state_array(readPtr) === s_retire) {
    state_array(readPtr) := s_idle
    readPtr := readPtr + 1.U
  }

}