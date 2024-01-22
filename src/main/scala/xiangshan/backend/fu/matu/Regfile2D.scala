package xiangshan.backend.fu.matu

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._

class Regfile_2D_wrapper (implicit  p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val data_in = Input(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
    val uop_in = Input(Vec(exuParameters.LduCnt, new MicroOp))
    val valid_in = Input(Vec(exuParameters.LduCnt, Bool()))
  })

    val rf2D =  Module(new Regfile_2D)

    val wdata_w  = Wire(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
    val wr_en = Wire(Vec(exuParameters.LduCnt, Bool()))
    val waddr_w = Wire(Vec(exuParameters.LduCnt, UInt(3.W)))
    val woffset_w = Wire(Vec(exuParameters.LduCnt, UInt(2.W)))

    for (i <- 0 until exuParameters.LduCnt) {
      wdata_w(i) := io.data_in(i)
      wr_en(i) := io.valid_in(i) && io.uop_in(i).ctrl.fuOpType === LSUOpType.mld
      woffset_w(i) := io.uop_in(i).cf.instr(11, 10)
      waddr_w(i) := io.uop_in(i).cf.instr(9, 7)
    }

   for (i <- 0 until exuParameters.LduCnt) {
     rf2D.io.wr_en(i) := wr_en(i)
     rf2D.io.waddr(i) := waddr_w(i)
     rf2D.io.wdata(i) := wdata_w(i)
     rf2D.io.woffset(i) := woffset_w(i)
   }


}

class Regfile_2D(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val wr_en = Input(Vec(exuParameters.LduCnt, Bool()))
    val waddr = Input(Vec(exuParameters.LduCnt, UInt(3.W)))
    val woffset = Input(Vec(exuParameters.LduCnt, UInt(2.W)))
    val wdata = Input(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
  })

  val regfile2d = dontTouch(Reg(Vec(8, Vec(2, UInt(64.W)))))



  for (i <- 0 until exuParameters.LduCnt) {
    when (io.wr_en(i)) {
      regfile2d(io.waddr(i))(io.woffset(i)) := io.wdata(i)
    }
  }

}

//class Reg_2D(implicit p: Parameters) extends XSModule {
//  val io = IO(new Bundle {
//    val wen = Input(Bool())
//    val woffset = Input(UInt(2.W))
//    val data_in = Input(UInt(XLEN.W))
//  })
//
//  val data2d = dontTouch(RegInit(VecInit(Seq.fill(2)(0.U(XLEN.W)))))
//  when (io.wen) {
//    data2d(io.woffset) := io.data_in
//  }
//}