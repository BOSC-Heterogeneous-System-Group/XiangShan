package xiangshan.backend.fu.matu

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._

class scb_load_in(implicit p: Parameters) extends XSBundle {
  val waddr_in = Input(UInt(3.W))
  val woffset_in = Input(UInt(2.W))
  val wdata_in = Input(UInt(XLEN.W))
  val valid_in = Input(Bool())
}

class fu_rf_io(implicit p: Parameters) extends XSBundle {
  val waddr_in = Input(UInt(3.W))
  val wdata_in = Input(Vec(2, UInt(XLEN.W)))
  val valid_in = Input(Bool())

  val raddr_in = Input(Vec(2, UInt(3.W)))
  val rdata_out = Output(Vec(2, Vec(2, UInt(XLEN.W))))
}

class writeback_info(implicit p: Parameters) extends XSBundle {
  val ld_wen = Output(Bool())
  val ld_waddr = Output(UInt(3.W))
  val ld_woffset = Output(UInt(2.W))
  val fu_wen = Output(Bool())
  val fu_waddr = Output(UInt(3.W))
}

class Regfile_2D_wrapper (implicit  p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ldIn = new scb_load_in()
    val fuIO = new fu_rf_io()
    val wbInfoOut = new writeback_info()
  })

  val rf2D =  Module(new Regfile_2D)

  rf2D.io.ld_wr_en := io.ldIn.valid_in
  rf2D.io.ld_waddr := io.ldIn.waddr_in
  rf2D.io.ld_woffset := io.ldIn.woffset_in
  rf2D.io.ld_wdata := io.ldIn.wdata_in

  io.wbInfoOut.ld_wen := io.ldIn.valid_in
  io.wbInfoOut.ld_waddr := io.ldIn.waddr_in
  io.wbInfoOut.ld_woffset := io.ldIn.woffset_in
  io.wbInfoOut.fu_wen := io.fuIO.valid_in
  io.wbInfoOut.fu_waddr := io.fuIO.waddr_in

  rf2D.io.fu_wr_en := io.fuIO.valid_in
  rf2D.io.fu_waddr := io.fuIO.waddr_in
  rf2D.io.fu_wdata <> io.fuIO.wdata_in

  rf2D.io.raddr <> io.fuIO.raddr_in
  io.fuIO.rdata_out <> rf2D.io.rdata

}

class Regfile_2D(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ld_wr_en = Input(Bool())
    val ld_waddr = Input(UInt(3.W))
    val ld_woffset = Input(UInt(2.W))
    val ld_wdata = Input(UInt(XLEN.W))

    val fu_wr_en = Input(Bool())
    val fu_waddr = Input(UInt(3.W))
    val fu_wdata = Input(Vec(2, UInt(XLEN.W)))

    val raddr = Input(Vec(2, UInt(3.W)))
    val rdata = Output(Vec(2, Vec(2, UInt(XLEN.W))))
  })

  val regfile2d = dontTouch(RegInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(2)(0.U(XLEN.W)))))))

  when (io.ld_wr_en && (io.ld_waddr > 0.U)) {
      regfile2d(io.ld_waddr)(io.ld_woffset) := io.ld_wdata
  }

  when (io.fu_wr_en && (io.fu_waddr > 0.U)) {
    regfile2d(io.fu_waddr) <> io.fu_wdata
  }

  for (i <- 0 until 2) {
    io.rdata(i) <> regfile2d(io.raddr(i))
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