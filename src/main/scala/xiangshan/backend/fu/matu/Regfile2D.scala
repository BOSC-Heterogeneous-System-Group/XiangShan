package xiangshan.backend.fu.matu

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._

class load_in(implicit p: Parameters) extends XSBundle {
  val data_in = Input(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
  val uop_in = Input(Vec(exuParameters.LduCnt, new MicroOp))
  val valid_in = Input(Vec(exuParameters.LduCnt, Bool()))
}

class fu_rf_in(implicit p: Parameters) extends XSBundle {
  val waddr_in = Input(UInt(3.W))
  val wdata_in = Input(Vec(2, UInt(XLEN.W)))
  val valid_in = Input(Bool())

  val raddr_in = Input(Vec(2, UInt(3.W)))
  val rdata_out = Output(Vec(2, Vec(2, UInt(XLEN.W))))
}

class commits_rf_in(implicit p: Parameters) extends XSBundle {
  val commits_pc = Input(Vec(CommitWidth, UInt(VAddrBits.W)))
  val commits_valid = Input(Vec(CommitWidth, Bool()))

}

class writeback_info(implicit p: Parameters) extends XSBundle {
  val ld_wen = Output(Vec(2, Bool()))
  val ld_waddr = Output(Vec(2, UInt(3.W)))
  val ld_woffset = Output(Vec(2, UInt(2.W)))
  val fu_wen = Output(Bool())
  val fu_waddr = Output(UInt(3.W))
}

class Regfile_2D_wrapper (implicit  p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ldIn = new load_in()
    val fuIO = new fu_rf_in()
    val commitsIn = new commits_rf_in()
    val wbInfoOut = new writeback_info()
  })

  val rf2D =  Module(new Regfile_2D)

  val ld_wdata_w  = Wire(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
  val ld_wr_en = Wire(Vec(exuParameters.LduCnt, Bool()))
  val ld_waddr_w = Wire(Vec(exuParameters.LduCnt, UInt(3.W)))
  val ld_woffset_w = Wire(Vec(exuParameters.LduCnt, UInt(2.W)))
  val ld_pc_w = Wire(Vec(exuParameters.LduCnt, UInt(VAddrBits.W)))

  for (i <- 0 until exuParameters.LduCnt) {
    ld_wdata_w(i) := io.ldIn.data_in(i)
    ld_wr_en(i) := io.ldIn.valid_in(i) && io.ldIn.uop_in(i).ctrl.fuOpType === LSUOpType.mld
    ld_woffset_w(i) := io.ldIn.uop_in(i).cf.instr(11, 10)
    ld_waddr_w(i) := io.ldIn.uop_in(i).cf.instr(9, 7)
    ld_pc_w(i) := io.ldIn.uop_in(i).cf.pc
  }

  val wdata_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(XLEN.W)))))
  val waddr_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(3.W)))))
  val woffset_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(2.W)))))
  val pc_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(VAddrBits.W)))))
  val writePtr = RegInit(VecInit(Seq.fill(exuParameters.LduCnt)(0.U(4.W))))
  val readPtr = RegInit(VecInit(Seq.fill(exuParameters.LduCnt)(0.U(4.W))))

  for (i <- 0 until exuParameters.LduCnt) {
    when (ld_wr_en(i) === true.B) {
      wdata_buffer(i)(writePtr(i)) := ld_wdata_w(i)
      waddr_buffer(i)(writePtr(i)) := ld_waddr_w(i)
      woffset_buffer(i)(writePtr(i)) := ld_woffset_w(i)
      pc_buffer(i)(writePtr(i)) := ld_pc_w(i)
      writePtr(i) := writePtr(i) + 1.U
    }
  }

  val ld_wr_en_r = dontTouch(Reg(Vec(exuParameters.LduCnt, Bool())))
  val ld_writeback_addr_r = dontTouch(Reg(Vec(exuParameters.LduCnt, UInt(3.W))))
  val ld_writeback_offset_r = dontTouch(Reg(Vec(exuParameters.LduCnt, UInt(2.W))))

  io.wbInfoOut.ld_wen <> ld_wr_en_r
  io.wbInfoOut.ld_waddr <> ld_writeback_addr_r
  io.wbInfoOut.ld_woffset <> ld_writeback_offset_r
  io.wbInfoOut.fu_wen := io.fuIO.valid_in
  io.wbInfoOut.fu_waddr := io.fuIO.waddr_in

  for (i <- 0 until exuParameters.LduCnt) {
    {
      val commit_flag = Seq.tabulate(CommitWidth)(j =>
        io.commitsIn.commits_valid(j) && io.commitsIn.commits_pc(j) === pc_buffer(i)(readPtr(i))
      )
      rf2D.io.ld_wr_en(i) := commit_flag.reduce(_ || _)
      rf2D.io.ld_waddr(i) := waddr_buffer(i)(readPtr(i))
      rf2D.io.ld_wdata(i) := wdata_buffer(i)(readPtr(i))
      rf2D.io.ld_woffset(i) := woffset_buffer(i)(readPtr(i))
      ld_wr_en_r(i) := commit_flag.reduce(_ || _)
      ld_writeback_addr_r(i) := waddr_buffer(i)(readPtr(i))
      ld_writeback_offset_r(i) := woffset_buffer(i)(readPtr(i))
      readPtr(i) := readPtr(i) + commit_flag.reduce(_ || _).asUInt
    }
  }
  rf2D.io.fu_wr_en := io.fuIO.valid_in
  rf2D.io.fu_waddr := io.fuIO.waddr_in
  rf2D.io.fu_wdata <> io.fuIO.wdata_in

  rf2D.io.raddr <> io.fuIO.raddr_in
  io.fuIO.rdata_out <> rf2D.io.rdata



  /*for (i <- 0 until exuParameters.LduCnt) {
    rf2D.io.wr_en(i) := wr_en(i)
    rf2D.io.waddr(i) := waddr_w(i)
    rf2D.io.wdata(i) := wdata_w(i)
    rf2D.io.woffset(i) := woffset_w(i)
  }*/


}

class Regfile_2D(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ld_wr_en = Input(Vec(exuParameters.LduCnt, Bool()))
    val ld_waddr = Input(Vec(exuParameters.LduCnt, UInt(3.W)))
    val ld_woffset = Input(Vec(exuParameters.LduCnt, UInt(2.W)))
    val ld_wdata = Input(Vec(exuParameters.LduCnt, UInt(XLEN.W)))

    val fu_wr_en = Input(Bool())
    val fu_waddr = Input(UInt(3.W))
    val fu_wdata = Input(Vec(2, UInt(XLEN.W)))

    val raddr = Input(Vec(2, UInt(3.W)))
    val rdata = Output(Vec(2, Vec(2, UInt(XLEN.W))))
  })

  val regfile2d = dontTouch(RegInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(2)(0.U(XLEN.W)))))))

  for (i <- 0 until exuParameters.LduCnt) {
    when (io.ld_wr_en(i) && (io.ld_waddr(i) > 0.U)) {
      regfile2d(io.ld_waddr(i))(io.ld_woffset(i)) := io.ld_wdata(i)
    }
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