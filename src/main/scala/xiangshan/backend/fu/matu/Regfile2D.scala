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

class commits_rf_in(implicit p: Parameters) extends XSBundle {
  val commits_pc = Input(Vec(CommitWidth, UInt(VAddrBits.W)))
  val commits_valid = Input(Vec(CommitWidth, Bool()))

}

class writeback_info(implicit p: Parameters) extends XSBundle {
  val wen = Output(Vec(2, Bool()))
  val waddr = Output(Vec(2, UInt(3.W)))
  val woffset = Output(Vec(2, UInt(2.W)))
}

class Regfile_2D_wrapper (implicit  p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ldIn = new load_in()
    val commitsIn = new commits_rf_in()
    val wbInfoOut = new writeback_info()
  })

  val rf2D =  Module(new Regfile_2D)

  val wdata_w  = Wire(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
  val wr_en = Wire(Vec(exuParameters.LduCnt, Bool()))
  val waddr_w = Wire(Vec(exuParameters.LduCnt, UInt(3.W)))
  val woffset_w = Wire(Vec(exuParameters.LduCnt, UInt(2.W)))
  val pc_w = Wire(Vec(exuParameters.LduCnt, UInt(VAddrBits.W)))

  for (i <- 0 until exuParameters.LduCnt) {
    wdata_w(i) := io.ldIn.data_in(i)
    wr_en(i) := io.ldIn.valid_in(i) && io.ldIn.uop_in(i).ctrl.fuOpType === LSUOpType.mld
    woffset_w(i) := io.ldIn.uop_in(i).cf.instr(11, 10)
    waddr_w(i) := io.ldIn.uop_in(i).cf.instr(9, 7)
    pc_w(i) := io.ldIn.uop_in(i).cf.pc
  }

  val wdata_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(XLEN.W)))))
  val waddr_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(3.W)))))
  val woffset_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(2.W)))))
  val pc_buffer = dontTouch(Reg(Vec(exuParameters.LduCnt, Vec(8, UInt(VAddrBits.W)))))
  val writePtr = RegInit(VecInit(Seq.fill(exuParameters.LduCnt)(0.U(4.W))))
  val readPtr = RegInit(VecInit(Seq.fill(exuParameters.LduCnt)(0.U(4.W))))

  for (i <- 0 until exuParameters.LduCnt) {
    when (wr_en(i) === true.B) {
      wdata_buffer(i)(writePtr(i)) := wdata_w(i)
      waddr_buffer(i)(writePtr(i)) := waddr_w(i)
      woffset_buffer(i)(writePtr(i)) := woffset_w(i)
      pc_buffer(i)(writePtr(i)) := pc_w(i)
      writePtr(i) := writePtr(i) + 1.U
    }
  }

  val wr_en_r = dontTouch(Reg(Vec(exuParameters.LduCnt, Bool())))
  val writeback_addr_r = dontTouch(Reg(Vec(exuParameters.LduCnt, UInt(3.W))))
  val writeback_offset_r = dontTouch(Reg(Vec(exuParameters.LduCnt, UInt(2.W))))

  io.wbInfoOut.wen <> wr_en_r
  io.wbInfoOut.waddr <> writeback_addr_r
  io.wbInfoOut.woffset <> writeback_offset_r

  for (i <- 0 until exuParameters.LduCnt) {
    {
      val commit_flag = Seq.tabulate(CommitWidth)(j =>
        io.commitsIn.commits_valid(j) && io.commitsIn.commits_pc(j) === pc_buffer(i)(readPtr(i))
      )
      rf2D.io.wr_en(i) := commit_flag.reduce(_ || _)
      rf2D.io.waddr(i) := waddr_buffer(i)(readPtr(i))
      rf2D.io.wdata(i) := wdata_buffer(i)(readPtr(i))
      rf2D.io.woffset(i) := woffset_buffer(i)(readPtr(i))
      wr_en_r(i) := commit_flag.reduce(_ || _)
      writeback_addr_r(i) := waddr_buffer(i)(readPtr(i))
      writeback_offset_r(i) := woffset_buffer(i)(readPtr(i))
      readPtr(i) := readPtr(i) + commit_flag.reduce(_ || _).asUInt
    }
  }



  /*for (i <- 0 until exuParameters.LduCnt) {
    rf2D.io.wr_en(i) := wr_en(i)
    rf2D.io.waddr(i) := waddr_w(i)
    rf2D.io.wdata(i) := wdata_w(i)
    rf2D.io.woffset(i) := woffset_w(i)
  }*/


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