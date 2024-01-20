package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan._
import xiangshan.backend.fu._

//class ldIO (implicit p: Parameters) extends XSBundle {
//
//    val ldIn = Vec(exuParameters.LduCnt, Flipped(DecoupledIO(new ExuOutput)))
//
//}


class Matu(implicit p: Parameters) extends FunctionUnit(64, MatuExeUnitCfg){
    val dataModule = Module(new XS_miniTPU_R)
    val inner_io = IO(new Bundle {
        val data = Output(UInt(XLEN.W))
    })

    val newReq = io.in.fire()
    val uop = io.in.bits.uop
    val uopReg = RegEnable(uop, newReq)
    //val load_in = IO(new ldIO)

    val load_in_r = dontTouch(RegInit(0.U(64.W)))
    inner_io.data := load_in_r + 3.U
    //load_in_r := load_in.ldIn(0).bits.data
    load_in_r := io.ldIn.get(0).bits.data
    io.ldIn.get(0).ready := true.B
    io.ldIn.get(1).ready := true.B
//    load_in.ldIn(0).ready := true.B
//    load_in.ldIn(1).ready := true.B

    dataModule.io.xsIO.in.bits.src := io.in.bits.src.take(2)
    dataModule.io.xsIO.in.bits.OpType := uopReg.ctrl.fuOpType

    io.in.ready := dataModule.io.xsIO.in.ready
    dataModule.io.xsIO.in.valid := io.in.valid
    dataModule.io.xsIO.out.ready := io.out.ready
    io.out.valid := dataModule.io.xsIO.out.valid
    io.out.bits.uop := uopReg
    io.out.bits.data := dataModule.io.xsIO.out.bits.data
}

class xsFUInput_R(implicit p: Parameters) extends XSBundle {
    val src = Input(Vec(2, UInt(XLEN.W)))
    val OpType = Input(FuOpType())
}

class xsFUOutput_R(implicit p: Parameters) extends XSBundle {
    val data = Output(UInt(XLEN.W))
}

class xsFUIO_R(implicit p: Parameters) extends XSBundle {

    val in = Flipped(DecoupledIO(new xsFUInput_R))

    val out = DecoupledIO(new xsFUOutput_R)
}

class XS_miniTPU_R(implicit p: Parameters) extends XSModule{
    val io = IO(new Bundle {
        val xsIO = new xsFUIO_R()
    })

    val inBridge  = Module(new InputBridge_R())
    val mini_tpu  = Module(new top_R(4,16,2,2))
    val outBridge = Module(new OutputBridge_R())

    inBridge.io.in_valid  := io.xsIO.in.valid
    io.xsIO.in.ready      := inBridge.io.out_ready
    inBridge.io.src(0) := io.xsIO.in.bits.src(0).asSInt
    inBridge.io.src(1) := io.xsIO.in.bits.src(1).asSInt

    inBridge.io.in_ready  := mini_tpu.io.tpuIO.in.ready
    mini_tpu.io.tpuIO.in.valid := inBridge.io.out_valid
    mini_tpu.io.tpuIO.in.bits.in_a := inBridge.io.in_a
    mini_tpu.io.tpuIO.in.bits.in_b := inBridge.io.in_b
    mini_tpu.io.tpuIO.in.bits.in_c := Seq.fill(2)(0.S(16.W))

    outBridge.io.in_valid := mini_tpu.io.tpuIO.out.valid
    mini_tpu.io.tpuIO.out.ready := outBridge.io.out_ready
    outBridge.io.out_c := mini_tpu.io.tpuIO.out.bits.out_c

    outBridge.io.in_ready := io.xsIO.out.ready
    io.xsIO.out.valid     := outBridge.io.out_valid
    io.xsIO.out.bits.data := Mux(io.xsIO.in.bits.OpType === MATUOpType.mmul,  outBridge.io.result.asUInt, 0.U(XLEN.W))

}

class OutputBridge_R() extends Module {
    val io = IO(new Bundle {
        val in_valid  = Input(Bool())
        val out_ready = Output(Bool())
        val out_c     = Input(Vec(2, SInt(16.W)))
        val in_ready  = Input(Bool())
        val out_valid = Output(Bool())
        val result    = Output(SInt(64.W))
    })

    val out_ptr = RegInit(false.B)
    val out_valid_r = RegInit(false.B)
    val result = Seq.fill(2, 2)(RegInit(0.S(16.W)))

    val indices = Seq((0, 0), (0, 1), (1, 0), (1, 1))

    when(out_ptr === false.B && io.in_valid) {
        out_ptr := true.B
        out_valid_r := false.B
        Seq(indices(2), indices(3)).zip(io.out_c).foreach { case ((i, j), out) => result(i)(j) := out }

    }.elsewhen(out_ptr === true.B && io.in_valid) {
        out_ptr := false.B
        out_valid_r := io.in_valid
        Seq(indices(0), indices(1)).zip(io.out_c).foreach { case ((i, j), out) => result(i)(j) := out }
    }.elsewhen(io.in_ready) {
        out_ptr := false.B
        out_valid_r := io.in_valid
        Seq(indices(0), indices(1), indices(2), indices(3)).foreach { case (i, j) => result(i)(j) := io.out_c(j) }
    }

    io.out_valid := out_valid_r
    io.out_ready := io.in_ready
    io.result := Cat(indices.map { case (i, j) => result(i)(j) }.reverse).asSInt
}

class InputBridge_R() extends Module {
    val io = IO(new Bundle {
        val in_valid  = Input(Bool())
        val out_ready = Output(Bool())
        val src       = Input(Vec(2, SInt(64.W)))
        val in_ready  = Input(Bool())
        val out_valid = Output(Bool())
        val in_a      = Output(Vec(2, SInt(4.W)))
        val in_b      = Output(Vec(2, SInt(4.W)))
    })

    val in_ptr = RegInit(false.B)
    val valid_r = RegInit(false.B)
    val srcChunks = Wire(Vec(2, SInt(16.W)))
    val in_a_r = Seq.fill(2, 2)(Reg(SInt(4.W)))
    val in_b_r = Seq.fill(2, 2)(Reg(SInt(4.W)))

    srcChunks := io.src.map { src => src(15, 0).asSInt }

    val indices = Seq((0, 0), (0, 1), (1, 0), (1, 1))

    when(in_ptr === false.B && io.in_ready && io.in_valid) {
        in_ptr := true.B
        valid_r := io.in_valid

        indices.foreach { case (i, j) => in_a_r(i)(j) := srcChunks(0)(i * 8 + j * 4 + 3, i * 8 + j * 4).asSInt }
        indices.foreach { case (i, j) => in_b_r(i)(j) := srcChunks(1)(i * 8 + j * 4 + 3, i * 8 + j * 4).asSInt }
        Seq(indices(0), indices(2)).zip(io.in_a).foreach { case ((i, j), in_a) => in_a := in_a_r(i)(j) }
        Seq(indices(0), indices(1)).zip(io.in_b).foreach { case ((i, j), in_b) => in_b := in_a_r(i)(j) }

    }.elsewhen(in_ptr === true.B && io.in_ready && valid_r) {
        in_ptr := false.B
        Seq(indices(0), indices(2)).zip(io.in_a).foreach { case ((i, j), in_a) => in_a := in_a_r(i)(j) }
        Seq(indices(0), indices(1)).zip(io.in_b).foreach { case ((i, j), in_b) => in_b := in_b_r(i)(j) }

    }.otherwise {
        in_ptr := false.B
        valid_r := io.in_valid
        indices.foreach { case (i, j) => in_a_r(i)(j) := srcChunks(0)(i * 8 + j * 4 + 3, i * 8 + j * 4).asSInt }
        indices.foreach { case (i, j) => in_b_r(i)(j) := srcChunks(1)(i * 8 + j * 4 + 3, i * 8 + j * 4).asSInt }
        Seq(indices(1), indices(3)).zip(io.in_a).foreach { case ((i, j), in_a) => in_a := in_a_r(i)(j) }
        Seq(indices(2), indices(3)).zip(io.in_b).foreach { case ((i, j), in_b) => in_b := in_b_r(i)(j) }
    }

    io.out_ready := io.in_ready
    io.out_valid := valid_r
}

class miniTPUInput_R(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
    val in_a = Input(Vec(SA_ROWS, SInt(IN_WIDTH.W)))
    val in_b = Input(Vec(SA_COLS, SInt(IN_WIDTH.W)))
    val in_c = Input(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUOutput_R(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
    val out_c = Output(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUIO_R(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
    val in = Flipped(DecoupledIO(new miniTPUInput_R(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS)))
    val out = DecoupledIO(new miniTPUOutput_R(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS))
}

class top_R (val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Module {
    val io = IO(new Bundle {
        val tpuIO = new miniTPUIO_R(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS)
    })

    val sa = Module(new SystolicArray(IN_WIDTH,C_WIDTH,SA_ROWS,SA_COLS))
    val controller = Module(new Controller(SA_ROWS, SA_COLS))
    val inBuffer_h   = Module(new SAInputBuffer(IN_WIDTH, SA_ROWS, SA_COLS))  // horizontal and vertical data buffer
    val inBuffer_v  = Module(new SAInputBuffer(IN_WIDTH, SA_COLS, SA_ROWS)) // TODO: add control logic to select data( B or D)
    val outBuffer  = Module(new SAOutputBuffer(C_WIDTH, SA_COLS, SA_ROWS))

    inBuffer_h.io.data_in.valid := io.tpuIO.in.valid
    inBuffer_h.io.data_in.bits := io.tpuIO.in.bits.in_a
    inBuffer_h.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out
    inBuffer_v.io.data_in.valid := io.tpuIO.in.valid
    inBuffer_v.io.data_in.bits := io.tpuIO.in.bits.in_b
    inBuffer_v.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out
    io.tpuIO.in.ready := inBuffer_h.io.data_in.ready & inBuffer_v.io.data_in.ready

    io.tpuIO.out.valid := outBuffer.io.data_out.valid
    io.tpuIO.out.bits.out_c := outBuffer.io.data_out.bits
    outBuffer.io.data_out.ready := io.tpuIO.out.ready
    outBuffer.io.ctrl_ob_data_in := controller.io.ctrl_ob_data_in

    sa.io.in_a := inBuffer_h.io.data_out
    sa.io.in_b := inBuffer_v.io.data_out
    sa.io.in_c := io.tpuIO.in.bits.in_c  // TODO: preload in_c as bias
    outBuffer.io.data_in := sa.io.out_c
    sa.io.in_control.foreach(_.ctrl_sa_send_data := controller.io.ctrl_sa_send_data)

    controller.io.ibh_data_in_done := inBuffer_h.io.ib_data_in_done
    controller.io.ibv_data_in_done := inBuffer_v.io.ib_data_in_done
    controller.io.ob_empty := outBuffer.io.ob_empty
}