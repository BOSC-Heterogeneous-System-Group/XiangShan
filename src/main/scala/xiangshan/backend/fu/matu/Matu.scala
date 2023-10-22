package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.fu._

class MaddModule(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val madd = Output(UInt(XLEN.W))
  })
    val maddElements = Wire(Vec(16, UInt(4.W)))
    for (i <- 0 until 16) {
        maddElements(i) := io.src(0)(4*i+3, 4*i) + io.src(1)(4*i+3, 4*i)
    }
    io.madd := maddElements.asUInt()

}

class MatuDataModule(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle(){
        val src = Vec(2, Input(UInt(XLEN.W)))
        val OpType = Input(FuOpType())
        val result = Output(UInt(XLEN.W))
    })
    val (src1, src2) = (io.src(0), io.src(1))

    // madd
    val maddModule = Module(new MaddModule)
    maddModule.io.src(0) := src1
    maddModule.io.src(1) := src2

    io.result := Mux(io.OpType === MATUOpType.madd, maddModule.io.madd, 0.U(XLEN.W))

}

class Matu(implicit p: Parameters) extends FunctionUnit{
    val dataModule = Module(new MatuDataModule)

    dataModule.io.src := io.in.bits.src.take(2)
    dataModule.io.OpType := io.in.bits.uop.ctrl.fuOpType

    io.in.ready := io.out.ready
    io.out.valid := io.in.ready
    io.out.bits.uop <> io.in.bits.uop
    io.out.bits.data := dataModule.io.result
}