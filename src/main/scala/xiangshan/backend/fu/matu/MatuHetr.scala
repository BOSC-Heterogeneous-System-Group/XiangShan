package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan._

class lsuIO (implicit p: Parameters) extends XSBundle {

  val ldIn = Vec(exuParameters.LduCnt, Flipped(DecoupledIO(new ExuOutput)))

  val data = ValidIO(UInt(XLEN.W))
}

class robIO (implicit p: Parameters) extends XSBundle {
  val deqPtrVec_v = Vec(CommitWidth, Input(UInt(5.W)))
}

class MatuHetr(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val lsuIO = new lsuIO()
    val robIO = new robIO()
  })
  val dataModule = Module(new XS_miniTPU_M)

  dataModule.io.xsIO.in(0).bits.ldIn := io.lsuIO.ldIn(0).bits.data
  dataModule.io.xsIO.in(1).bits.ldIn := io.lsuIO.ldIn(1).bits.data
  dataModule.io.xsIO.in(0).bits.OpType := io.lsuIO.ldIn(0).bits.uop.ctrl.fuOpType
  dataModule.io.xsIO.in(1).bits.OpType := io.lsuIO.ldIn(1).bits.uop.ctrl.fuOpType
  dataModule.io.xsIO.in(0).bits.robIdx := io.lsuIO.ldIn(0).bits.uop.robIdx.value
  dataModule.io.xsIO.in(1).bits.robIdx := io.lsuIO.ldIn(1).bits.uop.robIdx.value
  dataModule.io.xsIO.in(0).valid := io.lsuIO.ldIn(0).valid
  dataModule.io.xsIO.in(1).valid := io.lsuIO.ldIn(1).valid
  dataModule.io.xsIO.deqptr := io.robIO.deqPtrVec_v

  io.lsuIO.ldIn(0).ready := true.B // TODO: Back Pressure
  io.lsuIO.ldIn(1).ready := true.B

  dataModule.io.xsIO.out.ready := false.B
  io.lsuIO.data.valid := dataModule.io.xsIO.out.valid
  io.lsuIO.data.bits := dataModule.io.xsIO.out.bits.data

}

class xsFUInput_M(implicit p: Parameters) extends XSBundle {
  val ldIn = Input(UInt(XLEN.W))
  val OpType = Input(FuOpType())
  val robIdx = Input(UInt(5.W))
}

class xsFUOutput_M(implicit p: Parameters) extends XSBundle {
  val data = Output(UInt(XLEN.W))
}

class xsFUIO_M (implicit p: Parameters) extends XSBundle {

  val in = Vec(2, Flipped(ValidIO(new xsFUInput_M)))
  val deqptr = Vec(CommitWidth, Input(UInt(5.W)))

  val out = DecoupledIO(new xsFUOutput_M())
}

class XS_miniTPU_M(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val xsIO = new xsFUIO_M()
  })

  val mini_tpu  = Module(new top_M(4,16,2,2))
  val outBridge = Module(new OutputBridge_M())
  val inBridge_m  = Module(new InputBridge_M())
  val rob = Module(new SA_ROB_Wrapper)

  rob.io.ldin.valid(0) := io.xsIO.in(0).valid
  rob.io.ldin.valid(1) := io.xsIO.in(1).valid
  rob.io.ldin.data(0) := io.xsIO.in(0).bits.ldIn
  rob.io.ldin.data(1) := io.xsIO.in(1).bits.ldIn
  rob.io.ldin.robIdx(0) := io.xsIO.in(0).bits.robIdx
  rob.io.ldin.robIdx(1) := io.xsIO.in(1).bits.robIdx
  rob.io.ldin.fuOptype(0) := io.xsIO.in(0).bits.OpType
  rob.io.ldin.fuOptype(1) := io.xsIO.in(1).bits.OpType
  rob.io.deqptr := io.xsIO.deqptr

  inBridge_m.io.in_valid := rob.io.out.valid
  rob.io.out.ready := inBridge_m.io.out_ready
  inBridge_m.io.ldIn := rob.io.out.bits.asSInt

  inBridge_m.io.in_ready := mini_tpu.io.tpuIO.in.in_a.ready | mini_tpu.io.tpuIO.in.in_b.ready
  mini_tpu.io.tpuIO.in.in_a.valid := inBridge_m.io.out_valid_h
  mini_tpu.io.tpuIO.in.in_b.valid := inBridge_m.io.out_valid_v
  mini_tpu.io.tpuIO.in.in_a.bits := inBridge_m.io.in_a
  mini_tpu.io.tpuIO.in.in_b.bits := inBridge_m.io.in_b
  mini_tpu.io.tpuIO.in.in_c := Seq.fill(2)(0.S(16.W))

  outBridge.io.in_valid := mini_tpu.io.tpuIO.out.valid
  mini_tpu.io.tpuIO.out.ready := outBridge.io.out_ready
  outBridge.io.out_c := mini_tpu.io.tpuIO.out.bits.out_c

  outBridge.io.in_ready := io.xsIO.out.ready
  io.xsIO.out.valid := outBridge.io.out_valid
  io.xsIO.out.bits.data := outBridge.io.result.asUInt

}

class OutputBridge_M() extends Module {
  val io = IO(new Bundle {
    val in_valid  = Input(Bool())
    val out_ready = Output(Bool())
    val out_c     = Input(Vec(2, SInt(16.W)))
    val in_ready  = Input(Bool())
    val out_valid = Output(Bool())
    val result    = Output(SInt(64.W))
  })

  val out_ptr      = RegInit(false.B)
  val out_valid_r  = RegInit(false.B)
  val result       = Seq.fill(2, 2)(RegInit(0.S(16.W)))

  val indices = Seq((0, 0), (0, 1), (1, 0), (1, 1))

  when (out_ptr === false.B && io.in_valid) {
    out_ptr         := true.B
    out_valid_r     := false.B
    Seq(indices(2), indices(3)).zip(io.out_c).foreach {case ((i, j), out) => result(i)(j) := out}

  }.elsewhen (out_ptr === true.B && io.in_valid){
    out_ptr         := false.B
    out_valid_r     := io.in_valid
    Seq(indices(0), indices(1)).zip(io.out_c).foreach {case ((i, j), out) => result(i)(j) := out}
  }.elsewhen (io.in_ready) {
    out_ptr         := false.B
    out_valid_r     := io.in_valid
    Seq(indices(0), indices(1), indices(2), indices(3)).foreach {case (i, j) => result(i)(j) := io.out_c(j)}
  }

  io.out_valid := out_valid_r
  io.out_ready := io.in_ready
  io.result    := Cat(indices.map {case (i, j) => result(i)(j)}.reverse).asSInt

}

class ROBIn(implicit p: Parameters) extends XSBundle {
  val data = Vec(2, Input(UInt(XLEN.W)))
  val robIdx = Vec(2, Input(UInt(5.W)))
  val fuOptype = Vec(2, Input(FuOpType()))
  val valid = Vec(2, Input(Bool()))
}

class SA_ROB_Wrapper(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val ldin = new ROBIn()
    val deqptr = Vec(CommitWidth, Input(UInt(5.W)))
    val out = DecoupledIO(UInt(XLEN.W))
  })
  val ldBuff = Module(new ldBuffer())
  val rob = Module(new SA_ROB())

  ldBuff.io.ldin := io.ldin
  ldBuff.io.deqptr := io.deqptr

  rob.io.MatchVec := ldBuff.io.bufout.MatchVec
  rob.io.data := ldBuff.io.bufout.data


  io.out.bits := rob.io.out.bits
  io.out.valid := rob.io.out.valid
  rob.io.out.ready := io.out.ready



}


class BufIn (implicit p: Parameters) extends XSBundle {
  val data = Vec(2, Input(UInt(XLEN.W)))
  val robIdx = Vec(2, Input(UInt(5.W)))
  val fuOptype = Vec(2, Input(FuOpType()))
  val valid = Vec(2, Input(Bool()))
}

class BufOut (implicit p: Parameters) extends XSBundle {
  val data = Vec(4, Output(UInt(XLEN.W)))
  val MatchVec = Vec(4, Output(UInt(CommitWidth.W)))
  val valid = Vec(4, Output(Bool()))
}

class ldBuffer(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val deqptr = Vec(CommitWidth, Input(UInt(5.W)))
    val ldin = new BufIn()
    val bufout  = new BufOut()
    // val ready = Input(Bool())
  })

  val MatchVec = Wire(Vec(4, Vec(CommitWidth, Bool())))
  val matchVecUInt = WireInit(VecInit(MatchVec.map(_.asUInt())))
  val validReg = RegInit(VecInit(Seq.fill(4)(false.B)))
  val dataReg = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
  val deqptrReg = Reg(Vec(CommitWidth, UInt(5.W)))
  val robIdxReg = Reg(Vec(4, UInt(5.W)))


  validReg(0) := (io.ldin.fuOptype(0) === LSUOpType.mlb) && (io.ldin.valid(0) === true.B)
  validReg(1) := (io.ldin.fuOptype(1) === LSUOpType.mlb) && (io.ldin.valid(1) === true.B)
  validReg(2) := Mux(MatchVec(0).asUInt().orR(), false.B, validReg(0)) | Mux(MatchVec(2).asUInt().orR(), false.B, validReg(2))
  validReg(3) := Mux(MatchVec(1).asUInt().orR(), false.B, validReg(1)) | Mux(MatchVec(3).asUInt().orR(), false.B, validReg(3))
  dataReg(0) := Mux(io.ldin.valid(0) && io.ldin.fuOptype(0) === LSUOpType.mlb, io.ldin.data(0), dataReg(0))
  dataReg(1) := Mux(io.ldin.valid(1) && io.ldin.fuOptype(1) === LSUOpType.mlb, io.ldin.data(1), dataReg(1))
  dataReg(2) := dataReg(0)
  dataReg(3) := dataReg(1)
  robIdxReg(0) := Mux(io.ldin.valid(0) && io.ldin.fuOptype(0) === LSUOpType.mlb, io.ldin.robIdx(0), robIdxReg(0))
  robIdxReg(1) := Mux(io.ldin.valid(1) && io.ldin.fuOptype(1) === LSUOpType.mlb, io.ldin.robIdx(1), robIdxReg(1))
  robIdxReg(2) := robIdxReg(0)
  robIdxReg(3) := robIdxReg(1)
  deqptrReg := io.deqptr

  for (i <- 0 until 6) {
    MatchVec(0)(i) := validReg(0) && (deqptrReg(i) === robIdxReg(0)).asBool
    MatchVec(1)(i) := validReg(1) && (deqptrReg(i) === robIdxReg(1)).asBool
    MatchVec(2)(i) := validReg(2) && (deqptrReg(i) === robIdxReg(2)).asBool
    MatchVec(3)(i) := validReg(3) && (deqptrReg(i) === robIdxReg(3)).asBool
  }
  io.bufout.data := dataReg
  io.bufout.valid := validReg
  io.bufout.MatchVec := matchVecUInt

}

class SA_ROB() extends Module{
  val io = IO(new Bundle(){
    val MatchVec = Vec(4, Input(UInt(6.W)))
    val data = Vec(4, Input(UInt(64.W)))
    val out = DecoupledIO(UInt(64.W))
  })
  val isEmpty = WireInit(false.B)
  val deq = WireInit(false.B)
  val readPtr = RegInit(0.U(3.W))
  val writePtr = RegInit(0.U(3.W))
  val writePtrP1= WireInit(writePtr+1.U(3.W))
  val writePtrP2= WireInit(writePtr+2.U(3.W))
  val writePtrP3= WireInit(writePtr+3.U(3.W))
  val mem = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
  val matchVecORR = Cat(io.MatchVec(3).orR, io.MatchVec(2).orR, io.MatchVec(1).orR, io.MatchVec(0).orR)

  isEmpty := readPtr === writePtr
  deq := !isEmpty & io.out.ready
  val deqData = WireInit(0.U(64.W))

  val flag = WireInit(false.B)
  flag := (io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) > 0.U) && (io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3))

  when(deq) {
    readPtr := readPtr + 1.U
    deqData := mem(readPtr)
  }

  io.out.bits := deqData
  io.out.valid := !isEmpty

  writePtr := MuxCase(writePtr,
    Array(
      (PopCount(matchVecORR) === 4.U) -> (writePtr + 4.U),
      (PopCount(matchVecORR) === 3.U) -> (writePtr + 3.U),
      (PopCount(matchVecORR) === 2.U) -> (writePtr + 2.U),
      (PopCount(matchVecORR) === 1.U) -> (writePtr + 1.U),
      (PopCount(matchVecORR) === 0.U) -> (writePtr + 0.U)
    )
  )
  // This code is to express my dissatisfaction with the way the group operates
  when(PopCount(matchVecORR) === 1.U) {
    when(io.MatchVec(0) > 0.U) {
      mem(writePtr) := io.data(0)
    }.elsewhen(io.MatchVec(1)>0.U) {
      mem(writePtr) := io.data(1)
    }.elsewhen(io.MatchVec(2)>0.U) {
      mem(writePtr) := io.data(2)
    }.elsewhen(io.MatchVec(3)>0.U){
      mem(writePtr) := io.data(3)
    }
  }.elsewhen(PopCount(matchVecORR) === 2.U){
    when((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1)> 0.U).asBool && (io.MatchVec(0) < io.MatchVec(1))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(1)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) > 0.U).asBool && (io.MatchVec(0) > io.MatchVec(1))){
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2)> 0.U).asBool && (io.MatchVec(0) < io.MatchVec(2))){
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(2)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2)> 0.U).asBool && (io.MatchVec(0) > io.MatchVec(2))){
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(3)> 0.U).asBool && (io.MatchVec(0) < io.MatchVec(3))){
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(3)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(3)> 0.U).asBool && (io.MatchVec(0) > io.MatchVec(3))){
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(0)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2)> 0.U).asBool && (io.MatchVec(1) < io.MatchVec(2))){
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(2)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2)> 0.U).asBool && (io.MatchVec(1) > io.MatchVec(2))){
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(1)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(3)> 0.U).asBool && (io.MatchVec(1) < io.MatchVec(3))){
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(3)
    }.elsewhen((io.MatchVec(2) > 0.U).asBool && (io.MatchVec(3) > 0.U).asBool && (io.MatchVec(1) > io.MatchVec(3))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(1)
    }.elsewhen((io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) > 0.U).asBool && (io.MatchVec(2) < io.MatchVec(3))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(3)
    }.elsewhen((io.MatchVec(2) > 0.U).asBool && (io.MatchVec(3) > 0.U).asBool && (io.MatchVec(3) < io.MatchVec(2))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(2)
    }
  }.elsewhen(PopCount(matchVecORR) === 3.U) {
    when((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2) > 0.U).asBool && (io.MatchVec(3) > 0.U).asBool && (io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(3)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(1) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(2))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(2)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(2) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(3))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(3)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(2) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(1))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(1)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(3) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(1))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(1)
    }.elsewhen((io.MatchVec(1) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(3) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(2)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(0) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(3)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(0) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(2))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(2)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(2) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(3)))  {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(3)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(2) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(0))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(3) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(0))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(3) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(2))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(2)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(3))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(3)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(0) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(1))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(1)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(1) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(3))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(3)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(1) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(0))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(3) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(0))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(3) >0.U).asBool && (io.MatchVec(3) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(1))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(1)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(2)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(0) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(1))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(1)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(1) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(2))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(2)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(0))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(2) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(0))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(0)
    }.elsewhen((io.MatchVec(0) > 0.U).asBool && (io.MatchVec(1) >0.U).asBool && (io.MatchVec(2) >0.U).asBool && (io.MatchVec(2) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(1))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(1)
    }
  }.elsewhen(PopCount(matchVecORR) === 4.U) {
    when((io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3))){
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(2)
      mem(writePtrP3) := io.data(3)
    }.elsewhen((io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(2))){
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(3)
      mem(writePtrP3) := io.data(2)
    }.elsewhen((io.MatchVec(0) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(3))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(1)
      mem(writePtrP3) := io.data(3)
    }.elsewhen((io.MatchVec(0) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(1))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(3)
      mem(writePtrP3) := io.data(1)
    }.elsewhen((io.MatchVec(0) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(1)
      mem(writePtrP3) := io.data(2)
    }.elsewhen((io.MatchVec(0) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(1))) {
      mem(writePtr) := io.data(0)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(2)
      mem(writePtrP3) := io.data(1)
    }.elsewhen((io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(0))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(3)
      mem(writePtrP3) := io.data(0)
    }.elsewhen((io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(3))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(0)
      mem(writePtrP3) := io.data(3)
    }.elsewhen((io.MatchVec(1) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(2))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(0)
      mem(writePtrP3) := io.data(2)
    }.elsewhen((io.MatchVec(1) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(0))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(2)
      mem(writePtrP3) := io.data(0)
    }.elsewhen((io.MatchVec(1) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(3))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(2)
      mem(writePtrP3) := io.data(3)
    }.elsewhen((io.MatchVec(1) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(2))) {
      mem(writePtr) := io.data(1)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(3)
      mem(writePtrP3) := io.data(2)
    }.elsewhen((io.MatchVec(2) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(3))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(1)
      mem(writePtrP3) := io.data(3)
    }.elsewhen((io.MatchVec(2) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(1))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(3)
      mem(writePtrP3) := io.data(1)
    }.elsewhen((io.MatchVec(2) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(3))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(0)
      mem(writePtrP3) := io.data(3)
    }.elsewhen((io.MatchVec(2) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(0))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(3)
      mem(writePtrP3) := io.data(0)
    }.elsewhen((io.MatchVec(2) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(0))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(1)
      mem(writePtrP3) := io.data(0)
    }.elsewhen((io.MatchVec(2) < io.MatchVec(3)) && (io.MatchVec(3) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(1))) {
      mem(writePtr) := io.data(2)
      mem(writePtrP1) := io.data(3)
      mem(writePtrP2) := io.data(0)
      mem(writePtrP3) := io.data(1)
    }.elsewhen((io.MatchVec(3) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(1))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(0)
      mem(writePtrP3) := io.data(1)
    }.elsewhen((io.MatchVec(3) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(0))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(2)
      mem(writePtrP2) := io.data(1)
      mem(writePtrP3) := io.data(0)
    }.elsewhen((io.MatchVec(3) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(0))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(2)
      mem(writePtrP3) := io.data(0)
    }.elsewhen((io.MatchVec(3) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(2))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(1)
      mem(writePtrP2) := io.data(0)
      mem(writePtrP3) := io.data(2)
    }.elsewhen((io.MatchVec(3) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(1)) && (io.MatchVec(1) < io.MatchVec(2))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(1)
      mem(writePtrP3) := io.data(2)
    }.elsewhen((io.MatchVec(3) < io.MatchVec(0)) && (io.MatchVec(0) < io.MatchVec(2)) && (io.MatchVec(2) < io.MatchVec(1))) {
      mem(writePtr) := io.data(3)
      mem(writePtrP1) := io.data(0)
      mem(writePtrP2) := io.data(2)
      mem(writePtrP3) := io.data(1)
    }
  }
}

class InputBridge_M() extends Module {
  val io = IO(new Bundle() {
    val in_valid  = Input(Bool())
    val out_ready = Output(Bool())
    val ldIn      = Input(SInt(64.W))
    val in_ready  = Input(Bool())
    val out_valid_h = Output(Bool())
    val out_valid_v = Output(Bool())
    val in_a      = Output(Vec(2, SInt(4.W)))
    val in_b      = Output(Vec(2, SInt(4.W)))
  })

  val cnt     = RegInit(0.U(2.W))
  val in_ptr  = RegInit(false.B)
  when (in_ptr === false.B && cnt < 1.U && io.in_ready && io.in_valid ) {
    cnt := cnt + 1.U
    io.in_a(0) := io.ldIn(3, 0).asSInt
    io.in_a(1) := io.ldIn(7, 4).asSInt
    io.out_valid_h := io.in_valid
    io.out_valid_v := false.B
    io.in_b(0) := 0.S
    io.in_b(1) := 0.S
  }.elsewhen ( in_ptr === false.B && cnt === 1.U && io.in_ready && io.in_valid ) {
    in_ptr     := true.B
    cnt        := 0.U
    io.out_valid_h := io.in_valid
    io.out_valid_v := false.B
    io.in_a(0) := io.ldIn(3, 0).asSInt
    io.in_a(1) := io.ldIn(7, 4).asSInt
    io.in_b(0) := 0.S
    io.in_b(1) := 0.S
  }.elsewhen ( in_ptr === true.B && cnt < 1.U && io.in_ready && io.in_valid ) {
    cnt := cnt + 1.U
    io.in_a(0) := 0.S
    io.in_a(1) := 0.S
    io.out_valid_h := false.B
    io.out_valid_v := io.in_valid
    io.in_b(0) := io.ldIn(3, 0).asSInt
    io.in_b(1) := io.ldIn(7, 4).asSInt
  }.elsewhen ( in_ptr === true.B && cnt === 1.U && io.in_ready && io.in_valid ) {
    in_ptr     := false.B
    cnt        := 0.U
    io.in_a(0) := 0.S
    io.in_a(1) := 0.S
    io.out_valid_h := false.B
    io.out_valid_v := io.in_valid
    io.in_b(0) := io.ldIn(3, 0).asSInt
    io.in_b(1) := io.ldIn(7, 4).asSInt
  }.otherwise {
    io.out_valid_h := io.in_valid
    io.out_valid_v := io.in_valid
    io.in_a(0) := 0.S
    io.in_a(1) := 0.S
    io.in_b(0) := 0.S
    io.in_b(1) := 0.S
  }
  io.out_ready := io.in_ready
}

class miniTPUInput_M(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
  val in_a = Flipped(DecoupledIO(Vec(SA_ROWS, SInt(IN_WIDTH.W))))
  val in_b = Flipped(DecoupledIO(Vec(SA_COLS, SInt(IN_WIDTH.W))))
  val in_c = Input(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUOutput_M(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
  val out_c = Output(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUIO_M (val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
  val in = new miniTPUInput_M(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS)
  val out = DecoupledIO(new miniTPUOutput_M(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS))
}

/*
   type-0 : pipeline systolic array
   type-1 : algorithm accelerator
   ...
*/
class top_M (val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Module {
  val io = IO(new Bundle {
    val tpuIO = new miniTPUIO_M(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS)
  })

  val sa = Module(new SystolicArray(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS))
  val controller = Module(new Controller(SA_ROWS, SA_COLS))
  val inBuffer_h = Module(new SAInputBuffer(IN_WIDTH, SA_ROWS, SA_COLS)) // horizontal and vertical data buffer
  val inBuffer_v = Module(new SAInputBuffer(IN_WIDTH, SA_COLS, SA_ROWS)) // TODO: add control logic to select data( B or D)
  val outBuffer = Module(new SAOutputBuffer(C_WIDTH, SA_COLS, SA_ROWS))

  inBuffer_h.io.data_in.valid := io.tpuIO.in.in_a.valid
  inBuffer_h.io.data_in.bits := io.tpuIO.in.in_a.bits
  inBuffer_h.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out

  inBuffer_v.io.data_in.valid := io.tpuIO.in.in_b.valid
  inBuffer_v.io.data_in.bits := io.tpuIO.in.in_b.bits
  inBuffer_v.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out
  io.tpuIO.in.in_a.ready := inBuffer_h.io.data_in.ready
  io.tpuIO.in.in_b.ready := inBuffer_v.io.data_in.ready

  io.tpuIO.out.valid := outBuffer.io.data_out.valid
  io.tpuIO.out.bits.out_c := outBuffer.io.data_out.bits
  outBuffer.io.data_out.ready := io.tpuIO.out.ready
  outBuffer.io.ctrl_ob_data_in := controller.io.ctrl_ob_data_in

  sa.io.in_a := inBuffer_h.io.data_out
  sa.io.in_b := inBuffer_v.io.data_out
  sa.io.in_c := io.tpuIO.in.in_c // TODO: preload in_c as bias
  outBuffer.io.data_in := sa.io.out_c
  sa.io.in_control.foreach(_.ctrl_sa_send_data := controller.io.ctrl_sa_send_data)

  controller.io.ibh_data_in_done := inBuffer_h.io.ib_data_in_done
  controller.io.ibv_data_in_done := inBuffer_v.io.ib_data_in_done
  controller.io.ob_empty := outBuffer.io.ob_empty
}