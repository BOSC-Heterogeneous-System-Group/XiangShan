package matu.DataBuffer

import chisel3._
import chisel3.util._


// TODO: fusion vertical InputBuffer and OutputBuffer as one module?
class SAOutputBuffer(val C_WIDTH: Int, val QUEUE_NUM: Int, val QUEUE_LEN: Int) extends Module {
  val io = IO(new Bundle {
    val ctrl_ob_data_in = Input(Bool())
    val data_in = Input(Vec(QUEUE_NUM, SInt(C_WIDTH.W)))

    val data_out = DecoupledIO(Vec(QUEUE_NUM, SInt(C_WIDTH.W)))
    val ob_empty  = Output(Bool())
  })

  val data_queue = Seq.fill(QUEUE_NUM)(Module(new SyncFIFO(C_WIDTH, QUEUE_LEN)))

  val allEmpty = WireInit(false.B)
  allEmpty := data_queue.map(_.io.empty).reduce(_ & _)

  for (i <- 0 until QUEUE_NUM) {
    data_queue(i).io.enq := io.ctrl_ob_data_in
    data_queue(i).io.deq := io.data_out.ready && !allEmpty
    data_queue(i).io.enqData := io.data_in(i)
    io.data_out.bits(i) := data_queue(i).io.deqData
  }

  io.data_out.valid := !allEmpty
  io.ob_empty := allEmpty
}
