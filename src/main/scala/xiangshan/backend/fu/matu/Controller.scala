package xiangshan.backend.fu.matu

import chisel3._
import chisel3.util._


class GlobalCounter(val maxCount:Int) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val tick = Output(Bool())
  })

  val count = RegInit(0.U(log2Ceil(maxCount+1).W))
  val tick = count === maxCount.asUInt

  val enable = RegInit(false.B)
  when(io.start) {
    enable := true.B
  }.elsewhen(tick) {
    enable := false.B
  }

  when(enable) {
    when(count =/= maxCount.asUInt) {
      count := count + 1.U
    }.otherwise {
      count := 0.U
    }
  }

  io.tick := tick
}

class Controller(val SA_ROWS: Int, val SA_COLS: Int) extends Module {
  val io = IO(new Bundle {
    val ibh_data_in_done   = Input(Bool())
    val ibv_data_in_done     = Input(Bool())
    val ob_empty    = Input(Bool())

    val ctrl_ib_data_out = Output(Bool())
    val ctrl_ob_data_in    = Output(Bool())
    val ctrl_sa_isIdle = Output(Bool())
    val ctrl_sa_isStall = Output(Bool())
    val ctrl_sa_send_data= Output(Bool())
  })

  val ctrl_ib_data_out = WireInit(false.B)
  val delay_ctrl_ib_data_out = RegInit(false.B)
  val ctrl_ib_data_out_edge = WireInit(false.B)

  delay_ctrl_ib_data_out := ctrl_ib_data_out
  ctrl_ib_data_out_edge := !delay_ctrl_ib_data_out & ctrl_ib_data_out
  io.ctrl_ib_data_out := ctrl_ib_data_out_edge

  val cal_done = RegInit(false.B)
  val fall_done = RegInit(false.B)
  val cal_gc_start = WireInit(false.B)

  val isIdle = RegInit(true.B)
  io.ctrl_sa_isIdle := isIdle

  val isStall = RegInit(false.B)
  io.ctrl_sa_isStall := isStall

  // generate cal_done, meaning that calculation is done
  val cal_gc       = Module(new GlobalCounter(3*SA_ROWS-2)) // todo ROWs和COLs取最大值
  cal_gc.io.start := cal_gc_start
  when(cal_gc.io.tick) {
    cal_done := true.B
  }

  // generate fall_done, meaning that data enter to output buffer done
  val fall_gc       = Module(new GlobalCounter(SA_COLS-1))
  fall_gc.io.start := cal_gc.io.tick & io.ob_empty | isStall & io.ob_empty
  when(fall_gc.io.tick) {
    fall_done := true.B
  }

  io.ctrl_ob_data_in := cal_done & !fall_done & !isStall
  io.ctrl_sa_send_data := cal_done & !fall_done

  // idle：wait for instruction and prepare data
  // compute：execute matrix multiplication
  // fall: data enter to output buffer
  // To distinguish it from the OutputBuffer's output,
  // I use the term "fall" to represent the data output from the systolic array to the OutputBuffer,
  // because it looks like data falling out of the systolic array
  val idle :: compute :: stall :: fall :: Nil = Enum(4)
  val state = RegInit(idle)


  when(state === idle) {
    when(io.ibh_data_in_done && io.ibv_data_in_done && !isStall) {
      state := compute
      isIdle := false.B
      isStall := false.B
      ctrl_ib_data_out := true.B
      cal_gc_start := true.B
    }
  }.elsewhen(state === compute) {
    when(cal_done && io.ob_empty) {
      state := fall
    }.elsewhen(cal_gc.io.tick && !io.ob_empty) {
      state := stall
      isStall := true.B
    }
  }.elsewhen(state === stall) {
    when(io.ob_empty) {
      state := fall
      isStall := false.B
    }
  }.elsewhen(state === fall) {
    when(fall_done) {
      state := idle
      isIdle := true.B
      cal_done := false.B
      fall_done := false.B
    }
  }

}