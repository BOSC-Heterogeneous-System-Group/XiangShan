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

class fu_io(implicit p: Parameters) extends XSBundle {
  val instr_out = Output(UInt(32.W))
  val valid_out = Output(Bool())
  val rs1_out = Output(UInt(3.W))
  val rs2_out = Output(UInt(3.W))
  val rd_out = Output(UInt(3.W))
  val ready_in = Input(Bool())
}

class commits_scb_io(implicit p: Parameters) extends XSBundle {
  val commits_pc = Input(Vec(CommitWidth,  UInt(VAddrBits.W)))
  val commits_valid = Input(Vec(CommitWidth, Bool()))
  val commitPC = Input(Vec(exuParameters.LduCnt, UInt(VAddrBits.W)))
  val waw = Output(Bool())
  val war = Output(Bool())
}

class writeback_in(implicit p: Parameters) extends XSBundle {
  val wen = Input(Vec(3, Bool()))
  val waddr = Input(Vec(3, UInt(3.W)))
  val woffset = Input(Vec(3, UInt(2.W)))
}

class Scoreboard (implicit  p: Parameters) extends XSModule with HasXSParameter {
  val io = IO(new Bundle {
    val dpIn = new dispatch_in()
    val fuIO = new fu_io()
    val commitsIO = new commits_scb_io()
    val wbIn= new writeback_in()
  })

  val s_idle :: s_wait :: s_commit :: s_retire :: s_unready :: s_ready :: Nil = Enum(6) // ins state


  val OpType_w = Wire(Vec(2*dpParams.IntDqDeqWidth, FuOpType()))
  val instr_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(32.W)))
  val rs1_w = Wire(Vec(2 * dpParams.IntDqDeqWidth, UInt(3.W)))
  val rs2_w = Wire(Vec(2 * dpParams.IntDqDeqWidth, UInt(3.W)))
  val rd_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(3.W)))
  val offset_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(2.W)))
  val pc_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(VAddrBits.W)))
  val robIdx_w = Wire(Vec(2*dpParams.IntDqDeqWidth, UInt(5.W)))
  val dp_valid_w = Wire(Vec(2*dpParams.IntDqDeqWidth, Bool()))


  for(i <- 0 until 2 * dpParams.IntDqDeqWidth) {
    OpType_w(i) := io.dpIn.uop_in(i).ctrl.fuOpType
    instr_w(i) := io.dpIn.uop_in(i).cf.instr
    rs1_w(i) := io.dpIn.uop_in(i).cf.instr(17, 15)
    rs2_w(i) := io.dpIn.uop_in(i).cf.instr(22, 20)
    rd_w(i) := io.dpIn.uop_in(i).cf.instr(9, 7)
    offset_w(i) := io.dpIn.uop_in(i).cf.instr(11, 10)
    pc_w(i) := io.dpIn.uop_in(i).cf.pc
    robIdx_w(i) := io.dpIn.uop_in(i).robIdx.value
    dp_valid_w(i) := io.dpIn.valid_in(i) && io.dpIn.uop_in(i).cf.instr(6, 0) === "b0101011".U
  }

  /** reorder
   * the instrs that enter the array through 8 channels are out of order, need to be first reordered
   */

  val OpType_sel = WireInit(VecInit(Seq.fill(2)(0.U(7.W))))
  val instr_sel = WireInit(VecInit(Seq.fill(2)(0.U(32.W))))
  val rs1_sel = WireInit(VecInit(Seq.fill(2)(0.U(3.W))))
  val rs2_sel = WireInit(VecInit(Seq.fill(2)(0.U(3.W))))
  val rd_sel = WireInit(VecInit(Seq.fill(2)(0.U(3.W))))
  val offset_sel = WireInit(VecInit(Seq.fill(2)(0.U(2.W))))
  val pc_sel = WireInit(VecInit(Seq.fill(2)(0.U(VAddrBits.W))))
  val robIdx_sel = WireInit(VecInit(Seq.fill(2)(0.U(5.W))))
  val valid_sel = WireInit(VecInit(Seq.fill(2)(false.B)))

  val OpType_order = WireInit(VecInit(Seq.fill(2)(0.U(7.W))))
  val instr_order = WireInit(VecInit(Seq.fill(2)(0.U(32.W))))
  val rs1_order = WireInit(VecInit(Seq.fill(2)(0.U(3.W))))
  val rs2_order = WireInit(VecInit(Seq.fill(2)(0.U(3.W))))
  val rd_order = WireInit(VecInit(Seq.fill(2)(0.U(3.W))))
  val offset_order = WireInit(VecInit(Seq.fill(2)(0.U(2.W))))
  val pc_order = WireInit(VecInit(Seq.fill(2)(0.U(VAddrBits.W))))
  val robIdx_order = WireInit(VecInit(Seq.fill(2)(0.U(5.W))))
  val valid_order = WireInit(VecInit(Seq.fill(2)(false.B)))

  OpType_sel(0) := OpType_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  instr_sel(0) := instr_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  rs1_sel(0) := rs1_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  rs2_sel(0) := rs2_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  rd_sel(0) := rd_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  offset_sel(0) := offset_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  pc_sel(0) := pc_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  robIdx_sel(0) := robIdx_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))
  valid_sel(0) := dp_valid_w(PriorityEncoder(dp_valid_w.asUInt(3, 0)))

  OpType_sel(1) := OpType_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  instr_sel(1) := instr_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  rs1_sel(1) := rs1_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  rs2_sel(1) := rs2_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  rd_sel(1) := rd_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  offset_sel(1) := offset_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  pc_sel(1) := pc_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  robIdx_sel(1) := robIdx_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)
  valid_sel(1) := dp_valid_w(PriorityEncoder(dp_valid_w.asUInt(7, 4))+4.U)


  val cmp = robIdx_sel(0) > robIdx_sel(1)
  OpType_order(1) := Mux(cmp, OpType_sel(0), OpType_sel(1))
  OpType_order(0) := Mux(cmp, OpType_sel(1), OpType_sel(0))
  instr_order(1) := Mux(cmp, instr_sel(0), instr_sel(1))
  instr_order(0) := Mux(cmp, instr_sel(1), instr_sel(0))
  rs1_order(1) := Mux(cmp, rs1_sel(0), rs1_sel(1))
  rs1_order(0) := Mux(cmp, rs1_sel(1), rs1_sel(0))
  rs2_order(1) := Mux(cmp, rs2_sel(0), rs2_sel(1))
  rs2_order(0) := Mux(cmp, rs2_sel(1), rs2_sel(0))
  rd_order(1) := Mux(cmp, rd_sel(0), rd_sel(1))
  rd_order(0) := Mux(cmp, rd_sel(1), rd_sel(0))
  offset_order(1) := Mux(cmp, offset_sel(0), offset_sel(1))
  offset_order(0) := Mux(cmp, offset_sel(1), offset_sel(0))
  pc_order(1) := Mux(cmp, pc_sel(0), pc_sel(1))
  pc_order(0) := Mux(cmp, pc_sel(1), pc_sel(0))
  robIdx_order(1) := Mux(cmp, robIdx_sel(0), robIdx_sel(1))
  robIdx_order(0) := Mux(cmp, robIdx_sel(1), robIdx_sel(0))
  valid_order(1) := Mux(cmp, valid_sel(0), valid_sel(1))
  valid_order(0) := Mux(cmp, valid_sel(1), valid_sel(0))

  val state_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_idle))))
  val next_state_array = dontTouch(WireInit(state_array))
  val OpType_array = dontTouch(Reg(Vec(32, FuOpType())))
  val instr_array = dontTouch(Reg(Vec(32, UInt(32.W))))
  val rs1_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val rs1_ready_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_unready))))
  val rs2_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val rs2_ready_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_unready))))
  val rd_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val offset_array = dontTouch(Reg(Vec(32, UInt(2.W))))
  val pc_array = dontTouch(Reg(Vec(32, UInt(VAddrBits.W))))
  val robIdx_array = dontTouch(Reg(Vec(32, UInt(4.W))))

  for (i <- 0 until 32) {
    state_array(i) := next_state_array(i)
  }

  val writePtr = RegInit(0.U(6.W))
  val next_writePtr = Wire(UInt(6.W))
  val readPtr = RegInit(0.U(6.W))
  next_writePtr := writePtr + PopCount(valid_order)
  writePtr := next_writePtr

  val enq_offset = dontTouch(Wire(Vec(2, UInt(6.W))))
  val offset = (0 until 2).map(i => PopCount(valid_order.take(i)))
  enq_offset := offset

  /** enqueue
   * state: s_idle -> s_wait
   * when the type of a valid instr is Matrix-extension, the instr can enqueue
   * the corresponding state is set to s_wait
   * Entry type: 1. OpType  2. rd_addr  3. rd_offset  4. pc  5. robIdx  6. state
   */
  for(i <- 0 until 2) {
    when(state_array(writePtr(4, 0) + enq_offset(i)) === s_idle && valid_order(i) === true.B){
      OpType_array(writePtr(4, 0) + enq_offset(i)) := OpType_order(i)
      instr_array(writePtr(4, 0) + enq_offset(i)) := instr_order(i)
      rs1_array(writePtr(4, 0) + enq_offset(i)) := rs1_order(i)
      rs2_array(writePtr(4, 0) + enq_offset(i)) := rs2_order(i)
      rd_array(writePtr(4, 0) + enq_offset(i)) := rd_order(i)
      offset_array(writePtr(4, 0) + enq_offset(i)) := offset_order(i)
      pc_array(writePtr(4, 0) + enq_offset(i)) := pc_order(i)
      robIdx_array(writePtr(4, 0) + enq_offset(i)) := robIdx_order(i)
      next_state_array(writePtr(4, 0) + enq_offset(i)) := s_wait
    }
  }

  /**instr state switch
   * state: s_wait -> s_commit
   * when the instr is commited in rob, the state of instr is set to s_commit
   * state: s_commit -> s_retire
   * when the instr finally writeback data to regfile, the state of instr is set to s_retire
   */
  for (i <- 0 until 31) {
    val commit_flag = Seq.tabulate(CommitWidth)(j =>
      state_array(i) === s_wait && io.commitsIO.commits_valid(j) && io.commitsIO.commits_pc(j) === pc_array(i)
    )
    when(state_array(i) === s_wait) {
      next_state_array(i) := Mux(commit_flag.reduce(_||_), s_commit, s_wait)
    }
  }

  val commitVec = Wire(Vec(3, Vec(32, Bool())))
  for (i <- 0 until 32) {
    commitVec(0)(i) := (state_array(i) === s_commit || state_array(i) === s_wait) && io.wbIn.wen(0) && io.wbIn.waddr(0) === rd_array(i) && io.wbIn.woffset(0) === offset_array(i) &&
                       OpType_array(i) === LSUOpType.mld
    commitVec(1)(i) := (state_array(i) === s_commit || state_array(i) === s_wait) && io.wbIn.wen(1) && io.wbIn.waddr(1) === rd_array(i) && io.wbIn.woffset(1) === offset_array(i) &&
                       OpType_array(i) === LSUOpType.mld
    commitVec(2)(i) := state_array(i) === s_commit && io.wbIn.wen(2) && io.wbIn.waddr(2) === rd_array(i) && rs1_ready_array(i) === s_ready && rs2_ready_array(i) === s_ready &&
                      (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest)
  }

  /*  Seq.tabulate(3)(i =>
    VecInit.tabulate(32)(j =>
      state_array(j) === s_commit &&
      io.wbIn.wen(i) &&
      io.wbIn.waddr(i) === rd_array(j) &&
        ((io.wbIn.woffset(i) === offset_array(j) ||
          OpType_array(j) === MATUOpType.mmul || OpType_array(j) === MATUOpType.mtest) */

  val commitVecUInt = Wire(Vec(3, UInt(32.W)))
  val real_commitVec = Wire(Vec(3, Vec(32, Bool())))
  real_commitVec.foreach(_.foreach(_ := false.B))
  for (i <- 0 until 3) {
    commitVecUInt(i) := commitVec(i).asUInt
  }

  val selBits = Seq.tabulate(3)(i => Seq.tabulate(32)(j => Mux(j.U >= readPtr, true.B, false.B)))
  val shiftedIndices = Wire(Vec(3, Vec(32, UInt(5.W))))
  for (i <- 0 until 3) {
    for (j <- 0 until 32) {
      shiftedIndices(i)(j) := Mux(selBits(i)(j), j.U - (32.U - PopCount(selBits(i))), j.U + PopCount(selBits(i)))
    }
  }
  for (i <- 0 until 3) {
    for (j <- 0 until 32) {
      real_commitVec(i)(shiftedIndices(i)(j)) := commitVecUInt(i)(j).asBool
    }
  }

  for (i <- 0 until 3) {
    when(commitVec(i).asUInt.orR) {
      next_state_array(readPtr + PriorityEncoder(real_commitVec(i))) := s_retire
    }
  }

  /** rs state switch
   * state: s_unready -> s_ready, s_ready -> s_unready
   */
  for (i <- 0 until 32) {
    val rs1MatchVec_1 = dontTouch(Wire(Vec(i, Bool())))
    val rs1MatchVec_2 = dontTouch(Wire(Vec(i, Bool())))
    val real_rs1MatchVec = dontTouch(Wire(Vec(i, Bool())))
    for (j <- 0 until i) {
      rs1MatchVec_1(j) := rd_array(j) === rs1_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit)  && (OpType_array(i) === MATUOpType.mmul ||
                          OpType_array(i) === MATUOpType.mtest) && (state_array(j) === s_retire || state_array(j) === s_idle) &&
                          readPtr(5) === writePtr(5)
      rs1MatchVec_2(j) := rd_array(j) === rs1_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) && (OpType_array(i) === MATUOpType.mmul ||
                          OpType_array(i) === MATUOpType.mtest)
      real_rs1MatchVec(j) := ((rd_array(j) === rs1_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit)  && (OpType_array(i) === MATUOpType.mmul ||
                             OpType_array(i) === MATUOpType.mtest) && (state_array(j) === s_retire || state_array(j) === s_idle) &&
                             readPtr(5) === writePtr(5)) === (rd_array(j) === rs1_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) &&
                            (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest))) && (state_array(i) === s_wait || state_array(i) === s_commit)
    }
    rs1_ready_array(i) := Mux(real_rs1MatchVec.asUInt.andR, s_ready, s_unready)
  }

  for (i <- 0 until 32) {
    val rs2MatchVec_1 = dontTouch(Wire(Vec(i, Bool())))
    val rs2MatchVec_2 = dontTouch(Wire(Vec(i, Bool())))
    val real_rs2MatchVec = dontTouch(Wire(Vec(i, Bool())))
    for (j <- 0 until i) {
      rs2MatchVec_1(j) := rd_array(j) === rs2_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) && (OpType_array(i) === MATUOpType.mmul ||
                          OpType_array(i) === MATUOpType.mtest) &&  (state_array(j) === s_retire || state_array(j) === s_idle) &&
                          readPtr(5) === writePtr(5)
      rs2MatchVec_2(j) := rd_array(j) === rs2_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) && (OpType_array(i) === MATUOpType.mmul ||
                          OpType_array(i) === MATUOpType.mtest)
      real_rs2MatchVec(j) := ((rd_array(j) === rs2_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) && (OpType_array(i) === MATUOpType.mmul ||
                             OpType_array(i) === MATUOpType.mtest) && (state_array(j) === s_retire || state_array(j) === s_idle) &&
                             readPtr(5) === writePtr(5)) === (rd_array(j) === rs2_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) &&
                             (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest))) && (state_array(i) === s_wait || state_array(i) === s_commit)
    }
    rs2_ready_array(i) := Mux(real_rs2MatchVec.asUInt.andR, s_ready, s_unready)
  }

  /** WAW
   * Handle write after write hazard
   * */
  val rd_ready_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_ready))))
  for (i <- 0 until 32) {
    val m_wawMatchVec = dontTouch(Wire(Vec(i, Bool())))
    for (j <- 0 until i) {
      m_wawMatchVec(j) := rd_array(j) === rd_array(i) && (OpType_array(i) === MATUOpType.mtest || OpType_array(i) === MATUOpType.mmul) && (state_array(j) === s_wait || state_array(j) === s_commit) &&
        (state_array(i) === s_commit || (state_array(i) === s_wait && next_state_array(i) === s_commit)) && ((OpType_array(j) === LSUOpType.mld && offset_array(i) === offset_array(j)) || (OpType_array(j) === MATUOpType.mmul ||
        OpType_array(j) === MATUOpType.mtest))
    }
    rd_ready_array(i) := Mux(m_wawMatchVec.asUInt.orR, s_unready, s_ready)
  }

  val rs_ready_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    rs_ready_vec(i) := rs1_ready_array(i) === s_ready && rs2_ready_array(i) === s_ready && rd_ready_array(i) === s_ready &&
                      (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest) &&
                      (state_array(i) === s_wait || state_array(i) === s_commit)
  }
  io.fuIO.instr_out := instr_array(PriorityEncoder(rs_ready_vec.asUInt))
  io.fuIO.valid_out := rs_ready_vec.asUInt.orR && io.fuIO.ready_in &&
                       (state_array(PriorityEncoder(rs_ready_vec.asUInt)) === s_wait ||
                       state_array(PriorityEncoder(rs_ready_vec.asUInt)) === s_commit)
  io.fuIO.rs1_out := rs1_array(PriorityEncoder(rs_ready_vec.asUInt))
  io.fuIO.rs2_out := rs2_array(PriorityEncoder(rs_ready_vec.asUInt))
  io.fuIO.rd_out := rd_array(PriorityEncoder(rs_ready_vec.asUInt))

  /** WAW
   * Handle write after write hazard
   * */
  val ld_waw_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    val ld_wawMatchVec = dontTouch(Wire(Vec(i, Bool())))
    for (j <- 0 until i) {
      ld_wawMatchVec(j) := rd_array(j) === rd_array(i) && OpType_array(i) === LSUOpType.mld && (state_array(j) === s_wait || state_array(j) === s_commit) &&
        (state_array(i) === s_commit || (state_array(i) === s_wait && next_state_array(i) === s_commit)) && ((OpType_array(j) === LSUOpType.mld && offset_array(i) === offset_array(j)) || (OpType_array(j) === MATUOpType.mmul ||
                    OpType_array(j) === MATUOpType.mtest))
    }
    ld_waw_vec(i) := ld_wawMatchVec.asUInt.orR
  }

  val s_wait_commmit_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    s_wait_commmit_vec(i) :=  state_array(i) === s_commit || (state_array(i) === s_wait && next_state_array(i) === s_commit)
  }

  io.commitsIO.waw := ld_waw_vec(PriorityEncoder(ld_waw_vec))
  /**  WAR
   * Handle write after read hazard
   * */
  val war_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    val warMatchVec = dontTouch(Wire(Vec(i, Bool())))
    for (j <- 0 until i) {
      warMatchVec(j) := (rs1_array(j) === rd_array(i) || rs2_array(j) === rd_array(i)) && OpType_array(i) === LSUOpType.mld && (state_array(j) === s_wait || state_array(j) === s_commit) &&
        (state_array(i) === s_commit || (state_array(i) === s_wait && next_state_array(i) === s_commit)) && ((OpType_array(j) === LSUOpType.mld && offset_array(i) === offset_array(j)) || (OpType_array(j) === MATUOpType.mmul ||
        OpType_array(j) === MATUOpType.mtest))
    }
    war_vec(i) := warMatchVec.asUInt.orR
  }

  io.commitsIO.war := war_vec(PriorityEncoder(war_vec))


  /** dequeue
   * state: s_retire -> s_idle
   *
   */
  when(state_array(readPtr(4, 0)) === s_retire) {
    next_state_array(readPtr(4, 0)) := s_idle
    readPtr := readPtr + 1.U
  }

}