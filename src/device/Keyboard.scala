package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk  = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2   = new PS2IO
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

class SyncFIFO(num: Int, width: Int) extends Module {
  val io = IO(new Bundle {
    val write = Flipped(Decoupled(UInt(width.W)))
    val read  = Decoupled(UInt(width.W))
  })

  val buf = Reg(Vec(num, UInt(width.W)))
  // read and write pointer
  val readPtr  = RegInit(0.U(log2Ceil(num).W))
  val writePtr = RegInit(0.U(log2Ceil(num).W))
  val numData  = RegInit(0.U(log2Ceil(num + 1).W))
  // status
  val isFull  = numData === num.U
  val isEmpty = numData === 0.U
  io.read.valid  := !isEmpty
  io.write.ready := !isFull
  io.read.bits   := buf(readPtr)

  val writeOH = UIntToOH(writePtr)
  buf.zipWithIndex.foreach {
    case (reg, i) =>
      val isTarget = io.write.fire && writeOH(i)
      reg := Mux(isTarget, io.write.bits, reg)
  }
  // ptr (num may not 2^(integer))
  def nextPtr(ptr: UInt): UInt = Mux(ptr === (num - 1).U, 0.U, ptr + 1.U)
  writePtr := Mux(io.write.fire, nextPtr(writePtr), writePtr)
  readPtr  := Mux(io.read.fire, nextPtr(readPtr), readPtr)
  numData  := numData + io.write.fire.asUInt - io.read.fire.asUInt
}

class ps2Chisel extends Module {
  val io = IO(new PS2CtrlIO)
  // 2-FF Synchronizer
  val ps2_clk_sync  = RegNext(RegNext(io.ps2.clk, false.B), false.B)
  val ps2_data_sync = RegNext(RegNext(io.ps2.data, false.B), false.B)
  // Falling Edge Detection
  val ps2_clk_old     = RegNext(ps2_clk_sync, false.B)
  val is_falling_edge = ps2_clk_old && !ps2_clk_sync
  // Deserialize
  val counter    = RegInit(0.U(4.W))
  val shift_reg  = RegInit(0.U(11.W))
  val frame_done = counter === 11.U
  shift_reg := Mux(is_falling_edge, Cat(ps2_data_sync, shift_reg(10, 1)), shift_reg)
  counter   := Mux(frame_done, 0.U, Mux(is_falling_edge, counter + 1.U, counter))
  val scan_code        = shift_reg(8, 1)
  val data_valid_pulse = RegNext(counter) === 10.U && counter === 11.U // 1 cycle pulse!
  // SyncFIFO
  val fifo = Module(new SyncFIFO(num = 16, width = 8))
  fifo.io.write.valid := data_valid_pulse
  fifo.io.write.bits  := scan_code
  // Data output, read FIFO if not empty. Otherwise, output 0 to prevent CPU stall.
  val is_read_trans = io.in.psel && io.in.penable && !io.in.pwrite
  fifo.io.read.ready := is_read_trans && fifo.io.read.valid
  io.in.prdata       := Mux(fifo.io.read.valid, Cat(0.U(24.W), fifo.io.read.bits), 0.U)
  io.in.pready       := true.B
  io.in.pslverr      := false.B
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(
    Seq(
      APBSlavePortParameters(
        Seq(APBSlaveParameters(address = address, executable = true, supportsRead = true, supportsWrite = true)),
        beatBytes = 4
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _)    = node.in(0)
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
