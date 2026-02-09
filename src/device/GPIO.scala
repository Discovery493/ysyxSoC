package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import chisel3.util.experimental.decode.decoder
import chisel3.util.experimental.decode.QMCMinimizer
import chisel3.util.experimental.decode.TruthTable

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in  = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio  = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class GetGPIOSeg extends RawModule {
  val io = IO(new Bundle {
    val in  = Input(UInt(4.W))
    val out = Output(UInt(8.W))
  })
  io.out := decoder(
    QMCMinimizer,
    io.in,
    TruthTable(
      Map(
        BitPat("b0000") -> BitPat("b11111111"),
        BitPat("b0001") -> BitPat("b00011110"),
        BitPat("b0010") -> BitPat("b00001111")
      ),
      BitPat("b????????")
    )
  )
}

object GetGPIOSeg {
  def apply(in_num: UInt) = {
    val encode = Module(new GetGPIOSeg)
    encode.io.in := in_num
    encode.io.out
  }
}

class gpioChisel extends Module {
  val io = IO(new GPIOCtrlIO)
  // states
  val s_idle :: s_trans :: Nil = Enum(2)
  val state                    = withClock(io.clock) { withReset(io.reset) { RegInit(s_idle) } }
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle  -> Mux(io.in.psel, s_trans, s_idle),
      s_trans -> Mux(io.in.penable && io.in.pready, s_idle, s_trans)
    )
  )
  // GPIO controller registers
  val ledNext    = Wire(UInt(16.W))
  val ledEnable  = Wire(Bool())
  val led        = withClock(io.clock) { withReset(io.reset) { RegEnable(ledNext, 0.U, ledEnable) } }
  val switchNext = Wire(UInt(16.W))
  val switch     = withClock(io.clock) { withReset(io.reset) { RegNext(switchNext) } }
  val segNext    = Wire(UInt(32.W))
  val segEnable  = Wire(Bool())
  val seg        = withClock(io.clock) { withReset(io.reset) { RegEnable(segNext, 0.U, segEnable) } }
  // I/O to board
  switchNext     := io.gpio.in
  io.gpio.out    := led
  io.gpio.seg(0) := GetGPIOSeg(seg(3, 0))
  io.gpio.seg(1) := GetGPIOSeg(seg(7, 4))
  io.gpio.seg(2) := GetGPIOSeg(seg(11, 8))
  io.gpio.seg(3) := GetGPIOSeg(seg(15, 12))
  io.gpio.seg(4) := GetGPIOSeg(seg(19, 16))
  io.gpio.seg(5) := GetGPIOSeg(seg(23, 20))
  io.gpio.seg(6) := GetGPIOSeg(seg(27, 24))
  io.gpio.seg(7) := GetGPIOSeg(seg(31, 28))
  // APB bus
  ledEnable := io.in.pwrite && (io.in.paddr(3, 0) === 0.U) && io.in.penable && io.in.pready // addr:0x0
  segEnable := io.in.pwrite && (io.in.paddr(3, 0) === 8.U) && io.in.penable && io.in.pready // addr:0x8
  val wdataMask =
    Cat(Fill(8, io.in.pstrb(3)), Fill(8, io.in.pstrb(2)), Fill(8, io.in.pstrb(1)), Fill(8, io.in.pstrb(0)))
  segNext       := (seg & ~wdataMask) | (io.in.pwdata & wdataMask)
  ledNext       := (led & ~wdataMask(15, 0)) | (io.in.pwdata(15, 0) & wdataMask(15, 0))
  io.in.pready  := (state === s_trans)
  io.in.pslverr := (io.in.paddr =/= "h10002000".U) && (io.in.paddr =/= "h10002004".U) && (io.in.paddr =/= "h10002008".U)
  io.in.prdata := MuxLookup(io.in.paddr(3, 0), 0.U)(
    Seq(
      0.U -> Cat(Fill(16, "b0".U), led),
      4.U -> Cat(Fill(16, "b0".U), switch),
      8.U -> seg
    )
  )
}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val (in, _)     = node.in(0)
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
