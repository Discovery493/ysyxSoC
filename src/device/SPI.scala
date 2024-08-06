package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck  = Output(Bool())
  val ss   = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock       = Input(Clock())
    val reset       = Input(Reset())
    val in          = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi         = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val spi_bundle = IO(new SPIIO)

    val mspi   = Module(new spi_top_apb)
    val irq    = Wire(Bool())
    val enable = RegInit(false.B)
    val s_select :: s_normal :: s_xip_cmd :: s_xip_div :: s_xip_ss :: s_xip_ctrl :: s_xip_wait :: s_xip_read :: s_back :: Nil =
      Enum(9)
    val state = RegInit(s_select)
    state := MuxLookup(state, s_select)(
      Seq(
        s_select   -> Mux(in.psel, Mux(in.paddr(29), s_xip_cmd, s_normal), s_select),
        s_normal   -> Mux(enable && mspi.io.in.pready, s_back, s_normal),
        s_xip_cmd  -> Mux(enable && mspi.io.in.pready, s_xip_div, s_xip_cmd),
        s_xip_div  -> Mux(enable && mspi.io.in.pready, s_xip_ss, s_xip_div),
        s_xip_ss   -> Mux(enable && mspi.io.in.pready, s_xip_ctrl, s_xip_ss),
        s_xip_ctrl -> Mux(enable && mspi.io.in.pready, s_xip_wait, s_xip_ctrl),
        s_xip_wait -> Mux(irq, s_xip_read, s_xip_wait),
        s_xip_read -> Mux(enable && mspi.io.in.pready, s_back, s_xip_read),
        s_back     -> s_select
      )
    )
    enable := ((state =/= s_select) && (state =/= s_xip_wait) && (state =/= s_back)) && !(enable && mspi.io.in.pready)
    val rdata =
      RegEnable(mspi.io.in.prdata, 0.U, ((state === s_xip_read) || (state === s_normal) && enable && mspi.io.in.pready))
    val fetch =
      RegEnable(state === s_xip_read, ((state === s_xip_read) || (state === s_normal)) && enable && mspi.io.in.pready)
    mspi.io.clock := clock
    mspi.io.reset := reset
    // mspi.io.in <> in
    spi_bundle <> mspi.io.spi
    irq := mspi.io.spi_irq_out
    // overwrite
    mspi.io.in.psel    := (state =/= s_select) && (state =/= s_xip_wait) && (state =/= s_back)
    mspi.io.in.penable := enable
    mspi.io.in.pprot   := in.pprot
    mspi.io.in.pwrite := Mux(
      state === s_normal,
      in.pwrite,
      (state === s_xip_cmd) || (state === s_xip_div) || (state === s_xip_ss) || (state === s_xip_ctrl)
    )
    mspi.io.in.paddr := MuxLookup(state, 0.U)(
      Seq(
        s_normal   -> in.paddr,
        s_xip_cmd  -> "h10001004".U, // TX1
        s_xip_div  -> "h10001014".U, // DIVIDER
        s_xip_ss   -> "h10001018".U, // SS
        s_xip_ctrl -> "h10001010".U, // CTRL
        s_xip_read -> "h10001000".U // RX0
      )
    )
    mspi.io.in.pwdata := MuxLookup(state, 0.U)(
      Seq(
        s_normal   -> in.pwdata,
        s_xip_cmd  -> Cat("h03".U, in.paddr(23, 0)),
        s_xip_div  -> 0.U,
        s_xip_ss   -> 1.U,
        s_xip_ctrl -> "h00003540".U
      )
    )
    mspi.io.in.pstrb := Mux(state === s_normal, in.pstrb, "hf".U)
    in.prdata        := Mux(fetch, Cat(rdata(7, 0), rdata(15, 8), rdata(23, 16), rdata(31, 24)), rdata)
    in.pready        := (state === s_back)
    in.pslverr       := mspi.io.in.pslverr
  }
}
