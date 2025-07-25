package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val dq  = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in    = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

/*class sdramBank extends RawModule {
  val io = IO(new Bundle {
    val clk         = Input(Bool())
    val rst         = Input(Bool())
    val cs          = Input(Bool())
    val we          = Input(Bool())
    val cas         = Input(Bool())
    val ras         = Input(Bool())
    val a           = Input(UInt(13.W))
    val dqm         = Input(UInt(2.W))
    val inData      = Input(UInt(16.W))
    val outData     = Output(UInt(16.W))
    val outEnable   = Output(Bool())
    val burstLength = Input(UInt(3.W))
    val CASLatency  = Input(UInt(3.W))
  })
  val s_idle :: s_active :: s_r_burst :: s_w_burst :: s_wait_latency :: Nil = Enum(5)

  val state          = withClock(io.clk.asClock) { withReset(io.rst) { RegInit(s_idle) } }
  val cmd_ACTIVE     = !io.ras && io.cas && io.we
  val cmd_READ       = io.ras && !io.cas && io.we
  val cmd_WRITE      = io.ras && !io.cas && !io.we
  val cmd_BURST_TER  = io.ras && io.cas && !io.we
  val cmd_PRECHARGE  = !io.ras && io.cas && !io.we
  val latencyNext    = Wire(UInt(2.W))
  val latencyEnable  = Wire(Bool())
  val latencyCounter = RegEnable(latencyNext, latencyEnable)
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle    -> Mux(cmd_ACTIVE, s_active, s_idle),
      s_active  -> Mux(cmd_PRECHARGE, s_idle, Mux(cmd_READ, s_r_burst, Mux(cmd_WRITE, s_w_burst, s_active))),
      s_r_burst -> Mux(cmd_PRECHARGE || !(latencyCounter.orR) || cmd_BURST_TER, s_wait_latency, s_r_burst),
      s_w_burst -> Mux(cmd_PRECHARGE || !(latencyCounter.orR) || cmd_BURST_TER, s_wait_latency, s_w_burst)
    )
  )
  val activeRow = RegEnable(io.a, cmd_ACTIVE) // store row address in ACTIVE state
}*/
class SDRAMHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk   = Input(Bool())
    val addr  = Input(UInt(25.W))
    val dqm   = Input(UInt(2.W))
    val ren   = Input(Bool())
    val wen   = Input(Bool())
    val wdata = Input(UInt(16.W))
    val rdata = Output(UInt(16.W))
  })
  setInline(
    "SDRAMHelper.v",
    """module SDRAMHelper(
      |  input clk,
      |  input [24:0] addr,
      |  input [1:0] dqm,
      |  input ren,
      |  input wen,
      |  input [15:0] wdata,
      |  output reg [15:0] rdata
      |);
      |import "DPI-C" function void sdram_read(input int addr, output shortint data);
      |import "DPI-C" function void sdram_write(input int addr, input shortint data, input byte dqm);
      |always @(posedge clk) begin
      |  if (ren) begin
      |    sdram_read({7'b0, addr}, rdata);
      |  end
      |  else if (!ren) begin
      |    rdata = 0;
      |  end
      |  if (wen) begin
      |    sdram_write({7'b0, addr}, wdata, {6'b0, dqm});
      |  end
      |end
      |endmodule
    """.stripMargin
  )
}

class sdramChisel extends RawModule {
  val io        = IO(Flipped(new SDRAMIO))
  val outEnable = Wire(Bool())
  val outData   = Wire(UInt(io.dq.getWidth.W))
  val inData    = Wire(UInt(io.dq.getWidth.W))
  val di        = TriStateInBuf(io.dq, outData, outEnable) // change this if you need
  inData := di
  val posClock                                = io.clk.asClock
  val negClock                                = (!io.clk).asClock
  val bankReset                               = !io.cs && !io.ras && !io.cas && !io.we // reset all banks when load mode register
  val modeRegister                            = withClock(posClock) { RegEnable(io.a, bankReset) }
  val burstLength                             = 1.U << modeRegister(2, 0)
  val CASLatency                              = modeRegister(6, 4)
  val s_idle :: s_r_burst :: s_w_burst :: Nil = Enum(3)
  val state                                   = withClock(posClock) { withReset(bankReset) { RegInit(s_idle) } }
  val cmd_ACTIVE                              = !io.ras && io.cas && io.we
  val cmd_READ                                = io.ras && !io.cas && io.we
  val cmd_WRITE                               = io.ras && !io.cas && !io.we
  val cmd_BURST_TER                           = io.ras && io.cas && !io.we
  val blCounterNext                           = Wire(UInt(4.W))
  val burstCounter = withClock(posClock) {
    withReset(cmd_READ || cmd_WRITE) { RegNext(blCounterNext, burstLength - 1.U) }
  }
  blCounterNext := burstCounter - 1.U
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle    -> Mux(cmd_READ, s_r_burst, Mux(cmd_WRITE, s_w_burst, s_idle)),
      s_r_burst -> Mux(cmd_BURST_TER || !(burstCounter.orR), s_idle, s_r_burst),
      s_w_burst -> Mux(cmd_BURST_TER || !(burstCounter.orR), s_idle, s_w_burst)
    )
  )
  val columnReg = withClock(posClock) { RegEnable(io.a(8, 0), cmd_READ || cmd_WRITE) }
  val bankReg   = withClock(posClock) { RegEnable(io.ba, cmd_READ || cmd_WRITE) }
  val dqmReg    = withClock(posClock) { RegEnable(io.dqm, cmd_WRITE || ((burstCounter === 1.U) && (state === s_w_burst))) }
  val wdataReg = withClock(posClock) {
    RegEnable(inData, cmd_WRITE || ((burstCounter === 1.U) && (state === s_w_burst)))
  }
  val rowReg   = withClock(posClock) { RegEnable(io.a, cmd_ACTIVE) }
  val addrNext = Wire(UInt(25.W))
  val addrReg  = withClock(posClock) { RegEnable(addrNext, cmd_READ || cmd_WRITE || (state =/= s_idle)) }
  addrNext := Mux(cmd_READ || cmd_WRITE, Cat(rowReg, io.ba, io.a(8, 0), "b0".U), addrReg + 2.U)
  val fifoNext = Wire(UInt(32.W))
  val readFIFO = withClock(posClock) { RegNext(fifoNext) }
  val helper   = Module(new SDRAMHelper)
  fifoNext := (readFIFO >> 16.U) & Mux(CASLatency === 2.U, "hff00".U, "h00ff".U) | Mux(
    CASLatency === 2.U,
    Cat("h00".U, helper.io.rdata),
    Cat(helper.io.rdata, "h00".U)
  )
  helper.io.clk   := !io.clk
  helper.io.addr  := addrReg
  helper.io.dqm   := dqmReg
  helper.io.ren   := (state === s_r_burst)
  helper.io.wen   := (state === s_w_burst)
  helper.io.wdata := wdataReg

  outEnable := withClock(posClock) { RegNext(state === s_r_burst) } // TODO: only caslatency=2 case implemented
  outData   := readFIFO(15, 0)
  /*val bank0        = Module(new sdramBank)
  val bank1        = Module(new sdramBank)
  val bank2        = Module(new sdramBank)
  val bank3        = Module(new sdramBank)
  bank0.io.clk    := io.clk && io.cke
  bank1.io.clk    := io.clk && io.cke
  bank2.io.clk    := io.clk && io.cke
  bank3.io.clk    := io.clk && io.cke
  bank0.io.rst    := bankReset
  bank1.io.rst    := bankReset
  bank2.io.rst    := bankReset
  bank3.io.rst    := bankReset
  bank0.io.cs     := io.cs || !(bankSel === 0.U)
  bank1.io.cs     := io.cs || !(bankSel === 1.U)
  bank2.io.cs     := io.cs || !(bankSel === 2.U)
  bank3.io.cs     := io.cs || !(bankSel === 3.U)
  bank0.io.we     := io.we
  bank1.io.we     := io.we
  bank2.io.we     := io.we
  bank3.io.we     := io.we
  bank0.io.cas    := io.cas
  bank1.io.cas    := io.cas
  bank2.io.cas    := io.cas
  bank3.io.cas    := io.cas
  bank0.io.ras    := io.ras
  bank1.io.ras    := io.ras
  bank2.io.ras    := io.ras
  bank3.io.ras    := io.ras
  bank0.io.a      := io.a
  bank1.io.a      := io.a
  bank2.io.a      := io.a
  bank3.io.a      := io.a
  bank0.io.dqm    := io.dqm
  bank1.io.dqm    := io.dqm
  bank2.io.dqm    := io.dqm
  bank3.io.dqm    := io.dqm
  bank0.io.inData := inData
  bank1.io.inData := inData
  bank2.io.inData := inData
  bank3.io.inData := inData
  outData := MuxLookup(bankSel, 0.U)(
    Seq(
      0.U -> bank0.io.outData,
      1.U -> bank1.io.outData,
      2.U -> bank2.io.outData,
      3.U -> bank3.io.outData
    )
  )
  outEnable := MuxLookup(bankSel, false.B)(
    Seq(
      0.U -> bank0.io.outEnable,
      1.U -> bank1.io.outEnable,
      2.U -> bank2.io.outEnable,
      3.U -> bank3.io.outEnable
    )
  )
  bank0.io.burstLength := burstLength
  bank1.io.burstLength := burstLength
  bank2.io.burstLength := burstLength
  bank3.io.burstLength := burstLength
  bank0.io.CASLatency  := CASLatency
  bank1.io.CASLatency  := CASLatency
  bank2.io.CASLatency  := CASLatency
  bank3.io.CASLatency  := CASLatency*/
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 8
  val node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address       = address,
            executable    = true,
            supportsWrite = TransferSizes(1, beatBytes),
            supportsRead  = TransferSizes(1, beatBytes),
            interleavedId = Some(0)
          )
        ),
        beatBytes = beatBytes
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _)      = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val converter = Module(new AXI4DataWidthConverter64to32)
    converter.io.clock := clock
    converter.io.reset := reset.asBool
    converter.io.in <> in

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> converter.io.out
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val (in, _)      = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
