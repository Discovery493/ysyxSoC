package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIOold extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  //val dq  = Analog(16.W)
  val dqIn    = Output(UInt(16.W))
  val dqOut   = Input(UInt(16.W))
  val dqOutEn = Input(Bool())
}

class SDRAMIO extends Bundle {
  val clk  = Output(Bool())
  val cke  = Output(Bool())
  val cs   = Output(UInt(4.W))
  val ras  = Output(Bool())
  val cas  = Output(Bool())
  val we   = Output(Bool())
  val a    = Output(UInt(13.W))
  val ba   = Output(UInt(2.W))
  val dqm0 = Output(UInt(2.W))
  val dqm1 = Output(UInt(2.W))
  val dq0  = Analog(16.W)
  val dq1  = Analog(16.W)
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

class SDRAMHelper(memOffset: Int, high: Int)
    extends BlackBox(Map("offset" -> memOffset, "hiAddr" -> high))
    with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk   = Input(Bool())
    val addr  = Input(UInt(26.W))
    val dqm   = Input(UInt(2.W))
    val ren   = Input(Bool())
    val wen   = Input(Bool())
    val wdata = Input(UInt(16.W))
    val rdata = Output(UInt(16.W))
  })
  setInline(
    "SDRAMHelper.v",
    """module SDRAMHelper #(
      |  parameter offset = 32'h0,
      |  hiAddr = 32'h0 // 0 for low addr, 1 for high addr
      |)(
      |  input clk,
      |  input [25:0] addr,
      |  input [1:0] dqm,
      |  input ren,
      |  input wen,
      |  input [15:0] wdata,
      |  output reg [15:0] rdata
      |);
      |import "DPI-C" function void sdram_read(input int addr, output shortint data, input int offset, input int hiAddr);
      |import "DPI-C" function void sdram_write(input int addr, input shortint data, input byte dqm, input int offset, input int hiAddr);
      |always @(posedge clk) begin
      |  if (ren) begin
      |    sdram_read({6'b0, addr}, rdata, offset, hiAddr);
      |  end
      |  else if (!ren) begin
      |    rdata = 0;
      |  end
      |  if (wen) begin
      |    sdram_write({6'b0, addr}, wdata, {6'b0, dqm}, offset, hiAddr);
      |  end
      |end
      |endmodule
    """.stripMargin
  )
}

class sdramChisel(memOffset: Int, high: Int) extends RawModule {
  val io                                      = IO(Flipped(new SDRAMIOold))
  val posClock                                = io.clk.asClock
  val negClock                                = (!io.clk).asClock
  val bankReset                               = !io.cs && !io.ras && !io.cas && !io.we // reset all banks when load mode register
  val modeRegister                            = withClock(posClock) { RegEnable(io.a, bankReset) }
  val burstLength                             = 1.U << modeRegister(2, 0)
  val CASLatency                              = modeRegister(6, 4)
  val s_idle :: s_r_burst :: s_w_burst :: Nil = Enum(3)
  val state                                   = withClock(posClock) { withReset(bankReset) { RegInit(s_idle) } }
  val cmd_ACTIVE                              = !io.cs && !io.ras && io.cas && io.we
  val cmd_READ                                = !io.cs && io.ras && !io.cas && io.we
  val cmd_WRITE                               = !io.cs && io.ras && !io.cas && !io.we
  val cmd_BURST_TER                           = !io.cs && io.ras && io.cas && !io.we
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
    RegEnable(io.dqIn, cmd_WRITE || ((burstCounter === 1.U) && (state === s_w_burst)))
  }
  val rowReg0  = withClock(posClock) { RegEnable(io.a, cmd_ACTIVE && (io.ba === 0.U)) }
  val rowReg1  = withClock(posClock) { RegEnable(io.a, cmd_ACTIVE && (io.ba === 1.U)) }
  val rowReg2  = withClock(posClock) { RegEnable(io.a, cmd_ACTIVE && (io.ba === 2.U)) }
  val rowReg3  = withClock(posClock) { RegEnable(io.a, cmd_ACTIVE && (io.ba === 3.U)) }
  val addrNext = Wire(UInt(26.W))
  val addrReg  = withClock(posClock) { RegEnable(addrNext, cmd_READ || cmd_WRITE || (state =/= s_idle)) }
  addrNext := Mux(
    cmd_READ || cmd_WRITE,
    Cat(
      MuxLookup(io.ba, 0.U)(Seq(0.U -> rowReg0, 1.U -> rowReg1, 2.U -> rowReg2, 3.U -> rowReg3)),
      io.ba,
      io.a(8, 0),
      Fill(2, "b0".U)
    ),
    addrReg + 2.U
  )
  val fifoNext = Wire(UInt(32.W))
  val readFIFO = withClock(posClock) { RegNext(fifoNext) }
  val helper   = Module(new SDRAMHelper(memOffset, high))
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

  io.dqOutEn := withClock(posClock) { RegNext(state === s_r_burst) } // TODO: only caslatency=2 case implemented
  io.dqOut   := readFIFO(15, 0)
}

class ExtendedSDRAM extends RawModule {
  val io     = IO(Flipped(new SDRAMIO))
  val sdram0 = Module(new sdramChisel(0, 0))
  val sdram1 = Module(new sdramChisel(0, 1))
  val sdram2 = Module(new sdramChisel(0x4000000, 0))
  val sdram3 = Module(new sdramChisel(0x4000000, 1))
  sdram0.io.clk <> io.clk
  sdram1.io.clk <> io.clk
  sdram2.io.clk <> io.clk
  sdram3.io.clk <> io.clk
  sdram0.io.cke <> io.cke
  sdram1.io.cke <> io.cke
  sdram2.io.cke <> io.cke
  sdram3.io.cke <> io.cke
  sdram0.io.a <> io.a
  sdram1.io.a <> io.a
  sdram2.io.a <> io.a
  sdram3.io.a <> io.a
  sdram0.io.ba <> io.ba
  sdram1.io.ba <> io.ba
  sdram2.io.ba <> io.ba
  sdram3.io.ba <> io.ba
  sdram0.io.cs <> io.cs(0)
  sdram1.io.cs <> io.cs(1)
  sdram2.io.cs <> io.cs(2)
  sdram3.io.cs <> io.cs(3)
  sdram0.io.ras <> io.ras
  sdram1.io.ras <> io.ras
  sdram2.io.ras <> io.ras
  sdram3.io.ras <> io.ras
  sdram0.io.cas <> io.cas
  sdram1.io.cas <> io.cas
  sdram2.io.cas <> io.cas
  sdram3.io.cas <> io.cas
  sdram0.io.we <> io.we
  sdram1.io.we <> io.we
  sdram2.io.we <> io.we
  sdram3.io.we <> io.we
  sdram0.io.dqm := io.dqm0
  sdram1.io.dqm := io.dqm1
  sdram2.io.dqm := io.dqm0
  sdram3.io.dqm := io.dqm1
  val outEn0   = Wire(Bool())
  val outEn1   = Wire(Bool())
  val outData0 = Wire(UInt(io.dq0.getWidth.W))
  val outData1 = Wire(UInt(io.dq1.getWidth.W))
  val di0      = TriStateInBuf(io.dq0, outData0, outEn0)
  val di1      = TriStateInBuf(io.dq1, outData1, outEn1)
  outEn0         := Mux(io.cs(0) && io.cs(1), sdram2.io.dqOutEn, sdram0.io.dqOutEn)
  outEn1         := Mux(io.cs(0) && io.cs(1), sdram3.io.dqOutEn, sdram1.io.dqOutEn)
  outData0       := Mux(io.cs(0) && io.cs(1), sdram2.io.dqOut, sdram0.io.dqOut)
  outData1       := Mux(io.cs(0) && io.cs(1), sdram3.io.dqOut, sdram1.io.dqOut)
  sdram0.io.dqIn := di0
  sdram1.io.dqIn := di1
  sdram2.io.dqIn := di0
  sdram3.io.dqIn := di1
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 4
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

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
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
