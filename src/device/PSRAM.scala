package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class QSPIIO extends Bundle {
  val sck  = Output(Bool())
  val ce_n = Output(Bool())
  val dio  = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi  = new QSPIIO
  })
}

class PSRAMHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val counter = Input(UInt(5.W))
    val ren     = Input(Bool())
    val wen     = Input(Bool())
    val addr    = Input(UInt(32.W))
    val wdata   = Input(UInt(8.W))
    val rdata   = Output(UInt(8.W))
  })
  setInline(
    "PSRAMHelper.v",
    """module PSRAMHelper(
      |  input [4:0] counter,
      |  input ren,
      |  input wen,
      |  input [31:0] addr,
      |  input [7:0] wdata,
      |  output reg [7:0] rdata
      |);
      |import "DPI-C" function void psram_read(input int addr, output byte data);
      |import "DPI-C" function void psram_write(input int addr, input byte data);
      |always @(counter) begin
      |  if (ren) begin
      |    psram_read(addr, rdata);
      |  end
      |  else if (!ren) begin
      |    rdata = 0;
      |  end
      |  if (wen) begin
      |    psram_write(addr, wdata);
      |  end
      |end
      |endmodule
    """.stripMargin
  )
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel extends RawModule {
  val io                                                                                     = IO(Flipped(new QSPIIO))
  val outEnable                                                                              = Wire(Bool())
  val outData                                                                                = Wire(UInt(io.dio.getWidth.W))
  val inData                                                                                 = Wire(UInt(io.dio.getWidth.W))
  val di                                                                                     = TriStateInBuf(io.dio, outData, outEnable) // change this if you need
  val negClock                                                                               = (!io.sck).asClock
  val posClock                                                                               = io.sck.asClock
  val reset                                                                                  = io.ce_n.asAsyncReset
  val s_cmd :: s_rd_addr :: s_wr_addr :: s_rd_wait :: s_wr_data :: s_rd_data :: s_err :: Nil = Enum(7)
  val state                                                                                  = withClock(posClock) { withReset(reset) { RegInit(s_cmd) } }
  val countNext                                                                              = Wire(UInt(5.W))
  val counter                                                                                = withClock(posClock) { withReset(reset) { RegNext(countNext, 0.U) } }
  val qpiNext                                                                                = Wire(Bool())
  val qpi                                                                                    = withClock(posClock) { RegEnable(qpiNext, (state === s_cmd) && (counter === 7.U)) }
  val cmdNext                                                                                = Wire(UInt(8.W))
  val cmdReg                                                                                 = withClock(posClock) { RegNext(cmdNext) }
  val addrNext                                                                               = Wire(UInt(24.W))
  val addrReg = withClock(posClock) {
    RegEnable(
      addrNext,
      (state === s_rd_addr) || (state === s_wr_addr) || (state === s_rd_wait) || ((state === s_wr_data) && !counter(
        0
      ) && (counter > Mux(qpi, 8.U, 14.U)))
    )
  }
  val byte0N = Wire(UInt(8.W))
  val byte1N = Wire(UInt(8.W))
  val byte2N = Wire(UInt(8.W))
  val byte3N = Wire(UInt(8.W))
  val byte0 = withClock(posClock) {
    RegEnable(
      byte0N,
      (state === s_rd_wait) && (counter === Mux(qpi, 8.U, 14.U)) || (state === s_wr_data) && (counter(
        4,
        1
      ) === Mux(qpi, "b0100".U, "b0111".U))
    )
  }
  val byte1 = withClock(posClock) {
    RegEnable(
      byte1N,
      (state === s_rd_wait) && (counter === Mux(qpi, 9.U, 15.U)) || (state === s_wr_data) && (counter(
        4,
        1
      ) === Mux(qpi, "b0101".U, "b1000".U))
    )
  }
  val byte2 = withClock(posClock) {
    RegEnable(
      byte2N,
      (state === s_rd_wait) && (counter === Mux(qpi, 10.U, 16.U)) || (state === s_wr_data) && (counter(
        4,
        1
      ) === Mux(qpi, "b0110".U, "b1001".U))
    )
  }
  val byte3 = withClock(posClock) {
    RegEnable(
      byte3N,
      (state === s_rd_wait) && (counter === Mux(qpi, 11.U, 17.U)) || (state === s_wr_data) && (counter(
        4,
        1
      ) === Mux(qpi, "b0111".U, "b1010".U))
    )
  }
  val helper = Module(new PSRAMHelper)
  state := MuxLookup(state, s_err)(
    Seq(
      s_cmd -> Mux(
        counter < Mux(qpi, 1.U, 7.U),
        s_cmd,
        Mux(
          Mux(qpi, Cat(cmdReg(3, 0), inData), Cat(cmdReg(6, 0), inData(0))) === "heb".U,
          s_rd_addr,
          Mux(Mux(qpi, Cat(cmdReg(3, 0), inData), Cat(cmdReg(6, 0), inData(0))) === "h38".U, s_wr_addr, s_err)
        )
      ),
      s_rd_addr -> Mux(counter < Mux(qpi, 7.U, 13.U), s_rd_addr, s_rd_wait),
      s_wr_addr -> Mux(counter < Mux(qpi, 7.U, 13.U), s_wr_addr, s_wr_data),
      s_rd_wait -> Mux(counter < Mux(qpi, 14.U, 20.U), s_rd_wait, s_rd_data),
      s_wr_data -> s_wr_data,
      s_rd_data -> s_rd_data
    )
  )
  outEnable := (state === s_rd_data)
  outData := MuxLookup(counter, 0.U)(
    Seq(
      Mux(qpi, 15.U, 21.U) -> byte0(7, 4),
      Mux(qpi, 16.U, 22.U) -> byte0(3, 0),
      Mux(qpi, 17.U, 23.U) -> byte1(7, 4),
      Mux(qpi, 18.U, 24.U) -> byte1(3, 0),
      Mux(qpi, 19.U, 25.U) -> byte2(7, 4),
      Mux(qpi, 20.U, 26.U) -> byte2(3, 0),
      Mux(qpi, 21.U, 27.U) -> byte3(7, 4),
      Mux(qpi, 22.U, 28.U) -> byte3(3, 0)
    )
  )
  inData    := di
  cmdNext   := Mux(qpi, Cat(cmdReg(3, 0), inData), Cat(cmdReg(6, 0), inData(0)))
  addrNext  := Mux((state === s_rd_addr) || (state === s_wr_addr), Cat(addrReg(19, 0), inData), addrReg + 1.U)
  countNext := counter + 1.U
  byte0N    := Mux(state === s_rd_wait, helper.io.rdata, Cat(byte0(3, 0), inData))
  byte1N    := Mux(state === s_rd_wait, helper.io.rdata, Cat(byte1(3, 0), inData))
  byte2N    := Mux(state === s_rd_wait, helper.io.rdata, Cat(byte2(3, 0), inData))
  byte3N    := Mux(state === s_rd_wait, helper.io.rdata, Cat(byte3(3, 0), inData))
  qpiNext   := Cat(cmdReg(6, 0), inData(0)) === "h35".U
  // connect helper
  helper.io.counter := counter
  helper.io.ren     := (state === s_rd_wait) && (counter >= Mux(qpi, 8.U, 14.U)) && (counter <= Mux(qpi, 11.U, 17.U))
  helper.io.wen := (state === s_wr_data) && Mux(
    qpi,
    ((counter === 10.U) || (counter === 12.U) || (counter === 14.U) || (counter === 16.U)),
    (Cat(counter(4, 3), counter(0)) === "b100".U)
  ) // 16, 18, 20, 22
  helper.io.addr := Cat("h80".U, addrReg)
  helper.io.wdata := MuxLookup(counter, 0.U)(
    Seq(
      Mux(qpi, 10.U, 16.U) -> byte0,
      Mux(qpi, 12.U, 18.U) -> byte1,
      Mux(qpi, 14.U, 20.U) -> byte2,
      Mux(qpi, 16.U, 22.U) -> byte3
    )
  )
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
