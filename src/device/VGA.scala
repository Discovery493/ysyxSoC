package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r     = Output(UInt(8.W))
  val g     = Output(UInt(8.W))
  val b     = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in    = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga   = new VGAIO
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)
  // 640x480 parameters
  val h_frontporch = 96.U
  val h_active     = 144.U
  val h_backporch  = 784.U
  val h_total      = 800.U
  val v_frontporch = 2.U
  val v_active     = 35.U
  val v_backporch  = 515.U
  val v_total      = 525.U
  // scan counter
  val x_cnt = RegInit(1.U(10.W))
  val y_cnt = RegInit(1.U(10.W))
  x_cnt := Mux(x_cnt === h_total, 1.U, x_cnt + 1.U)
  y_cnt := Mux(x_cnt === h_total, Mux(y_cnt === v_total, 1.U, y_cnt + 1.U), y_cnt)
  // sync and valid signals(need to delay 1 cycle to wait sync mem)
  val internal_hsync = x_cnt > h_frontporch
  val internal_vsync = y_cnt > v_frontporch
  val h_valid        = x_cnt > h_active && x_cnt <= h_backporch
  val v_valid        = y_cnt > v_active && y_cnt <= v_backporch
  val internal_valid = h_valid && v_valid
  // current scan position
  val h_addr = Mux(h_valid, x_cnt - h_active - 1.U, 0.U)
  val v_addr = Mux(v_valid, y_cnt - v_active - 1.U, 0.U)
  // frame buffer
  val vram = SyncReadMem(640 * 480, UInt(32.W))
  // APB bus
  io.in.pready  := true.B
  io.in.pslverr := false.B
  io.in.prdata  := 0.U
  // vram read and write
  val write_idx   = io.in.paddr(20, 2) // write 4 bytes each time
  val read_idx    = v_addr * 640.U + h_addr
  val is_write_en = io.in.psel && io.in.penable && io.in.pwrite
  val vga_data    = vram.read(read_idx) // has 1 cycle delay
  when(is_write_en) {
    vram.write(write_idx, io.in.pwdata)
  }
  // delay 1 cycle to output sync and valid signals
  io.vga.hsync := RegNext(internal_hsync)
  io.vga.vsync := RegNext(internal_vsync)
  val valid_delay = RegNext(internal_valid)
  io.vga.valid := valid_delay
  io.vga.r     := Mux(valid_delay, vga_data(23, 16), 0.U)
  io.vga.g     := Mux(valid_delay, vga_data(15, 8), 0.U)
  io.vga.b     := Mux(valid_delay, vga_data(7, 0), 0.U)
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
