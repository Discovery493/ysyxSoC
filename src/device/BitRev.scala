package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io                                = IO(Flipped(new SPIIO(1)))
  val s_idle :: s_recv :: s_send :: Nil = Enum(3)
  val numNext                           = Wire(UInt(8.W))
  val countNext                         = Wire(UInt(3.W))
  val inClock                           = io.sck.asClock
  val negClock                          = (!io.sck).asClock
  val reset                             = (io.ss.asBool).asAsyncReset
  val counter                           = withClock(negClock) { withReset(reset) { RegNext(countNext, 0.U) } }
  val state                             = withClock(negClock) { withReset(reset) { RegInit(s_recv) } }
  val num                               = withClock(negClock) { RegEnable(numNext, !io.ss.asBool) }
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle -> s_idle,
      s_recv -> Mux((counter < 8.U), s_recv, s_send),
      s_send -> Mux((counter < 16.U), s_send, s_idle)
    )
  )
  numNext := Mux((state === s_recv), Cat(num(6, 0), io.mosi), Cat(0.U, num(7, 1)))
  io.miso := Mux((state === s_send), num(0), true.B)
}
