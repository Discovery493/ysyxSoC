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
  val countNext                         = Wire(UInt(5.W))
  val inClock                           = io.sck.asClock
  val negClock                          = (!io.sck).asClock
  val reset                             = (io.ss.asBool).asAsyncReset
  val counter                           = withClock(inClock) { withReset(reset) { RegNext(countNext, 0.U) } }
  val state                             = withClock(inClock) { withReset(reset) { RegInit(s_recv) } }
  val num                               = withClock(inClock) { RegEnable(numNext, !io.ss.asBool) }
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle -> s_idle,
      s_recv -> Mux((counter < 8.U), s_recv, s_send),
      s_send -> Mux((counter < 16.U), s_send, s_idle)
    )
  )
  numNext   := Mux((state === s_recv) && (counter < 8.U), Cat(num(6, 0), io.mosi), num)
  countNext := Mux((state === s_idle), counter, counter + 1.U)
  io.miso   := Mux((state === s_send), num(counter - 9.U), true.B)
}
