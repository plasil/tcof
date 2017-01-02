package rcrs.comm

import scodec.bits._
import scodec.codecs._

case class RegRequest() extends Message

object RegRequest {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.REG_REQUEST.id, Message.MessageTypeBits)))
  }.xmap((x: Unit) => new RegRequest(), (x: RegRequest) => {} )
}