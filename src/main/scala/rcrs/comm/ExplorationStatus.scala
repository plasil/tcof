package rcrs.comm

import scodec._
import scodec.bits._
import scodec.codecs._

case class ExplorationStatus(dummy: Int) extends Message

object ExplorationStatus {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.EXPLORATION_STATUS.id, Message.MessageTypeBits))) ::
    ("dummy" | int(32))
  }.as[ExplorationStatus]
}