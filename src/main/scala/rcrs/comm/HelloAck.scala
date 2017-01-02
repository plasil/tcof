package rcrs.comm

import rescuecore2.worldmodel.EntityID
import scodec._
import scodec.bits._
import scodec.codecs._

case class HelloAck(ids: List[EntityID]) extends Message

object HelloAck {
  val entityIDCodec = uint16.xmap((id: Int) => new EntityID(id), (id: EntityID) => id.getValue)

  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.HELLO_ACK.id, Message.MessageTypeBits))) ::
    ("ids" | listOfN(uint4, entityIDCodec))
  }.as[HelloAck]
}