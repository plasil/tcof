package rcrs.comm

import rescuecore2.worldmodel.EntityID
import scodec._
import scodec.bits._
import scodec.codecs._

case class RegResponse(id: EntityID, shortId: Int) extends Message

object RegResponse {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.REG_RESPONSE.id, Message.MessageTypeBits))) ::
    ("id" | Message.entityIDCodec) ::
    ("shortId" | Message.shortIdCodec)
  }.as[RegResponse]
}