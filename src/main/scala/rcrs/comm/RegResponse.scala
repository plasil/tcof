package rcrs.comm

import rescuecore2.worldmodel.EntityID
import scodec._
import scodec.bits._
import scodec.codecs._

case class RegResponse(id: EntityID, shortId: Int) extends Message

object RegResponse {
  val entityIDCodec = int32.xmap((id: Int) => new EntityID(id), (id: EntityID) => id.getValue)

  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.REG_RESPONSE.id, Message.MessageTypeBits))) ::
    ("id" | entityIDCodec) ::
    ("shortId" | Message.shortIdCodec)
  }.as[RegResponse]
}