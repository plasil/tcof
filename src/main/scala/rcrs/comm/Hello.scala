package rcrs.comm

import rcrs.AgentType
import rcrs.AgentType.AgentType
import scodec._
import scodec.bits._
import scodec.codecs._

case class Hello(agentType: AgentType) extends Message

object Hello {
  val agentTypeCodec = mappedEnum(uint(3), AgentType.values.map(v => (v, v.id)).toMap)

  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.HELLO.id, Message.MessageTypeBits))) ::
    agentTypeCodec
  }.as[Hello]
}