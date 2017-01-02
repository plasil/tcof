package rcrs.comm

import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits._

import scala.reflect.ClassTag

abstract class Message

object Message {
  val MessageTypeBits = 4

  object MessageType extends Enumeration {
    type MessageType = Value

    val HELLO = Value(0)
    val HELLO_ACK = Value(1)
    val EXPLORATION_STATUS = Value(2)
  }

  val codecs = Map[Class[_ <: Message], Codec[_ <: Message]](
    classOf[Hello] -> Hello.codec,
    classOf[HelloAck] -> HelloAck.codec,
    classOf[ExplorationStatus] -> ExplorationStatus.codec
  )

  def decode(bytes: Array[Byte]): Message = {
    val bits = BitVector(bytes)

    val codecsIterator = codecs.values.iterator
    var result: Message = null

    while (result == null && codecsIterator.hasNext) {
      val codec = codecsIterator.next
      codec.decodeValue(bits) match {
        case Successful(msg) => result = msg
        case Failure(_) =>
      }
    }

    result
  }

  def encode[MessageType <: Message](msg: MessageType)(implicit messageType: ClassTag[MessageType]): Array[Byte] = {
    val codec = codecs(messageType.runtimeClass.asInstanceOf[Class[_ <: Message]]).asInstanceOf[Codec[MessageType]]
    val bits = codec.encode(msg.asInstanceOf[MessageType]).require

    bits.toByteArray
  }
}