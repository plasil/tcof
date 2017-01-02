package rcrs.comm

import rcrs.AgentType
import rescuecore2.worldmodel.EntityID
import scodec.bits.BitVector

object Test {
  def main(args: Array[String]): Unit = {
    val bytes1 = Message.encode(ExplorationStatus(42))
    println(BitVector(bytes1))

    val msg1 = Message.decode(bytes1)
    println(msg1)

    val bytes2 = Message.encode(Hello(AgentType.POLICE_FORCE))
    println(BitVector(bytes2))

    val msg2 = Message.decode(bytes2)
    println(msg2)

    val bytes3 = Message.encode(HelloAck(List(new EntityID(12), new EntityID(24))))
    println(BitVector(bytes3))

    val msg3 = Message.decode(bytes3)
    println(msg3)
  }
}