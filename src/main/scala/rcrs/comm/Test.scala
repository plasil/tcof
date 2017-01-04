package rcrs.comm

import rcrs.traits.map2d.RoadStatus
import rescuecore2.worldmodel.EntityID
import scodec.bits.BitVector

object Test {
  def main(args: Array[String]): Unit = {
    val bytes1 = Message.encode(ExplorationStatus(new EntityID(42), Map(1 -> RoadStatus(42), 2 -> RoadStatus(14))))
    println(BitVector(bytes1))

    val msg1 = Message.decode(bytes1)
    println(msg1)

    val bytes2 = Message.encode(RegRequest())
    println(BitVector(bytes2))

    val msg2 = Message.decode(bytes2)
    println(msg2)

    val bytes3 = Message.encode(RegResponse(new EntityID(12),5))
    println(BitVector(bytes3))

    val msg3 = Message.decode(bytes3)
    println(msg3)
  }
}