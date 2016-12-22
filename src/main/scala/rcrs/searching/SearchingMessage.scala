package rcrs.searching

import rescuecore2.misc.EncodingTools
import rescuecore2.misc.geometry.spatialindex.RectangleRegion
import rescuecore2.worldmodel.EntityID

object Kind extends Enumeration {
  type Kind = Value

  /**
    * Confirming to agent - content = EntityID of agent to which the confirmation belongs
    */
  val ACK = Value

  /**
    * Announcing myself - content = empty
    */
  val ME = Value

  /**
    * Info about a building on fire - content = EntityID of the building
    */
  val ONFIRE = Value

  /**
    * Message to agent to explore a given area - content =  RectangleRegion
    */
  val EXPL = Value

  /**
    * Message to agent to extinguish a fire - content = EntityID of the building
    */
  val EXTI = Value
}

import Kind._

class SearchingMessage private (_kind: Kind, _content: AnyRef) {

  private def this(kind: Kind) {
    this(kind, "")
  }

  def kind = this._kind
  def content = this._content

  def getBytes: Array[Byte] = {
    val skind: Array[Byte] = _kind.toString.getBytes
    var scontent: Array[Byte] = null

    _kind match {
      case ACK | ONFIRE | EXTI =>
        scontent = SearchingMessage.intToByteArray((_content.asInstanceOf[EntityID]).getValue)
      case EXPL =>
        scontent = new Array[Byte](4 * 8)
        val r: RectangleRegion = _content.asInstanceOf[RectangleRegion]
        EncodingTools.writeDouble(r.getXMin, scontent, 0)
        EncodingTools.writeDouble(r.getXMax, scontent, 8)
        EncodingTools.writeDouble(r.getYMin, scontent, 16)
        EncodingTools.writeDouble(r.getYMax, scontent, 24)
      case _ =>
        scontent = (_content.asInstanceOf[String]).getBytes
    }
    val size: Int = skind.length + scontent.length + 1
    val ret: Array[Byte] = new Array[Byte](size)
    ret(0) = skind.length.toByte // WARNING: works for short kinds only !!!!!
    System.arraycopy(skind, 0, ret, 1, skind.length)
    System.arraycopy(scontent, 0, ret, skind.length + 1, scontent.length)
    return ret
  }

}

object SearchingMessage {

  private def create(bytes: Array[Byte]): SearchingMessage = {
    val ln = bytes(0)
    val s = new String(bytes, 1, ln)
    val localKind = Kind.withName(s)
    var localContent: AnyRef = null

    localKind match {
      case ACK | ONFIRE | EXTI =>
        localContent = new EntityID(SearchingMessage.byteArrayToInt(bytes, 1 + ln))
      case EXPL =>
        localContent = new RectangleRegion(
          EncodingTools.readDouble(bytes, 1 + ln),
          EncodingTools.readDouble(bytes, 9 + ln),
          EncodingTools.readDouble(bytes, 17 + ln),
          EncodingTools.readDouble(bytes, 25 + ln)
        )
      case _ =>
        localContent = new String(bytes, 1 + ln, bytes.length - ln - 1);
    }

    return new SearchingMessage(localKind, localContent)
  }

  def fromBytes(bytes: Array[Byte]): SearchingMessage = {
    return create(bytes)
  }

  def newME: SearchingMessage = {
    return new SearchingMessage(Kind.ME)
  }

  def newACK(to: EntityID): SearchingMessage = {
    return new SearchingMessage(Kind.ACK, to)
  }

  def newONFIRE(building: EntityID): SearchingMessage = {
    return new SearchingMessage(Kind.ONFIRE, building)
  }

  def newEXPL(r: RectangleRegion): SearchingMessage = {
    return new SearchingMessage(Kind.EXPL, r)
  }

  def intToByteArray(a: Int): Array[Byte] = {
    return Array[Byte](
      ((a >> 24) & 0xFF).toByte,
      ((a >> 16) & 0xFF).toByte,
      ((a >> 8) & 0xFF).toByte,
      (a & 0xFF).toByte
    )
  }

  def byteArrayToInt(b: Array[Byte], startIndex: Int): Int = {
    (b(startIndex + 3) & 0xFF) |
    ((b(startIndex + 2) & 0xFF) << 8) |
    ((b(startIndex + 1) & 0xFF) << 16) |
    ((b(startIndex + 0) & 0xFF) << 24)
  }
}