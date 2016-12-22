package rcrs.traits.map2d

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jiri on 12/22/16.
  */
class IndexOfBasedOnHistTest extends FlatSpec with Matchers {
  "indexOfBasedOnHist" should "find last index in path according to history" in {
    val path = List("A", "B", "C", "B", "D",           "B")
    val hist = List("A",      "C", "B", "D", "G", "H", "B")

    RCRSMapStatic.indexOfBasedOnHist(path, hist) should be (Some(5))
  }

  it should "return None on empty hist" in {
    val path = List("A", "B", "C", "B", "D",           "B")
    val hist = List()

    RCRSMapStatic.indexOfBasedOnHist(path, hist) should be (None)
  }

  it should "return None on empty path" in {
    val path = List()
    val hist = List("A", "B", "C")

    RCRSMapStatic.indexOfBasedOnHist(path, hist) should be (None)
  }

  it should "return last index if path and hist are same" in {
    val path = List("A", "B", "C")
    val hist = List("A", "B", "C")

    RCRSMapStatic.indexOfBasedOnHist(path, hist) should be (Some(2))
  }

}
