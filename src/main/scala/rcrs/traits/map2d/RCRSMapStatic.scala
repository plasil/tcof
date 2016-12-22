package rcrs.traits.map2d

import java.io._

import mpmens.traits.map2d.Position
import rcrs.LineOfSight
import rescuecore2.config.Config
import rescuecore2.standard.entities.{Area, StandardWorldModel}
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable


object RCRSMapStatic {
  private var initialized = false

  var lineOfSight = mutable.Map.empty[EntityID, Set[EntityID]]

  def initialize(config: Config, model: StandardWorldModel): Unit = {
    val precomputeFileName = "precompute.data"

    if (!initialized) {
      // try to load map from file
      var input: ObjectInputStream = null
      try {
        input = new ObjectInputStream(new FileInputStream(precomputeFileName))
        lineOfSight = loadLineOfSight(input)
        println(s"Loaded precomputed data from '${precomputeFileName}'")
        initialized = true
      } catch {
        case e: FileNotFoundException => println(s"File with precomputed data '${precomputeFileName}' not found")
      } finally {
        if (input != null) {
          input.close()
        }
      }
    }

    if (!initialized) {
      val modelIterable = model.asScala

      val entityCount = modelIterable.size
      var entityIndex = 0

      println("Computation of lines of sight...")
      for (entity <- modelIterable) {
        entity match {
          case area: Area =>
            val los = new LineOfSight(config, model)
            val visibleEntities = los.getVisibleEntities(Position(area.getX, area.getY)).asScala.collect{ case visibleArea: Area => visibleArea.getID }.toSet
            lineOfSight += (area.getID -> visibleEntities)
          case _ =>
        }

        entityIndex = entityIndex + 1

        if (entityIndex % 100 == 0) {
          println(s"  $entityIndex / $entityCount")
        }
      }

      // try to save map to file
      var output: ObjectOutputStream = null
      try {
        val output = new ObjectOutputStream(new FileOutputStream(precomputeFileName))
        saveLineOfSight(output)
        println(s"Saved precomputed data to \'${precomputeFileName}\'")
      } finally {
        if (output != null) {
          output.close
        }
      }

      println(s"  finished")
      initialized = true
    }
  }

  private def loadLineOfSight(objectInputStream: ObjectInputStream): mutable.Map[EntityID, Set[EntityID]] = {
    val o = objectInputStream.readObject
    val loadedMap = o.asInstanceOf[mutable.Map[Int, Set[Int]]]

    // transforms Map[Int, Set[Int]] to Map[EntityID, Set[EntityID]]
    return loadedMap.map{case (key: Int, value: Set[Int]) => (new EntityID(key), value.map(valueID => new EntityID(valueID)))}
  }

  private def saveLineOfSight(objectOutputStream: ObjectOutputStream): Unit = {
    // transforms Map[EntityID, Set[EntityID]] to Map[Int, Set[Int]]
    val mapToSave = lineOfSight.map{case (key: EntityID, value: Set[EntityID]) => (key.getValue, value.map(valueID => valueID.getValue))}

    objectOutputStream.writeObject(mapToSave)
  }

  def coordinatesToArea(x: Int, y: Int, model: StandardWorldModel): Option[Area] = {
    model.getObjectsInRectangle(x, y, x, y).asScala
      .filter(_.isInstanceOf[Area])
      .map(_.asInstanceOf[Area])
      .headOption
  }

  /**
    * Removes consecutive duplicates.
    *
    * @param l list to be iterated
    * @tparam A type of list
    * @return new list with removed consecutive duplicates
    */
  def compress[A](l: List[A]):List[A] = l.foldRight(List[A]()) {
    case (e, ls) if (ls.isEmpty || ls.head != e) => e::ls
    case (e, ls) => ls
  }

  /**
    * Returns index of last visited item in "path" when
    * "hist" is the list of traversed nodes.
    */
  def indexOfBasedOnHist[T](path: Seq[T], hist: Seq[T]): Option[Int] = {
    if (path.length == 0 || hist.length == 0) {
      return None
    }

    if (path(0) == hist(0)) {

      // same value - search tails
      val res = indexOfBasedOnHist(path.tail, hist.tail)
      res match {
        case Some(n) => return Some(n + 1)
        case None => return Some(0)
      }
    }

    // skip current from path (history may miss some points)
    val pathRes = indexOfBasedOnHist(path.tail, hist)

    // hist may contain some point not planned - skip it
    val histRes = indexOfBasedOnHist(path, hist.tail)

    if (pathRes.isEmpty && histRes.isEmpty) {
      return None
    }

    // at least one of the res1 or res2 is defined
    pathRes match {
      case Some(pathIndexFromRec) =>
        // res1 defined - add +1 as we are moving in path
        val pathNewResult = pathIndexFromRec + 1
        histRes match {
          case Some(histIndexFromRec) =>
            val max = Math.max(pathNewResult, histIndexFromRec)
            return Some(max)
          case None =>
            return Some(pathNewResult)
        }
      case None =>
        return histRes
    }
  }
}
