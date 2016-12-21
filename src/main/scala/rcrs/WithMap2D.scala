package rcrs

import java.io._

import mpmens.concerns.map2d.{Position, Map2D => Map2DTrait}
import rescuecore2.config.Config
import rescuecore2.standard.entities.{Area, StandardEntity, StandardWorldModel}
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable

private object Map2D {
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
}

trait WithMap2D extends IScalaAgent {
  this: ScalaAgent =>

  override protected def postConnect(): Unit = {
    super.postConnect()

    map.populate()
  }

  protected object map extends Map2DTrait {
    val areaIdToNode = mutable.Map.empty[EntityID, Node]
    val nodeToArea = mutable.Map.empty[Node, StandardEntity]

    def toNode(areaId: EntityID) = areaIdToNode(areaId)
    def toArea(node: Node) = nodeToArea(node)

    def currentNode = areaIdToNode(currentAreaId)

    val lineOfSight = mutable.Map.empty[Node, Set[Node]]

    def populate(): Unit = {
      Map2D.initialize(config, model)

      for (entity <- model.asScala) {
        entity match {
          case area: Area =>
            val node = map.addNode(Position(area.getX, area.getY))
            map.areaIdToNode += (area.getID -> node)
            map.nodeToArea += (node -> area)
          case _ =>
        }
      }

      for (entity <- model.asScala) {
        entity match {
          case area: Area =>
            val areaNode = map.areaIdToNode(area.getID)
            val areaPosition = areaNode.center

            for (neighborId <- area.getNeighbours asScala) {
              val neighbor = model.getEntity(neighborId)
              neighbor match {
                case neighborArea: Area =>
                  val neighborNode = map.areaIdToNode(neighborId)
                  val neighborPosition = neighborNode.center
                  map.addDirectedEdge(areaNode, neighborNode, areaPosition.distanceTo(neighborPosition))

                case _ =>
              }
            }

          case _ =>
        }
      }

      lineOfSight ++= Map2D.lineOfSight.map { case (area, areasInSight) => ( toNode(area) -> areasInSight.map(toNode)) }
    }

    def shortestPath(source: Node): ShortestPath =
      new ShortestPath(source)

    def areaExploration(origin: Node, toExplore: Set[Node]): AreaExploration =
      new AreaExploration(origin, toExplore, lineOfSight)
  }
}
