package rcrs

import mpmens.concerns.map2d.{Node, Position, Map2D => Map2DConcern}
import rescuecore2.config.Config
import rescuecore2.standard.entities.{Area, StandardEntity, StandardWorldModel}
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable

private object Map2D {
  private var initialized = false

  val lineOfSight = mutable.Map.empty[EntityID, Set[EntityID]]

  def initialize(config: Config, model: StandardWorldModel): Unit = {
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

      println(s"  finished")

      initialized = true
    }
  }
}

trait WithMap2D extends IScalaAgent {
  this: ScalaAgent =>

  override protected def postConnect(): Unit = {
    super.postConnect()

    map.populate()
  }

  protected object map extends Map2DConcern {
    val areaIdToNode = mutable.Map.empty[EntityID, Node]
    val nodeToArea = mutable.Map.empty[Node, StandardEntity]

    def toNode(areaId: EntityID) = areaIdToNode(areaId)
    def toArea(node: Node) = nodeToArea(node)

    def currentNode = areaIdToNode(currentAreaId)

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
            for (neighborId <- area.getNeighbours asScala) {
              map.addDirectedEdge(map.areaIdToNode(area.getID), map.areaIdToNode(neighborId))
            }
          case _ =>
        }
      }
    }

    def getExplorePath(source: Node, lt: Position, rb: Position): List[Node] =
      getExplorePath(source, lt, rb, node => Map2D.lineOfSight(toArea(node).getID).map(toNode(_)))
  }
}
