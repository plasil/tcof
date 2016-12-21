package rcrs

import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Building, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  override protected def postConnect() {
    super.postConnect()

    Logger.info(s"Central agent connected")
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"CentralAgent: Think called at time $time")

    // val toExplore = map.nodes.filter(node => node.center.x >= leftBottom.x && node.center.y >= leftBottom.y && node.center.x < rightTop.x && node.center.y <= rightTop.y)

    /*
    object RescueScenario extends Universe {

      abstract class MobileUnit(var position: Position) extends Component with PositionAware
      case class FireBrigade(_position: Position) extends MobileUnit(_position)
      case class AmbulanceTeam(_position: Position) extends MobileUnit(_position)
      case class PoliceForce(_position: Position) extends MobileUnit(_position)

      case class MapZone

      class ExplorationAssignment(val incident: Incident) extends Ensemble("Incident Response Team for " + incident) {
        val ambulances = role("ambulances", components.withRole[Ambulance])

        membership(
          ambulances.cardinality >= 1 &&
            ambulances.all(_.position.distanceTo(incident.position) <= 5)
        )

        utility = ambulances.sum(100 - _.position.distanceTo(incident.position).round.toInt)
      }

      systems {
        val rescueTeams = ensembles("rescueTeams", components.withRole[Incident].map(new IncidentResponseTeam(_)))

        membership(
          rescueTeams.map(_.ambulances).allDisjoint
        )
      }
    }
*/

  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_STATION, StandardEntityURN.AMBULANCE_CENTRE, StandardEntityURN.POLICE_OFFICE)
}
