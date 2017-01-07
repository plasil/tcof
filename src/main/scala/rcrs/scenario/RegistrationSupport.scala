package rcrs.scenario

import rcrs.comm.{Constants, Message, RegRequest, RegResponse}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.time.CurrentTimeTrait
import rescuecore2.log.Logger

trait RegistrationSupport {
  this: RCRSConnectorTrait with CurrentTimeTrait =>

  /**
    * Obtains a shortId from the central server.
    */
  trait Registration {
    this: MobileUnitComponent#MobileUnit =>

    private var shortId = -1

    val Register = State

    preActions {
      sensing.messages.foreach{
        case RegResponse(id, sId) if id == agent.getID =>
          shortId = sId
          Logger.info(s"Agent registered id: $id, shortId: $sId")

        case _ =>
      }
    }

    constraints(
      Register <-> (shortId == -1)
    )

    actions {
      states.selectedMembers.foreach {
        case Register => agent.sendSpeak(time, Constants.TO_STATION, Message.encode(new RegRequest()))
        case _ =>
      }
    }
  }
}
