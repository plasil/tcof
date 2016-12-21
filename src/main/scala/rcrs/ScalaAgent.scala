package rcrs

import java.util.{Collection, EnumSet}

import rescuecore2.messages.Command
import rescuecore2.standard.components.StandardAgent
import rescuecore2.standard.entities.{Area, Human, StandardEntity, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

abstract class ScalaAgent extends IScalaAgent with WithMap2D {
  sagent =>

  type AgentEntityType <: StandardEntity

  protected def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit
  protected def getRequestedEntityURNs: List[StandardEntityURN]

  protected def currentAreaId = me match {
    case area: Area => area.getID
    case human: Human => human.getPosition
    case _ => null
  }

  protected def config = agent.delegateConfig
  protected def model = agent.delegateModel
  protected def me = agent.delegateMe
  protected def location = agent.delegateLocation
  protected def sendMove(time: Int, path: List[map.Node]) = agent.delegateSendMove(time, path)
  protected def sendMove(time: Int, path: List[map.Node], destX: Int, destY: Int) = agent.delegateSendMove(time, path, destX, destY)

  protected var ignoreAgentCommandsUntil: Int = _

  class Agent extends StandardAgent[AgentEntityType] {
    override def getRequestedEntityURNsEnum: EnumSet[StandardEntityURN] = EnumSet.copyOf(sagent.getRequestedEntityURNs.asJavaCollection)

    override def think(time: Int, changes: ChangeSet, heard: Collection[Command]): Unit = sagent.think(time, changes, heard.asScala.toList)

    override protected def postConnect() {
      super.postConnect()

      ignoreAgentCommandsUntil = config.getIntValue(kernel.KernelConstants.IGNORE_AGENT_COMMANDS_KEY)

      sagent.postConnect()
    }

    def delegateConfig = config
    def delegateModel = model
    def delegateMe = me
    def delegateLocation = location

    def delegateSendMove(time: Int, path: List[map.Node]) = sendMove(time, ListBuffer(path.map(map.toArea(_).getID): _*).asJava)
    def delegateSendMove(time: Int, path: List[map.Node], destX: Int, destY: Int) = sendMove(time, ListBuffer(path.map(map.toArea(_).getID): _*).asJava, destX, destY)
  }

  val agent = new Agent

}
