package rcrs

import java.util.{Collection, EnumSet}

import mpmens.traits.map2d.Position
import rcrs.traits.RCRSTrait
import rescuecore2.messages.Command
import rescuecore2.standard.components.StandardAgent
import rescuecore2.standard.entities.{Area, Human, StandardEntity, StandardEntityURN}
import rescuecore2.worldmodel.{ChangeSet, EntityID}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer



abstract class ScalaAgent extends RCRSTrait {
  sagent =>

  type AgentEntityType <: StandardEntity

  trait RCRSAgentTrait extends RCRSTrait {
    override def agent = sagent
  }

  /* This is for making it possible to mix Traits with agents */
  val agent = this

  protected def postConnect(): Unit = {
    traitInit()
  }

  protected def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    rcrsTraitStep(time, changes, heard)
  }


  protected def getRequestedEntityURNs: List[StandardEntityURN]

  def currentAreaId = me match {
    case area: Area => area.getID
    case human: Human => human.getPosition
    case _ => null
  }

  def getPosition = me match {
    case area: Area => new Position(area.getX, area.getY)
    case human: Human => new Position(human.getX, human.getY)
    case _ => null
  }

  def config = rcrsAgent.delegateConfig
  def model = rcrsAgent.delegateModel
  def me = rcrsAgent.delegateMe
  def location = rcrsAgent.delegateLocation
  def sendMove(time: Int, path: List[EntityID]) = rcrsAgent.delegateSendMove(time, path)
  def sendMove(time: Int, path: List[EntityID], destX: Int, destY: Int) = rcrsAgent.delegateSendMove(time, path, destX, destY)

  var ignoreAgentCommandsUntil: Int = _

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

    def delegateSendMove(time: Int, path: List[EntityID]) = sendMove(time, ListBuffer(path: _*).asJava)
    def delegateSendMove(time: Int, path: List[EntityID], destX: Int, destY: Int) = sendMove(time, ListBuffer(path: _*).asJava, destX, destY)
  }

  val rcrsAgent = new Agent
}
