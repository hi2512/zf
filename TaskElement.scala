
import scala.collection.mutable
import scala.collection.mutable.MutableList

class TaskElement(tasks : MutableList[TaskType], stack : MutableList[Object]) 


abstract class TaskType


case object REMEMBER extends TaskType
case object SAY extends TaskType
case object REND extends TaskType
case object MOAN extends TaskType
case object TURN extends TaskType
case object ANIMATE extends TaskType
case object BANISH extends TaskType
case object DISTURB extends TaskType
case object FORGET extends TaskType
case object SHAMBLE extends TaskType
case object UNTIL extends TaskType
case object AROUND extends TaskType
case object STUMBLE extends TaskType

trait taskStatement