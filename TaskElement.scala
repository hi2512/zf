
import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

class TaskElement(tasksList: ListBuffer[TaskType], stackList: ListBuffer[Object]) {

  var tasks = tasksList
  var stack = stackList
}

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
case class UNTIL(a : Int) extends TaskType
case class AROUND(a: Int) extends TaskType
case object STUMBLE extends TaskType
case object INVOKE extends TaskType


trait taskStatement