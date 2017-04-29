
import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

  abstract class EntityType {
   var name : String = ""
   var memInt : Integer = 0
   var prog : ListBuffer[TaskElement] = new ListBuffer[TaskElement]
 
 }
 
 //class Zombie extends EntityType 
 
 case object Zombie extends EntityType
 
 case object Ghost extends EntityType
 case object Vampire extends EntityType
 case object Demon extends EntityType
 case object Djinn extends EntityType
