
import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap

abstract class EntityObject

 abstract class Entity {
   var name : String = ""
   var memInt : Integer = 0
   var prog : ListBuffer[TaskElement] = new ListBuffer[TaskElement]()

 }

 
 case object Zombie extends EntityObject
 case object Ghost extends EntityObject
 case object Vampire extends EntityObject
 
 case class ZombieEntity() extends Entity
 case class GhostEntity() extends Entity
 case class VampireEntity() extends Entity

