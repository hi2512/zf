
import scala.collection.mutable
import scala.collection.mutable.MutableList

  abstract class EntityType {
   var name : String = ""
   var task = new MutableList[Unit]
   var memInt : Integer = 0
   var memString : String = ""
 
 }
 
 //class Zombie extends EntityType 
 
 case object Zombie extends EntityType
 
 case object Ghost extends EntityType
 case object Vampire extends EntityType
 case object Demon extends EntityType
 case object Djinn extends EntityType
