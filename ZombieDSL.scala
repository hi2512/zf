

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

import scala.language.implicitConversions
import scala.language.dynamics



class ZombieDSL extends App {
  
   var entities = new mutable.HashMap[String, EntityType]
   var tasks = new mutable.HashMap[String, MutableList[MutableList[taskStatement]]]
   var callStack = new mutable.MutableList[EntityType]
   var currentSummon = false
   var currentTask = false
   var currentLoop = false
   var currentTaskStatement : taskStatement = new rememberTask("", new MutableList[Object])
   var currentTaskName : String = ""
   var currentTaskObject : Object = remember
   
   var taskElement = new MutableList[MutableList[taskStatement]]
   //for remember
   var statementStack : MutableList[Object] = new MutableList[Object]
   
   var taskStatementStack : MutableList[taskStatement] = new MutableList[taskStatement]
   var currentEntity : EntityType = Zombie
  
  
  implicit class EntityName(s : String) {
    
    def is (e : EntityType) {
      if(currentSummon) {
        throw new RuntimeException("");
      }
      entities.put(s, e)
      currentEntity = e
      currentEntity.name = s
      currentSummon = true
    }
  }
   
   def summon {
     if(!currentSummon) {
       throw new RuntimeException("");
     }
     currentTask = true
   }
   
   object task {
     if(!currentSummon) {
       throw new RuntimeException("");
     }
     currentTask = true
     def apply (taskName : String) = {
       currentTaskName = taskName
       taskElement = new MutableList[MutableList[taskStatement]]
     }
   }
   
   def animate {
     if(!currentTask) {
       throw new RuntimeException("");
     }
     currentEntity match {
       case Zombie => 
       case _ => throw new RuntimeException("");
       
     }
   
     
   }
   
   def shamble {
     
     def apply(entityName : String) {
       
     }
     
   }
   
   def stumble {
     
   }
   
   def until {
     
   }
   
   def bind {
     
   }
   
   def disturb {
     
   }
   
   
   object remember {
     
     def start(entityName : String) = {
       //currentRemember = true
       currentTaskObject = this
       //is remember the only thing that uses the task stack??
       statementStack = new MutableList[Object]
       taskStatementStack = new MutableList[taskStatement]
       currentTaskStatement = new rememberTask(entityName, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       TaskGetter 
     }
     
     def start = {
       currentTaskObject = this
       statementStack = new MutableList[Object]
       taskStatementStack = new MutableList[taskStatement]
       currentTaskStatement = new rememberTask(currentEntity.name, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       TaskGetter
     }
      
     def apply(num : Integer) = {
       currentTaskObject = this
       //currentEntity.memInt = num
       statementStack = new MutableList[Object]
       statementStack.+=(num)
       taskStatementStack = new MutableList[taskStatement]
       currentTaskStatement = new rememberTask(currentEntity.name, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       TaskGetter
     }
     
   }
   
   trait taskStatement
   
   class rememberTask (entityName : String, stack : MutableList[Object]) extends taskStatement {
     
     def this(num : Integer, stack : MutableList[Object]) {
        this(currentEntity.name, stack) 
     }
     
     def apply {
       var sum = 0
       var intStack = new MutableList[Integer]
       for(obj <- stack if obj.isInstanceOf[Integer]) {
         intStack.+=(obj.asInstanceOf[Integer])
       }
      
       for(i <- intStack) {
         sum += i
       }
       entities(entityName).memInt = sum
     }
     
   }
   
   object moan  {
     
     
     def apply(num : Integer) = {
       currentTaskObject = this
       statementStack = new MutableList[Object]
       //statementStack.+=(num)
       currentTaskStatement = new moanTask(currentEntity.name, num, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       TaskGetter
     }
     
     
     def start (entityName : String) = {
       currentTaskObject = this
       statementStack = new MutableList[Object]
       //statementStack.+=(entities(entityName).memInt)
       currentTaskStatement = new moanTask(entityName, entities(entityName).memInt, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       TaskGetter
     }
     
   }
   
   class moanTask(moaner : String, moanVal : Integer, stack : MutableList[Object]) extends taskStatement {
     
     def this(moaner: String, stack : MutableList[Object]) {
       this(moaner, 0, stack)
     }
     
     def apply {
       stack.+=(moanVal)
     }
     
     
   }
   
   object TaskGetter {
     
       def apply = {
         /*
         //finish statement stack
         currentTaskObject match {
           case remember =>
           case moan =>
           case say =>
           case _ =>

         }
         */
           //sum ints in the statement stack
           //val res = 0
           //val x = statementStack.foreach(res += match


       }
     
       def moan(entityName : String) = {
         statementStack.+=(entities(entityName).memInt)
         TaskGetter
       }
       
       def moan(num : Integer) = {
         statementStack.+=(num)
         TaskGetter
       }
     
       /*
        * can this be supported??
       def remember(entityName : String) = {
       
         TaskGetter
       }
     		*/
     
       def say(something : String) = {
       
         TaskGetter
       }
     }
   
   object say {
     def apply(something : String) = {
       
     }
   }
   
   class sayStatement(s : String) extends taskStatement {
     
   }
 

  

  "tom" is Zombie
  summon
  task ("SayHello")
  moan
  moan start "tom" moan 5 moan "tom"
  say ("sorry")
  remember start "tom" moan 5
  say ("Hello World")
  animate
  animate

  
}

 

