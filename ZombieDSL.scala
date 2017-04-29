

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

import scala.language.implicitConversions
import scala.language.dynamics



object ZombieDSL extends App {
  
   var entities = new mutable.HashMap[String, EntityType]
   var tasks = new mutable.HashMap[String, MutableList[MutableList[taskStatement]]]
   var rememberEntity = ""
   var callStack = new mutable.MutableList[EntityType]
   var currentSummon = false
   var currentTask = false
   var currentLoop = false
   var currentTaskStatement : taskStatement = new rememberTask("", new MutableList[Int])
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
     
     if(!currentTask && !currentSummon) {
       throw new RuntimeException("");
     }
     
     if(currentTask) {
      if(currentTaskObject.equals(remember) && rememberEntity != "")
       {
        var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }

       }
       rememberEntity = ""
       currentTask = false
       tasks(currentTaskName) = taskElement
     } else {
       //finish summon
       currentEntity match {
         case Zombie => 
         case _ => throw new RuntimeException("");
       }
       
     }
     
   
     
   }
   class loop(tasks : MutableList[taskStatement])
   {
     
   }
   def shamble {
     
     def apply(entityName : String) 
     {
         
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
              if(currentTaskObject.equals(remember) && rememberEntity != "")  {
         var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }
       }
       //currentRemember = true
       currentTaskObject = this
       rememberEntity = entityName
       //is remember the only thing that uses the task stack??
       statementStack = new MutableList[Int]
       taskStatementStack = new MutableList[taskStatement]
       taskStatementStack.+=(currentTaskStatement)
       taskElement.+=(taskStatementStack)
       new TaskGetter (statementStack)
       //entities(entityName).memInt = statementStack.sum
     }
     
     def start = {
              if(currentTaskObject.equals(remember) && rememberEntity != "")  {
         entities(rememberEntity).memInt = statementStack.sum
       }
       rememberEntity = currentEntity.name
       currentTaskObject = this       
       statementStack = new MutableList[Int]
       taskStatementStack = new MutableList[taskStatement]
       taskStatementStack.+=(currentTaskStatement)
       taskElement.+=(taskStatementStack)
       new TaskGetter(statementStack)
       

     }
      
     def apply(num : Integer) = {
              if(currentTaskObject.equals(remember) && rememberEntity != "")  {
         var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }
       }
       rememberEntity = currentEntity.name
       currentTaskObject = this
       //currentEntity.memInt = num
       statementStack = new MutableList[Int]
       statementStack.+=(num)
       taskStatementStack = new MutableList[taskStatement]
       taskStatementStack.+=(currentTaskStatement)
       taskElement.+=(taskStatementStack)
       new TaskGetter(statementStack)
       //entities(currentEntity.name).memInt = statementStack.sum

       //new rememberTask(currentEntity.name, statementStack)
       
     }
     
   }
   
   trait taskStatement
   
   class rememberTask (entityName : String, stack : MutableList[Int]) extends taskStatement {
     
     def this(num : Integer, stack : MutableList[Int]) {
        this(currentEntity.name, stack) 
     }
     
     def apply {
       entities(entityName).memInt = stack.sum
     }
     
   }
   
   object moan  {
     
     
     def apply(num : Integer) = {
              if(currentTaskObject.equals(remember) && rememberEntity != "")  {
         var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }
       }
       currentTaskObject = this
       statementStack = new MutableList[Object]
       //statementStack.+=(num)
       currentTaskStatement = new moanTask(currentEntity.name, num, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       taskElement.+=(taskStatementStack)
       new TaskGetter(statementStack)
     }
     
     
     def start (entityName : String) = {
       if(currentTaskObject.equals(remember) && rememberEntity != "") {
         var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }
       }
       currentTaskObject = this
       statementStack = new MutableList[Int]
       //statementStack.+=(entities(entityName).memInt)
       currentTaskStatement = new moanTask(entityName, entities(entityName).memInt, statementStack)
       taskStatementStack.+=(currentTaskStatement)
       taskElement.+=(taskStatementStack)
       new TaskGetter(statementStack)
     }
     
   }
   
   class moanTask(moaner : String, moanVal : Integer, stack : MutableList[Int]) extends taskStatement {
     
     def this(moaner: String, stack : MutableList[Int]) {
       this(moaner, 0, stack)
     }
     
     def apply {
       stack.+=(moanVal)
     }
     
     
   }
   
   class TaskGetter(s : MutableList[Object]) {
     
       def apply = {
         taskElement.+=(taskStatementStack)
       }
     
       def moan(entityName : String) = {         
         s.+=(entities(entityName))
         new TaskGetter(s)
       }
       
       def moan(num : Integer) = {
         s.+=(num)
         new TaskGetter(s)
       }
     
       def remember(entityName: String) = {
         var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }
       }
      
     
       def say(something : String) = {
         print(something)
         new TaskGetter(s)
       }
     }
   
   object say {
     def apply(something : String) = {
              if(currentTaskObject.equals(remember) && rememberEntity != "")  {
         var sum = 0
          for( a <- statementStack) {
            if(a.isInstanceOf[EntityType]) {
              sum += a.asInstanceOf[EntityType].memInt
            } else {
              sum += a.asInstanceOf[Int]
            }
          }
       }
       currentTaskObject = this
       if(entities.contains(something)) {
         print(entities(something).memInt)
       } else {
         print(something)
       }
       new TaskGetter(new MutableList[Int])
     }
   }

 

  

  "tom" is Zombie

  summon
  task ("SayHello")
  moan
  moan start "tom" moan 5 moan "tom"
  remember (12)
  say ("sorry\n")
  say ("tom")
  say ("\n")
  remember start "tom" moan 5
  remember start "tom" moan 5 moan "tom" moan 10
  say ("Hello World\n")
  say ("tom")
  animate
  animate

  
}