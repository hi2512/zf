

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

import scala.language.implicitConversions
import scala.language.dynamics

object ZombieDSL extends App {

  var entities = new mutable.HashMap[String, EntityType]
  var callStack = new mutable.MutableList[EntityType]

  var currentSummon = false
  var currentTask = false
  var currentLoop = false

  var currentProg = new MutableList[TaskElement]

  var currentTaskName: String = ""
  var currentTaskObject: Object = noTask

  var currentTaskElement = new TaskElement(new MutableList[TaskType], new MutableList[Object])
  //for remember
  var taskStack: MutableList[TaskType] = new MutableList[TaskType]
  var statementStack: MutableList[Object] = new MutableList[Object]

  var currentEntity: EntityType = Zombie

  //for loop
  var shambleCount: Int = 0

  //var taskStatementStack: MutableList[taskStatement] = new MutableList[taskStatement]
  //var currentTaskStatement: taskStatement = new rememberTask("", new MutableList[Int])
  // var tasks = new mutable.HashMap[String, MutableList[MutableList[taskStatement]]]
  //var rememberEntity = ""
  
  class EntityThread(entity : EntityType) extends Runnable {
    
    def run {
      for(t <- entity.prog) {
          
      }
      
    }
    
  }

  implicit class EntityName(s: String) {

    def is(e: EntityType) {
      if (currentSummon) {
        throw new RuntimeException("");
      }
      entities.put(s, e)
      currentEntity = e
      currentEntity.name = s
      currentSummon = true
    }
  }

  def summon {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    currentTask = true
    currentProg = new MutableList[TaskElement]
  }

  object task {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    currentTask = true
    def apply(taskName: String) = {
      currentTaskName = taskName

    }
  }

  def animate {

    if (!currentTask && !currentSummon) {
      throw new RuntimeException("");
    }

    //finishing a task
    if (currentTask) {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }
      currentTaskObject = noTask
      currentTask = false
      //tasks(currentTaskName) = taskElement
    } else {
      //finish summon
      currentEntity match {
        case Zombie => currentSummon = false
        case _      => throw new RuntimeException("");
      }
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }
      currentTaskObject = noTask
      //add to running entities
      callStack.+=(currentEntity)
    }

  }

  def shamble {

    def apply {
      if (!currentSummon) {
        throw new RuntimeException("");
      }
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }
      shambleCount += 1
      //put empty stack to keep even
      statementStack = new MutableList[Object]

      taskStack = new MutableList[TaskType]
      taskStack.+=(SHAMBLE)
      currentTaskElement = new TaskElement(taskStack, statementStack)
    }

  }

  object until {

    def remembering(entityName: String) = {
      if (!currentSummon || shambleCount < 1) {
        throw new RuntimeException("");
      }
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }
      shambleCount -= 1
      statementStack = new MutableList[Object]

      taskStack = new MutableList[TaskType]
      taskStack.+=(UNTIL)
      new IsGetter(statementStack)
    }

  }

  class IsGetter(stack: MutableList[Object]) {
    def is(num: Integer) = {
      stack.+=(num)
    }

    def is(entityName: String) = {
      stack.+=(entities(entityName).memInt)
    }
  }

  def around {
    if (!currentSummon || shambleCount < 1) {
      throw new RuntimeException("");
    }
    if (!currentTaskObject.equals(noTask)) {
      currentEntity.prog.+=(currentTaskElement)
    }
    shambleCount -= 1
    statementStack = new MutableList[Object]

    taskStack = new MutableList[TaskType]
    taskStack.+=(AROUND)
  }

  def stumble {

  }

  def bind {

  }

  def disturb {

  }

  object noTask

  object remember {

    def start(entityName: String) = {
      //add previous task statement to the entity's program list
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this
      //is remember the only thing that uses the task stack??
      statementStack = new MutableList[Object]
      statementStack.+=(entityName)
      taskStack = new MutableList[TaskType]
      taskStack.+=(REMEMBER)
      currentTaskElement = new TaskElement(taskStack, statementStack)
      new TaskGetter(taskStack, statementStack)
      //entities(entityName).memInt = statementStack.sum
    }

    def start = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this

      statementStack = new MutableList[Object]
      statementStack.+=(currentEntity.name)
      taskStack = new MutableList[TaskType]
      taskStack.+=(REMEMBER)
      currentTaskElement = new TaskElement(taskStack, statementStack)
      new TaskGetter(taskStack, statementStack)

    }

    def apply(num: Integer) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this

      statementStack = new MutableList[Object]
      statementStack.+=(num)
      taskStack = new MutableList[TaskType]
      taskStack.+=(REMEMBER)
      currentTaskElement = new TaskElement(taskStack, statementStack)
      new TaskGetter(taskStack, statementStack)
      //entities(currentEntity.name).memInt = statementStack.sum

      //new rememberTask(currentEntity.name, statementStack)

    }

  }

  /*
  class rememberTask(entityName: String, stack: MutableList[Object]) extends taskStatement {

    def this(num: Integer, stack: MutableList[Object]) {
      this(currentEntity.name, stack)
    }

    def apply {
      if (currentTaskObject.equals(remember) && rememberEntity != "") {
        var sum = 0
        for (a <- statementStack) {
          if (a.isInstanceOf[EntityType]) {
            sum += a.asInstanceOf[EntityType].memInt
          } else {
            sum += a.asInstanceOf[Int]
          }
        }
      }
      
      
      
      
    }

  }
*/

  object moan {

    def apply(num: Integer) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this
      statementStack = new MutableList[Object]
      //statementStack.+=(num)

      new TaskGetter(taskStack, statementStack)
    }

    def start(entityName: String) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this
      statementStack = new MutableList[Object]
      //statementStack.+=(entities(entityName).memInt)

      new TaskGetter(taskStack, statementStack)
    }

  }

  class moanTask(moaner: String, moanVal: Integer, stack: MutableList[Object]) extends taskStatement {

    def this(moaner: String, stack: MutableList[Object]) {
      this(moaner, 0, stack)
    }

    def apply {
      stack.+=(moanVal)
    }

  }

  class TaskGetter(ts: MutableList[TaskType], s: MutableList[Object]) {

    def apply = {

    }

    def moan(entityName: String) = {
      ts.+=(MOAN)
      s.+=(entities(entityName))
      new TaskGetter(ts, s)
    }

    def moan(num: Integer) = {
      ts.+=(MOAN)
      s.+=(num)
      new TaskGetter(ts, s)
    }

    def remember(entityName: String) = {
      ts.+=(REMEMBER)
      s.+=(entities(entityName))
      new TaskGetter(ts, s)
    }

    def say(something: String) = {
      ts.+=(MOAN)
      s.+=(something)
      new TaskGetter(ts, s)
    }
  }

  object say {
    def apply(something: String) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this

      statementStack = new MutableList[Object]
      statementStack.+=(currentEntity.name)
      taskStack = new MutableList[TaskType]
      taskStack.+=(SAY)
      currentTaskElement = new TaskElement(taskStack, statementStack)
      new TaskGetter(taskStack, statementStack)
    }
  }

  "tom" is Zombie

  summon
  task("SayHello")
  moan start "tom" moan 5 moan "tom"
  shamble
  remember(12)
  say("sorry\n")
  until remembering "tom" is 12
  say("tom")
  say("\n")
  remember start "tom" moan 5
  remember start "tom" moan 5 moan "tom" moan 10 say "yd"
  say("Hello World\n")
  say("tom")
  animate
  animate

}