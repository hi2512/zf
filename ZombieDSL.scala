

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer


import scala.language.implicitConversions
import scala.language.dynamics
import scala.util.Random

object ZombieDSL extends App {

  var entities = new mutable.HashMap[String, EntityType]
  var callStack = new mutable.ListBuffer[EntityType]
  var shambleStack = new Stack[Int]
  var currentSummon = false
  var currentTask = false
  var currentLoop = false

  var currentProg = new ListBuffer[TaskElement]

  var currentTaskName: String = ""
  var currentTaskObject: Object = noTask

  var currentTaskElement = new TaskElement(new ListBuffer[TaskType], new ListBuffer[Object])
  //for remember
  var taskStack: ListBuffer[TaskType] = new ListBuffer[TaskType]
  var statementStack: ListBuffer[Object] = new ListBuffer[Object]

  var currentEntity: EntityType = Zombie

  var threadStack = new MutableList[EntityThread]()
  //for loop
  var shambleCount: Int = 0

  //var taskStatementStack: ListBuffer[taskStatement] = new ListBuffer[taskStatement]
  //var currentTaskStatement: taskStatement = new rememberTask("", new MutableList[Int])
  // var tasks = new mutable.HashMap[String, MutableList[MutableList[taskStatement]]]
  //var rememberEntity = ""

  class EntityThread(entity: EntityType) extends Runnable {

    def run {
      entity match {
        case Ghost => {
          val r = Random
          var a = r.nextInt(5000)
          Thread.sleep(a.toLong,0)
        }
      }
      var j = 0

      while (j < entity.prog.size) {
        var t = entity.prog(j)
        var tasksOrdered = t.tasks.reverse
        var stackOrdered = t.stack.reverse
        var i = 0
        var stackCopy = stackOrdered.clone()
        while (i < tasksOrdered.size) {
          tasksOrdered(i) match {
            case REMEMBER => {
              rememberTask(stackCopy(i).asInstanceOf[EntityType], stackCopy, i)
              i += 1
            }
            case MOAN => {
              moanTask(stackCopy(i).asInstanceOf[Object], stackCopy, i)
              i += 1
            }
            case SAY => {
              sayTask(stackCopy(i).asInstanceOf[Object], stackCopy, i)
              i += 1
            }
            case ANIMATE => {
              animateTask(stackCopy(i).asInstanceOf[EntityType], stackCopy)
              i += 1
            }
            case DISTURB => {
              disturbTask(stackCopy(i).asInstanceOf[EntityType], stackCopy)
              i += 1
            }
            case BANISH => {
              banishTask(stackCopy(i).asInstanceOf[EntityType], stackCopy)
              i += 1
            }
            case REND => {
              rendTask(stackCopy)
              i += 1
            }
            case SHAMBLE => {
              shambleTask(stackCopy(i).asInstanceOf[EntityType], stackCopy)
              i += 1
            }
            case AROUND => {
              aroundTask(stackCopy(i).asInstanceOf[EntityType], stackCopy)
              i += 1
            }
            case UNTIL(a : Int) => {
              if(untilTask(stackCopy(i).asInstanceOf[EntityType], stackOrdered))
                i = a+1
              else
                i += 1
            }
            case TURN => {
              turnTask(stackCopy)
              i += 1
            }
            
            case TURN => {
              rendTask(stackCopy)
              i += 1
            }
            
          }

        }
        j += 1
      }

    }

  }

  def rememberTask(entity: EntityType, stack: ListBuffer[Object], index: Int) {

    var sum = 0
    var i = 0
    while (i < stack.size) {
      if (i != index) {

        if (stack(i).isInstanceOf[EntityType]) {
          sum += stack(i).asInstanceOf[EntityType].memInt
        } else if (stack(i).isInstanceOf[Int]) {
          sum += stack(i).asInstanceOf[Int]
        }

      }
      i += 1
    }

    entity.memInt = sum

  }
  def moanTask(entity: Object, stack: ListBuffer[Object], index: Int) {

    if (entity.isInstanceOf[Int]) { return }
    else if (entity.isInstanceOf[EntityType]) { stack(index) = entity.asInstanceOf[EntityType].memInt }
    else { throw new Exception("Moan received non-entity/int arg") }

  }
  def animateTask(entity: EntityType, stack: ListBuffer[Object]) {

    var sum = 0
    for (a <- statementStack) {
      if (a.isInstanceOf[EntityType]) {
        sum += a.asInstanceOf[EntityType].memInt
      } else if (a.isInstanceOf[Int]) {
        sum += a.asInstanceOf[Int]
      }
    }

  }
  def banishTask(entity: EntityType, stack: ListBuffer[Object]) {

    var sum = 0
    for (a <- statementStack) {
      if (a.isInstanceOf[EntityType]) {
        sum += a.asInstanceOf[EntityType].memInt
      } else if (a.isInstanceOf[Int]) {
        sum += a.asInstanceOf[Int]
      }
    }

  }
  def sayTask(line: Object, stack: ListBuffer[Object], index: Int) {

    if (line.isInstanceOf[String]) {
      if (!entities.contains(line.asInstanceOf[String])) {
        println(line)
      } else {
        println(entities(line.asInstanceOf[String]).memInt)
      }
    } else {
      throw new Exception("Invalid arg sent to sayTask")
    }

  }
  def disturbTask(entity: EntityType, stack: ListBuffer[Object]) {

    var sum = 0
    for (a <- statementStack) {
      if (a.isInstanceOf[EntityType]) {
        sum += a.asInstanceOf[EntityType].memInt
      } else if (a.isInstanceOf[Int]) {
        sum += a.asInstanceOf[Int]
      }
    }

  }
  def rendTask(stack: ListBuffer[Object]) {

    var a : Int = 0
    var b : Int = 0
    if(stack(0).isInstanceOf[Int]) {
      a = stack(0).asInstanceOf[Int]
    } else if(stack(0).isInstanceOf[EntityType]){
      a = stack(0).asInstanceOf[EntityType].memInt
    } else {
      throw new Exception("Invalid arg sent to rendTask")
    }

    if(stack(1).isInstanceOf[Int]) {
      b = stack(1).asInstanceOf[Int]
    } else if(stack(1).isInstanceOf[EntityType]){
      b = stack(1).asInstanceOf[EntityType].memInt
    } else {
      throw new Exception("Invalid arg sent to rendTask")
    }
    
    var c = b/a
    
    stack(0) = c.asInstanceOf[Object]
    stack(1) = 0.asInstanceOf[Object]
  }
  def shambleTask(entity: EntityType, stack: ListBuffer[Object]) {

 

  }
  
  def untilTask(entity: EntityType, stack: ListBuffer[Object]): Boolean = {
    var a = true
    
    if(stack(0).isInstanceOf[EntityType])
      a = stack(0).asInstanceOf[EntityType].memInt == entity.memInt
    else if (stack(0).isInstanceOf[Int])
      a = stack(0).asInstanceOf[Int] == entity.memInt
    else
      throw new Exception("Bad arg passed to untilTask.")
    a

  }
  def aroundTask(entity: EntityType, stack: ListBuffer[Object]) {

    var sum = 0
    for (a <- statementStack) {
      if (a.isInstanceOf[EntityType]) {
        sum += a.asInstanceOf[EntityType].memInt
      } else if (a.isInstanceOf[Int]) {
        sum += a.asInstanceOf[Int]
      }
    }

  }

  def turnTask(stack: ListBuffer[Object]) {
    if (stack(0).isInstanceOf[EntityType])
      stack(0).asInstanceOf[EntityType].memInt *= -1
    else if (stack(0).isInstanceOf[Int])
      stack(0) = (stack(0).asInstanceOf[Int] * (-1)).asInstanceOf[Object]
    else
      throw new Exception("Bad arg passed to turnTask.")
  

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
    currentProg = new ListBuffer[TaskElement]
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
      new EntityThread(currentEntity).run()
    }
    
   

  }

  def disturb {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    currentEntity match {
      case Zombie => currentSummon = false
      case Ghost => {
        currentSummon = false
        new EntityThread(currentEntity).run()
      }
      case _ => throw new RuntimeException("");
    }

  }

  def shamble {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    if (!currentTaskObject.equals(noTask)) {
      currentEntity.prog.+=(currentTaskElement)
    }
    shambleCount += 1
    //put empty stack to keep even
    shambleStack.push(currentEntity.prog.size)

    taskStack = new ListBuffer[TaskType]
    taskStack.+=(SHAMBLE)
    currentTaskElement = new TaskElement(taskStack, new ListBuffer[Object])

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
      statementStack = new ListBuffer[Object]

      taskStack = new ListBuffer[TaskType]
      var index = shambleStack.pop
      taskStack.+=(UNTIL(index))
      new IsGetter(statementStack)
    }
    
    
  }

  class IsGetter(stack: ListBuffer[Object]) {
    def is(num: Integer) = {
      stack.+=(num)
    }

    def is(entityName: String) = {
      stack.+=(entities(entityName))
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
    statementStack = new ListBuffer[Object]

    taskStack = new ListBuffer[TaskType]
    taskStack.+=(AROUND)
  }

  def stumble {

  }

  def bind {

  }


  object noTask

  object remember {

    def start(entityName: String) = {

      currentTaskObject = this
      //is remember the only thing that uses the task stack??
      statementStack = new ListBuffer[Object]
      statementStack.+=(entities(entityName))
      taskStack = new ListBuffer[TaskType]
      taskStack.+=(REMEMBER)
      
      currentEntity.prog.+=(new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)
      //entities(entityName).memInt = statementStack.sum
    }

    def start = {

      currentTaskObject = this

      statementStack = new ListBuffer[Object]
      statementStack.+=(currentEntity)
      taskStack = new ListBuffer[TaskType]
      taskStack.+=(REMEMBER)

      currentEntity.prog.+=(new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)

    }

    def apply(num: Integer) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this

      statementStack = new ListBuffer[Object]
      statementStack.append(currentEntity)
      statementStack.append(num)
      taskStack = new ListBuffer[TaskType]
      taskStack.append(REMEMBER)
      taskStack.append(MOAN)
      currentEntity.prog.+=(new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)
      //entities(currentEntity.name).memInt = statementStack.sum

      //new rememberTask(currentEntity.name, statementStack)

    }

  }

  object moan {

    def apply(num: Integer) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this
      statementStack = new ListBuffer[Object]
      //statementStack.+=(num)

      new TaskGetter(taskStack, statementStack)
    }

    def start(entityName: String) = {
      if (!currentTaskObject.equals(noTask)) {
        currentEntity.prog.+=(currentTaskElement)
      }

      currentTaskObject = this
      statementStack = new ListBuffer[Object]
      //statementStack.+=(entities(entityName).memInt)

      new TaskGetter(taskStack, statementStack)
    }

  }

  class moanTask(moaner: String, moanVal: Integer, stack: ListBuffer[Object]) extends taskStatement {

    def this(moaner: String, stack: ListBuffer[Object]) {
      this(moaner, 0, stack)
    }

    def apply {
      stack.+=(moanVal)
    }

  }

  class TaskGetter(ts: ListBuffer[TaskType], s: ListBuffer[Object]) {

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
    
    def turn() =
    {
      ts.+=(TURN)
      s.+=(null)
      new TaskGetter(ts, s)
    }
    
    def rend() =
    {
      ts.+=(REND)
      s.+=(null)
      new TaskGetter(ts, s)
    }
  }

  object say {
    def apply(something: String) = {
     
      currentTaskObject = this

      statementStack = new ListBuffer[Object]
      statementStack.+=(something)
      taskStack = new ListBuffer[TaskType]
      taskStack.+=(SAY)
      currentEntity.prog.append( new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)
    }
  }


"tom" is Ghost
summon
task ("SayHello")
    say ("tom")
	animate
disturb

}