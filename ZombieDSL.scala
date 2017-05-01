

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

import scala.language.implicitConversions
import scala.language.dynamics
import scala.util.Random

class ZombieDSL{

  var entities = new mutable.HashMap[String, Entity]
  var callStack = new mutable.ListBuffer[Entity]
  var shambleStack = new Stack[Int]
  var currentSummon = false
  var currentTask = false
  var currentLoop = false

  var taskList: ListBuffer[TaskElement] = new ListBuffer[TaskElement]()

  var currentProg = new ListBuffer[TaskElement]

  var currentTaskName: String = ""
  var currentTaskObject: Object = noTask

  var currentTaskElement = new TaskElement(new ListBuffer[TaskType], new ListBuffer[Object])
  //for remember
  var taskStack: ListBuffer[TaskType] = new ListBuffer[TaskType]
  var statementStack: ListBuffer[Object] = new ListBuffer[Object]

  var currentEntity: Entity = new ZombieEntity()

  var threadStack = new MutableList[EntityThread]()
  //for loop
  var shambleCount: Int = 0

  //var taskStatementStack: ListBuffer[taskStatement] = new ListBuffer[taskStatement]
  //var currentTaskStatement: taskStatement = new rememberTask("", new MutableList[Int])
  // var tasks = new mutable.HashMap[String, MutableList[MutableList[taskStatement]]]
  //var rememberEntity = ""

  class EntityThread(entity: Entity) extends Runnable {

    def run {
      entity match {
        case GhostEntity() => {
          val r = Random
          var a = r.nextInt(5000)
          Thread.sleep(a.toLong, 0)
        }
        
        case VampireEntity() =>
          {
            entity.prog = Random.shuffle(entity.prog)
          }
        case ZombieEntity() => {}
      }
      
      var j = 0
      
      while (j < entity.prog.size ) {
        entity match {
        case GhostEntity() => {
          val r = Random
          var a = r.nextInt(5000)
          Thread.sleep(a.toLong, 0)
        }
        
        case VampireEntity() =>
          {
            
          }
        case ZombieEntity() => {}
      }
        var t = entity.prog(j)
        var tasksOrdered = t.tasks.reverse
        var stackOrdered = t.stack.reverse
        var i = 0
        var stackCopy = stackOrdered.clone()
        while (i < tasksOrdered.size) {
          tasksOrdered(i) match {
            case REMEMBER => {
              rememberTask(stackCopy(i).asInstanceOf[Entity], stackCopy, i)
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
              animateTask(stackCopy(i).asInstanceOf[Entity], stackCopy)
              i += 1
            }
            case BANISH => {
              banishTask(stackCopy(i).asInstanceOf[Entity], stackCopy)
              i += 1
            }
            case REND => {
              rendTask(stackCopy)
              i += 1
            }
            case SHAMBLE => {
              shambleTask()
              i += 1
            }
            case AROUND(a : Int) => {
              j = a
              i += 1
            }
            case UNTIL(a: Int) => {
              if (!untilTask(stackCopy))
                j = a
              i += 1     
           
            }
            case TURN => {
              turnTask(stackCopy)
              i += 1
            }
            
            case FORGET => {
              forgetTask(stackCopy(i).asInstanceOf[Entity])
              i += 1
            }
            
            case INVOKE =>
              {
                invokeTask(stackCopy)
                i += 1
              }

          }

        }
        j += 1
      }

    }

  }

  def rememberTask(entity: Entity, stack: ListBuffer[Object], index: Int) {

    var sum = 0
    var i = 0
    while (i < stack.size) {
      if (i != index) {

        if (stack(i).isInstanceOf[Entity]) {
          throw new Exception("Invalid arg sent to rememberTask")
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
    else if (entity.isInstanceOf[Entity]) { stack(index) = entity.asInstanceOf[Entity].memInt }
    else { throw new Exception("Moan received non-entity/int arg") }

  }
  def animateTask(entity: Entity, stack: ListBuffer[Object]) {

  }
  def banishTask(entity: Entity, stack: ListBuffer[Object]) {

    var sum = 0
    for (a <- statementStack) {
      if (a.isInstanceOf[Entity]) {
        sum += a.asInstanceOf[Entity].memInt
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

  def rendTask(stack: ListBuffer[Object]) {

    var a: Int = 0
    var b: Int = 0
    if (stack(0).isInstanceOf[Int]) {
      a = stack(0).asInstanceOf[Int]
    } else if (stack(0).isInstanceOf[Entity]) {
      a = stack(0).asInstanceOf[Entity].memInt
    } else {
      throw new Exception("Invalid arg sent to rendTask")
    }

    if (stack(1).isInstanceOf[Int]) {
      b = stack(1).asInstanceOf[Int]
    } else if (stack(1).isInstanceOf[Entity]) {
      b = stack(1).asInstanceOf[Entity].memInt
    } else {
      throw new Exception("Invalid arg sent to rendTask")
    }

    var c = b / a

    stack(0) = c.asInstanceOf[Object]
    stack(1) = 0.asInstanceOf[Object]
  }
  def shambleTask() {

  }

  def untilTask(stack: ListBuffer[Object]): Boolean = {
    var a = true
    var entity = entities(stack(1).asInstanceOf[String])
    
    if (stack(0).isInstanceOf[Entity])
      a = stack(0).asInstanceOf[Entity].memInt == entity.memInt
    else if (stack(0).isInstanceOf[Int])
      a = stack(0).asInstanceOf[Int] == entity.memInt
    else
      throw new Exception("Bad arg passed to untilTask.")
    a

  }
  def aroundTask(entity: Entity, stack: ListBuffer[Object]) {

  }

  def turnTask(stack: ListBuffer[Object]) {
    if (stack(0).isInstanceOf[Entity])
      stack(0).asInstanceOf[Entity].memInt *= -1
    else if (stack(0).isInstanceOf[Int])
      stack(0) = (stack(0).asInstanceOf[Int] * (-1)).asInstanceOf[Object]
    else
      throw new Exception("Bad arg passed to turnTask.")

  }

  def forgetTask(entity: Entity) {
    entity.memInt = 0
  }
  
    def invokeTask(stack : ListBuffer[Object]) {
      new EntityThread(stack(0).asInstanceOf[Entity]).run()
    
  }

  implicit class EntityName(s: String) {

    def is(e: EntityObject) {
      if (currentSummon) {
        throw new RuntimeException("");
      }
      
      var entity : Entity = new ZombieEntity()
      e match {
        case Zombie =>    entity = new ZombieEntity()
        case Vampire =>   entity = new VampireEntity()
        case Ghost =>     entity = new GhostEntity()
      }
      entities.put(s, entity)
      currentEntity = entity
      currentEntity.name = s
      currentSummon = true
    }
  }

  def summon {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    currentProg = new ListBuffer[TaskElement]
  }

  object task {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    currentTask = true
    def apply(taskName: String) = {
      taskList = new ListBuffer[TaskElement]()

      currentTaskName = taskName

    }
  }

  def animate {

    if (!currentTask && !currentSummon) {
      throw new RuntimeException("");
    }

    //finishing a task
    if (currentTask) {
        currentEntity.prog.append(currentTaskElement)

      currentTaskObject = noTask
      currentTask = false

      //tasks(currentTaskName) = taskElement
    } else {
      //finish summon
      currentEntity match {
        case ZombieEntity()  => currentSummon = false
        case VampireEntity() => currentSummon = false;
        case default => return;
      }
        currentEntity.prog.append(currentTaskElement)
      currentTaskObject = noTask
      //add to running entities
      callStack.append(currentEntity)
      new EntityThread(currentEntity).run()
    }

  }

  def bind {
    if (!currentTask && !currentSummon) {
      throw new RuntimeException("");
    }

    //finishing a task
    if (currentTask) {

        currentEntity.prog.append(currentTaskElement)
      
      currentTaskObject = noTask
      currentTask = false

      //tasks(currentTaskName) = taskElement
    } else {
      //finish summon
      currentEntity match {
        case ZombieEntity()  => currentSummon = false
        case default => return ;
      }

        currentEntity.prog.append(currentTaskElement)
      
      currentTaskObject = noTask
      //add to running entities
      callStack.append(currentEntity)
      new EntityThread(currentEntity).run()
      currentSummon = false
    }
  }

  def disturb {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
    currentEntity match {
      case GhostEntity() => {
        currentSummon = false
        new EntityThread(currentEntity).run()
      }
      case default => {}
    }

  }

  def shamble {
    if (!currentSummon) {
      throw new RuntimeException("");
    }
      
    shambleCount += 1
    //put empty stack to keep even
    shambleStack.push(currentEntity.prog.size)

    taskStack = new ListBuffer[TaskType]
    taskStack.append(SHAMBLE)
    
    currentTaskElement = new TaskElement(taskStack, new ListBuffer[Object])
    currentEntity.prog.append(currentTaskElement)

  }
  
  

  object until {

    def remembering(entityName: String) = {
      if (!currentSummon || shambleCount < 1) {
        throw new RuntimeException("");
      }
      
      shambleCount -= 1
      statementStack = new ListBuffer[Object]

      taskStack = new ListBuffer[TaskType]
      var index = shambleStack.pop
      taskStack.append(UNTIL(index))
      currentEntity.prog.append(new TaskElement(taskStack, statementStack))
      statementStack.append(entityName)
      new IsGetter(statementStack)
    }

  }

  class IsGetter(stack: ListBuffer[Object]) {
    def is(num: Integer) = {
      stack.append(num)
    }

    def is(entityName: String) = {
      stack.append(entities(entityName))
    }
  }

  def around {

    if (!currentSummon || shambleCount < 1) {
      throw new RuntimeException("");
    }

    shambleCount -= 1
    statementStack = new ListBuffer[Object]

    taskStack = new ListBuffer[TaskType]
    var index = shambleStack.pop
    taskStack.append(AROUND(index))
    currentEntity.prog.append(new TaskElement(taskStack, statementStack))

    new IsGetter(statementStack)


  }

  def stumble {

  }

  object noTask

  object remember {

    def start(entityName: String) = {

      currentTaskObject = this
      //is remember the only thing that uses the task stack??
      statementStack = new ListBuffer[Object]
      var a = entities
      statementStack.append(entities(entityName))
      taskStack = new ListBuffer[TaskType]
      taskStack.append(REMEMBER)

      currentEntity.prog.append(new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)
      //entities(entityName).memInt = statementStack.sum
    }

    def start = {

      currentTaskObject = this

      statementStack = new ListBuffer[Object]
      statementStack.append(currentEntity)
      taskStack = new ListBuffer[TaskType]
      taskStack.append(REMEMBER)

      currentEntity.prog.append(new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)

    }

    def apply(num: Integer) = {
        currentEntity.prog.append(currentTaskElement)


      currentTaskObject = this

      statementStack = new ListBuffer[Object]
      statementStack.append(currentEntity)
      statementStack.append(num)
      taskStack = new ListBuffer[TaskType]
      taskStack.append(REMEMBER)
      taskStack.append(MOAN)
      currentEntity.prog.append(new TaskElement(taskStack, statementStack))
      new TaskGetter(taskStack, statementStack)
      //entities(currentEntity.name).memInt = statementStack.sum

      //new rememberTask(currentEntity.name, statementStack)

    }

  }

  object moan {

    def apply(num: Integer) = {
        currentEntity.prog.append(currentTaskElement)

      currentTaskObject = this
      statementStack = new ListBuffer[Object]
      //statementStack.append(num)

      new TaskGetter(taskStack, statementStack)
    }

    def start(entityName: String) = {
        currentEntity.prog.append(currentTaskElement)
      

      currentTaskObject = this
      statementStack = new ListBuffer[Object]
      //statementStack.append(entities(entityName).memInt)

      new TaskGetter(taskStack, statementStack)
    }

  }

  class moanTask(moaner: String, moanVal: Integer, stack: ListBuffer[Object]) extends taskStatement {

    def this(moaner: String, stack: ListBuffer[Object]) {
      this(moaner, 0, stack)
    }

    def apply {
      stack.append(moanVal)
    }

  }

  class TaskGetter(ts: ListBuffer[TaskType], s: ListBuffer[Object]) {

    def apply = {

    }

    def forget(entityName: String) = {
      ts.append(FORGET)
      s.append(entities(entityName))
      new TaskGetter(ts, s)
    }
    def moan(entityName: String) = {
      ts.append(MOAN)
      s.append(entities(entityName))
      new TaskGetter(ts, s)
    }

    def moan(num: Integer) = {
      ts.append(MOAN)
      s.append(num)
      new TaskGetter(ts, s)
    }

    def remember(entityName: String) = {
      ts.append(REMEMBER)
      s.append(entities(entityName))
      new TaskGetter(ts, s)
    }

    def say(something: String) = {
      ts.append(MOAN)
      s.append(something)
      new TaskGetter(ts, s)
    }

    def turn() =
      {
        ts.append(TURN)
        s.append(null)
        new TaskGetter(ts, s)
      }

    def rend() =
      {
        ts.append(REND)
        s.append(null)
        new TaskGetter(ts, s)
      }
  }

  object say {
    def apply(something: String) = {

      currentTaskObject = this

      statementStack = new ListBuffer[Object]
      statementStack.append(something)
      taskStack = new ListBuffer[TaskType]
      taskStack.append(SAY)
      var taskElement = new TaskElement(taskStack, statementStack)

      currentEntity.prog.append(taskElement)
      if (currentTask)
        taskList.append(taskElement)

      new TaskGetter(taskStack, statementStack)
    }
  }

  object forget {
    def apply(entityName: String) = {


      statementStack = new ListBuffer[Object]
      statementStack.append(entities(entityName))
      taskStack = new ListBuffer[TaskType]
      taskStack.append(FORGET)
      var taskElement = new TaskElement(taskStack, statementStack)

      currentEntity.prog.append(taskElement)


      new TaskGetter(taskStack, statementStack)
    }
  }
  
  object invoke {
    def apply(entityName : String)
    {
      statementStack = new ListBuffer[Object]
      statementStack.append(entities(entityName))
      taskStack = new ListBuffer[TaskType]
      taskStack.append(INVOKE)
      var taskElement = new TaskElement(taskStack, statementStack)

      currentEntity.prog.append(taskElement)


      new TaskGetter(taskStack, statementStack)
    }
  }


}