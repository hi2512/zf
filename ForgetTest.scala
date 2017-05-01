

object ForgetTest extends ZombieDSL {
  def main(args: Array[String]): Unit = {
    "ghost1" is Ghost
    summon
      say("Summoned Ghost with shamble and turn")
      remember(15)
        say ("ghost1")
        forget("ghost1")
        say ("ghost1")
    animate
  }
}