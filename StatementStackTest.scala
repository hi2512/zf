

object StatementStackTest extends ZombieDSL {
  def main(args: Array[String]): Unit = {
    "ghost1" is Ghost
    summon
      say("Summoned Ghost with long statement stack")
      remember(15)
        say ("ghost1")
        remember start "ghost1" moan 1 moan 1 moan 1 moan 1 moan 1 moan 1 turn() moan 1 moan 1 moan 1 rend() moan 1 moan 1 moan 1 moan 1 moan 1 moan 1 moan 1 moan 1
        say ("ghost1")
    animate
  }
}