

object TurnTest extends ZombieDSL {
  def main(args: Array[String]): Unit = {
    "zombie1" is Zombie
    summon
      say("Summoned Zombie with shamble and turn")
      remember(15)
      shamble
        say ("zombie1")
        remember start "zombie1" moan "zombie1" turn() moan 1
      until remembering "zombie1" is 0
    animate
    
  }
}