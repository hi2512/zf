

object InvokeTest extends ZombieDSL {
  def main(args: Array[String]): Unit = {
    "vampire1" is Vampire
    summon
      say("Summoned Vampire with shamble and turn and invoke")
      remember(15)
      shamble
        say ("vampire1")
        remember start "vampire1" moan "vampire1" turn() moan 1
      until remembering "vampire1" is 0
      invoke ("vampire1")
    animate
    
  }
}