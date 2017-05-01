

object RendTest extends ZombieDSL {
  def main(args: Array[String]): Unit = {
    "tom1" is Zombie
    summon
    
      remember(100)
      shamble
        say ("tom1")
        remember start "tom1" rend() moan "tom1" moan 2
      until remembering "tom1" is 0
    animate
    
  }
}