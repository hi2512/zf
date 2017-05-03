

object EntitiesTest extends ZombieDSL {
  def main(args: Array[String]): Unit = 
  {
   
    
    "ghost1" is Ghost
    summon
      say("Summoned Ghost. Saying 1-15.")
      remember(15)
      say ("G1")
      say ("G2")
      say ("G3")
      say ("G4")
      say ("G5")
      say ("G6")
      say ("G7")
      say ("G8")
      say ("G9")
      say ("G10")
      say ("G11")
      say ("G12")
      say ("G13")
      say ("G14")
      say ("G15")
    animate
    
    Thread.sleep(8000L)

    "vampire1" is Vampire
    summon
      say("Summoned Vampire with shamble")
      remember(15)
      shamble
        say ("vampire1")
        remember start "vampire1" moan "vampire1" moan -1
      until remembering "vampire1" is 0
    animate
    "vampire2" is Vampire
    
    summon
      say("Summoned Vampire. Saying 1-15.")
      remember(15)
      say ("V1")
      say ("V2")
      say ("V3")
      say ("V4")
      say ("V5")
      say ("V6")
      say ("V7")
      say ("V8")
      say ("V9")
      say ("V10")
      say ("V11")
      say ("V12")
      say ("V13")
      say ("V14")
      say ("V15")
    animate
    
    
    "zombie1" is Zombie
    summon
      say("Summoned Zombie with shamble")
      remember(15)
      shamble
        say ("zombie1")
        remember start "zombie1" moan "zombie1" moan -1
      until remembering "zombie1" is 0
    animate
    
  }
}