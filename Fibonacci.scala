

object Fibonacci extends ZombieDSL {
  def main(args: Array[String]): Unit = {
    "tom1" is Zombie
    summon
    remember(1)
    animate

    "tom2" is Zombie
    summon
    remember(1)
    animate

    "FibonacciZombie" is Zombie
    summon
    remember(0)
    task("SayFibonaccis")
    shamble
    say("tom1")
    say("tom2")
    remember start "tom1" moan "tom1" moan "tom2"
    remember start "tom2" moan "tom1" moan "tom2"
    remember start "FibonacciZombie" moan 1 moan "FibonacciZombie"
    until remembering "FibonacciZombie" is 5
    animate
    animate
  }
}