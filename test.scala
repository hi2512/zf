  object SymbolExample extends App {
  
  val symbols = scala.collection.mutable.HashMap.empty[Symbol, Int]

  
  implicit class Variable(s: Symbol) {
    
    def := (v : Int) {
      symbols += (s -> v)
    }
    
  }

  object PRINT {
    def apply(s: Symbol) {
      symbols.get(s) match {
        case Some(v) => println("PRINT: " + v)  
        case None => println("ERROR, VARIABLE " + s + " HAS NOT BEEN DECLARED") 
      }
    }
  }
  

  
  'x := 10
  PRINT ('x)
  PRINT ('y)
  
  
  
}