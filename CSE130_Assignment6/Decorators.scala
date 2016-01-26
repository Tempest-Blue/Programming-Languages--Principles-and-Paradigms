object Decorators {
    
  object profile { 
    
    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) = 
      cm(name)
   
    def reset(name: String) = 
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }
 
  object trace {
    
      private var nestingLevel: Int = 0// You may add more fields here
    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here

      def apply (x: A): B = {
        for (i <- 1 to nestingLevel) {print("| ")}
        println(",- " + name + "(" + x.toString + ")")
        val originalNestingLevel =  nestingLevel
        nestingLevel += 1
        val result = try {f(x)}
        catch {
          case exception : Throwable => nestingLevel -= 1
                    throw exception
        }
        nestingLevel -= 1
        for (i <- 1 to originalNestingLevel) {print("| ")}
        println("`- " + result.toString)
        result
      }
    }
  } 
  
  
  object memo {
      // You may add more fields here
    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
      private var cache:Map[A,Either[B,Throwable]] = Map()// You may add more fields here
      def apply (x: A): B = {
        cache.get(x) match {
          case Some(Left(result))     => result
          case Some(Right(exception)) => throw exception
          case None                   => val result = try {f(x)}
                                         catch {
                                          case exception : Throwable => cache += (x -> Right(exception))
                                                            throw exception
                                         }
                                         cache += (x -> Left(result))
                                         result
        }
      }
    }
  }
}


