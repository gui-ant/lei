object Ex3 {
    /*
    a) Define a method that calculates the result (Int) of the following exponentiation x^y
    without resorting to predefined functions.
    */
    def a(x:Int, y:Int):Int = if (y == 0)1 else  x * a(x, y-1)
    /*
    b) Define a method that receives a Int List and constructs a pair with the first and the last
    element of the list.
    */
    def b(l:List[Int]):(Int, Int) =  {
        l.length match {
            case 0 => (0, 0)
            case 1 => (l(0), l(0))
            case 2 => (l(0), l(1))
            case _ => b(l.head::l.tail.tail)
        }
    }

    /*
    c) Define a method that, given a Int list, gives the pair with that list and its length.
    */

    def c(l:List[Int]):(List[Int], Int) =  {
        def loop(l:List[Int], x:Int):Int = l match {
            case Nil => x
            case _ => loop(l.tail, x + 1)
        }
        (l, loop(l, 0))
    }

    /*
    d) Define a method that, given a Double list, calculates its average. The pre-defined
    function tail of List might be useful.
    */
    def d(l:List[Double]):Double =  {
        def avg(l:List[Double], x:Int, acc:Double):Double = l match {
            case Nil => if(x == 0)0 else acc / x 
            case _ => avg(l.tail, x + 1, acc + l(0))
        }
        avg(l, 0, 0)
    }
}