import scala.annotation.tailrec

object app extends App {
    println("Exercise 1")
    Ex1.main(Array())
    println("\n")
    println("Exercise 2")
    Ex2.main(Array())
    println("\n")
    println("Exercise 3")
    Ex3.main(Array())
}

object Ex1 extends App {
    println("factorialA: " + factorialA(5))
    println("factorialB: " + factorialB(5))
    println("factorialC: " + factorialC(5))
    def factorialA(n:Int):Int = {
        n match {
            case 0 => 1
            case x => x * factorialA(x - 1)
        }
    }

    def factorialB(n:Int):Int = if (n == 0) 1 else n * factorialB(n - 1)

    def factorialC(n:Int):Int = {
        @tailrec def loop(acc: Int, n:Int):Int = {
            if (n == 0) acc else loop(acc * n, n - 1)
        }
        loop(1,n)
    }

}

object Ex2 extends App {
    def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
        println(lo)
        if(lo >= hi) 
            LazyList.empty
        else 
            lo #:: lazyListRange(lo + 1, hi)
    }
}
object Ex3 {
    def sumList(lst1: List[Int], lst2: List[Int]):List[Int] = {
        (lst1, lst2) match {
            case (Nil, Nil) => Nil
            case (l1, l2) if (l1.size != l2.size) => throw new IllegalArgumentException("The number of elements in lists must be equal.")
            case (h1::t1, h2::t2) => (h1 + h2) :: sumList(t1, t2)
        }
    }
    // zipWith(List(1,2,3),List(4,5,6),(x:Int,y:Int) => x + y)
    // zipWith(List(1.0,2.0,3.0),List(4.0,5.0,6.0),(x:Double,y:Double) => x * y)
    def zipWith[A](lst1: List[A], lst2: List[A], f: (A, A) => A):List[A] = {
        (lst1, lst2) match {
            case (Nil, Nil) => Nil
            case (l1, l2) if (l1.size != l2.size) => throw new IllegalArgumentException("The number of elements in lists must be equal.")
            case (h1::t1, h2::t2) => f(h1, h2) :: zipWith(t1, t2, f)
        }
    }
}