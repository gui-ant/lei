import scala.annotation.tailrec

object Ex1 {

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

    def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
        println(lo)
        if(lo >= hi) 
            LazyList.empty
        else 
            lo #:: lazyListRange(lo + 1, hi)
    }

    def sumList(lst1: List[Int], lst2: List[Int]):List[Int] = {
        (lst1, lst2) match {
            case (Nil, Nil) => Nil
            case (l1, l2) if (l1.size != l2.size) => throw new IllegalArgumentException("The number of elements in lists must be equal.")
            case (x::t1, y::t2) => (x + y) :: sumList(t1,t2)
        }
    }
}