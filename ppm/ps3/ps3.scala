import scala.annotation.tailrec

object Ex1 {
    println("factorialA: " + factorialA(5))
    println("factorialB: " + factorialB(5))
    println("factorialC: " + factorialC(5))

    // a) without using an if
    def factorialA(n:Int):Int = {
        n match {
            case 0 => 1
            case x => x * factorialA(x - 1)
        }
    }
    
    // b) using an if
    def factorialB(n:Int):Int = if (n == 0) 1 else n * factorialB(n - 1)

    // c) tail recursive
    def factorialC(n:Int):Int = {
        @tailrec
        def loop(acc: Int, n:Int):Int = {
            if (n == 0) acc else loop(acc * n, n - 1)
        }
        loop(1,n)
    }

}

object Ex2 {
    def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
        println(lo)
        if(lo >= hi) 
            LazyList.empty
        else 
            lo #:: lazyListRange(lo + 1, hi)
    }
}
object Ex3 {

    // a) Write a function that accepts two lists (with the same length) and constructs a new list
    // by adding corresponding elements. For example, List(1,2,3) and List(4,5,6)
    // become List(5,7,9).

    def sumList(lst1: List[Int], lst2: List[Int]):List[Int] = {
        (lst1, lst2) match {
            case (Nil, Nil) => Nil
            case (l1, l2) if (l1.size != l2.size) => throw new IllegalArgumentException("The number of elements in lists must be equal.")
            case (h1::t1, h2::t2) => (h1 + h2) :: sumList(t1, t2)
        }
    }
    // b) Generalize the function you just wrote so that it is not specific to integers or addition.
    // Name your generalized function zipWith.
    // test: 
    //      zipWith(List(1,2,3),List(4,5,6),(x:Int,y:Int) => x + y)
    //      zipWith(List(1.0,2.0,3.0),List(4.0,5.0,6.0),(x:Double,y:Double) => x * y)
    def zipWith[A](lst1: List[A], lst2: List[A], f: (A, A) => A):List[A] = {
        (lst1, lst2) match {
            case (Nil, Nil) => Nil
            case (l1, l2) if (l1.size != l2.size) => throw new IllegalArgumentException("The number of elements in lists must be equal.")
            case (h1::t1, h2::t2) => f(h1, h2) :: zipWith(t1, t2, f)
        }
    }
}

object Ex4 {
    // a) Paresord function that receives a list of pairs of numbers and returns only the pairs
    // in that the first component is inferior to the second.
    def paresord(lst: List[(Int,Int)]):List[(Int,Int)] = 
        lst match {
            case Nil => Nil
            case x :: xs => if (x._1 < x._2) x :: paresord(xs) else paresord(xs)
        }
    //b) myconcat function that receives a list of strings and joins them (concatenated) in
    //a single string.
    def myconcat(lst: List[String]):String = 
        lst match {
            case Nil => ""
            case x :: xs => x + myconcat(xs)
        }
    // c) Maximum function that receives a list of pairs of numbers (of type Double) and
    // calculates a list with just the largest element of each pair.
    def maximum(lst:List[(Double, Double)]):List[Double] = 
        lst match {
            case Nil => Nil
            case x :: xs => if (x._1 >= x._2) x._1 :: maximum(xs) else x._2 :: maximum(xs)
        }
}
object Ex5 {
    // def indicative(ind:String, telefs:List[String]) = telefs filter ( x => x. substring(0,ind.length).equals(ind))
    // Redefine this function with explicit recursion, that is, avoiding the use of filter.
    def a(lst:List[Int]) = lst map (x => x%2 !=0)
    def b(lst:List[Int]) = lst filter (x => x%2 !=0)
    def c(lst:List[Int]) = lst map ( x => x%3 == 0)
    def d(lst:List[Int]) = lst filter ( x => x%3 == 0)
    def e(lst:List[Int]) = lst filter ( x => x < 7)
    def f(lst:List[List[Int]]) = lst map ( x => 7 :: x)
    def g(lst:List[Int]) = lst map ( x => List(x))
    def h(lst:List[Int]) = lst filter ( x => x%2 !=0) map ( x => x + 1)
    def i(lst:List[Int]) = lst map ( x => x + 1) filter ( x => x%2 !=0)
}
object Ex6 {
    // def indicative(ind:String, telefs:List[String]) = telefs filter ( x => x. substring(0,ind.length).equals(ind))
    // Redefine this function with explicit recursion, that is, avoiding the use of filter.
    def indicative(ind:String, telefs:List[String]):List[String] = telefs match {
            case Nil => Nil
            case x :: xs => 
                if (x.substring(0,ind.length).equals(ind)) 
                    x :: indicative(ind, xs) 
                else 
                    indicative(ind, xs)
        }
}

object Ex7 {
    // Implement a function to convert a list of names to a list of abbreviations for those names, as
    // follows: List(“José Carlos Mendes”, “André Carlos Oliveira”) in List(“J. Mendes ”,“ A. Oliveira").
    // Note: use map and the s String Interpolator to create the final String (e.g. s"${exp}. ${exp}")
    def abbreviations(names:List[String]):List[String] = names match {
            case Nil => Nil
            case x :: xs => s"${x.substring(0,1)}. ${x.split(" ").last}" :: abbreviations(xs) 
        }
}

object Main extends App {
    println("\nExercise 1")
    val v = 10
    println(s"Fatorial de ${v}:")
    println("Sem if: " + Ex1.factorialA(v))
    println("Com if: " + Ex1.factorialB(v))
    println("tail recursive: " + Ex1.factorialC(v))
    println("\nExercise 2")
    println(Ex2.lazyListRange(0,10))
    println("\nExercise 3")
    val lst3_1 = List(1, 2, 3); 
    val lst3_2 = List(4, 5, 6)
    println("lst3_1: " + lst3_1.toString)
    println("lst3_2: " + lst3_2.toString)
    println("Sum list: " + Ex3.sumList(lst3_1, lst3_2))
    println("Sum list (HOF): " + Ex3.zipWith(lst3_1, lst3_2,(x:Int, y:Int) => x + y))
    println("Multiply list (HOF): " + Ex3.zipWith(lst3_1.map((x) => x * 1.0), lst3_2.map((x) => x * 1.0), (x:Double, y:Double) => x * y))
    println("\nExercise 4")
    val lst4_1 = List((0, 0), (0, 1), (1, 0), (1, 1))
    val lst4_2 = List("my", "concat")
    val lst4_3 = List((2.0, 4.0), (3.2, 1.9))
    println(lst4_1.toString + " => " + Ex4.paresord(lst4_1))
    println(lst4_2.toString + " => " + Ex4.myconcat(lst4_2))
    println(lst4_3.toString + " => " + Ex4.maximum(lst4_3))
    println("\nExercise 5")
    val lst5_1 = List(1,2,3,4,5)
    val lst5_2 = List(5, 6, 23, 3)
    val lst5_3 = List(1,3,7,8,12,15)
    val lst5_4 = List(List(2,3),List(1,5,3))
    println(lst5_1.toString + " => " + Ex5.a(lst5_1).toString)
    println(lst5_1.toString + " => " + Ex5.b(lst5_1).toString)
    println(lst5_2.toString + " => " + Ex5.c(lst5_2).toString)
    println(lst5_2.toString + " => " + Ex5.d(lst5_2).toString)
    println(lst5_3.toString + " => " + Ex5.e(lst5_3).toString)
    println(lst5_4.toString + " => " + Ex5.f(lst5_4).toString)
    println(lst5_1.toString + " => " + Ex5.g(lst5_1).toString)
    println(lst5_1.toString + " => " + Ex5.h(lst5_1).toString)
    println(lst5_1.toString + " => " + Ex5.i(lst5_1).toString)
    println("\nExercise 6")
    val lst6_1 = List("253116787", "213448023", "253119905")
    println(lst6_1.toString + " => " + Ex6.indicative("253", lst6_1).toString)
    println("\nExercise 7")
    val lst7_1 = List("José Carlos Mendes", "André Carlos Oliveira")
    println(lst7_1.toString + " => " + Ex7.abbreviations(lst7_1).toString)
}