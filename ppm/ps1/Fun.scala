//singleton object
object Fun {
    def func1(x:Double, y:Int) = x + (70 * y)
    def ex(a:Double) = 50 * a
    
    /*
    a) Define a method that receives two pairs of integers and returns a pair of integers. The
    first element of the result being the sum of the input pair, and the second element of
    the pair the product of the input pairs
    */
    def a(x:Int, y:Int) = (x + y, x * y)
    
    /*
    b) Write a method that, given three Int numbers, returns a pair containing the largest
    element in the first element, and the second largest number in the second element.
    */
    def b(x:Int, y:Int, z:Int) =  {
        val l = List(x, y, z).sorted.reverse
        (l.head, l.tail.head)
    }

    /*
    c) Write a method that receives a triple of Int numbers and returns a triple where the same
    numbers are ordered in descending order.
    */
    def c(x:Int, y:Int, z:Int) = List(x, y, z).sorted.reverse

    /*
    d) The sides of any triangle respect the following constraint: the sum of the lengths of any
    two sides, is greater than the length of the third side. Write a method that receive the
    length of three segments and return a Boolean value indicating whether the constraint
    is satisfied.
    */
    def d(x:Int, y:Int, z:Int) =  {
        val l = List(x, y, z).sorted
        l.head + l.tail.head > l.tail.tail.head
    }

    /*
    e) Write a method abrev that receives a string containing a person's name and returns a
    string with the first and last name.
    For example, abrev "José Carlos Martins Sousa" = "José Sousa"
    */

    def abrev(x: String) = {
        val y = x.split(" ")
        if (y.length<=1) x else y.head + " " + y.last
    } 

}