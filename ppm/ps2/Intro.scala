object Intro{

    // A method that verify if a list (of any type) is empty can be defined in the following form:
    def empty[A](lst: List[A]) = if(lst.length == 0) true else false

    // The method to calculate the length of a List might be implemented in the following way:
    def length( lst: List[Int]): Int = {
        lst match {
            case Nil => 0
            case _ :: tail => 1 + length(tail)
        }
    }

    // The method to calculate the length of a List might be implemented in the following way:
    def distance(lstPoint: List[(Double, Double)]): List[Double] = {
        lstPoint match {
            case Nil => Nil;
            case (x, y) :: tail => (Math.sqrt(x * x + y * y)) :: distance(tail);
        }
    }

}