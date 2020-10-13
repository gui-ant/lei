object Ex1 extends App {
    /*
    a) Define the method transf that does the following transformation: receives a list and
    replaces the 1st with the 2nd element, and the 3rd with the 4th, until the end of the list.
    For example: transf [1,2,3,4,5,6] ⇒ [2,1,4,3,6,5].
    */
    def transf(lst:List[Int]):List[Int] =  {
        lst match {
            case Nil => Nil; 
            case x::Nil => lst; 
            case x::y::tail => y::x::transf(tail); 
        }
    }
    /*
    b) Define a method that calculates the product of all the elements in a list of numbers.
    */
    def product(lst:List[Int]):Int =  {
        lst match {
            case x::Nil => x; 
            case h::tail => h * product(tail)
            case _ => 0; 
        }
    }
    /*
    c) Define a method that, given a list and an element, places it at the end of the list.
    */
    def placeInTheEnd[A](lst:List[A], el: A):List[A] =  {
        lst match {
            case Nil => List(el);
            case x::tail => x::placeInTheEnd(tail, el)
        }
    }
    /*
    d) Define a method that, given two lists (of any type) concatenates them (without using
    the ++ operator), i.e., calculates a list with the elements of the first list followed by those
    on the second list. 
    */
    def concatenate[A](lst1:List[A], lst2:List[A]):List[A] =  {
        lst2 match {
            case Nil => lst1; 
            case h::tail => concatenate(placeInTheEnd(lst1, h),tail); 
        }
    }

    /*
    e) Define a sumEl method that receives a list of pairs of Int and calculates the sum of the
    pairs with indexes 2 and 4.
    */
    def sumEl(pairs:List[(Int, Int)]):Int =  {
        def sum(lst:List[(Int, Int)], count:Int, acc:Int):Int =  {
            lst match {
                case Nil => acc
                case h::tail =>  {
                    if (count == 2 || count == 4)
                        sum(tail, count + 1, acc + h._1 + h._2)
                    else
                        sum(tail, count + 1, acc)
                }
            }
        }
        sum(pairs, 0, 0)
    }

    /*
    f) In the previous work sheet, we have seen a way to calculate the average of a list of numbers. 
        def average1(lst: List[Double]) = lst.sum / lst.length
    
        However, this solution goes through the list two times!
            a. Define a method that given a list, calculates a pair containing the length of the
            list and the sum of its elements – going through the list only once.
            b. Using the previous method, define a method that calculates the average of the
            elements of a list.
    */
    def lengthAndSum(lst:List[Int]):(Int, Int) = {
        def loop(l:List[Int], len:Int, s:Int):(Int, Int) = {
            l match {
                case Nil => (len, s);
                case x::tail => loop(tail, len + 1, s + x)
            }
        }
        loop(lst,0,0)
    }
    def average(lst:List[Int]):Double = {
        val x = lengthAndSum(lst)
        x._2 / x._1
    }

    /*
    However, there are methods in which it is more difficult to avoid these multiple crossings of
    the list.
        g) Define a polymorphic method (without using the List method - take) that, given a list,
        divides it into two (returning a pair of lists) with the same number of elements (this, of
        course, if the original list has an even number of elements; in the other case, one of the
        lists will have one more element). Use the takeRight method.
        h) Define a method that, given a list (of Double) and a value (Double), returns a pair of lists
        where the first contains all the elements of the list below this value and the second list
        contains all other elements.
        Example: metH(List(1.0,2.0,4.0,5.0), 3.0) //(List(1.0, 2.0), List(4.0, 5.0))
        i) Define a method that, given a list of Double, returns the list with the elements that are
        superior to the average.
    */
    def devide[A](lst:List[A]):(List[A], List[A]) = {
        def aux(l:List[A], l1:List[A], i:Int):(List[A], List[A]) = {
            l match{
                case Nil => (l1, lst.takeRight(i))
                case x::Nil => (l1 :+ x, lst.takeRight(i))
                case x::tail => aux(tail.init, l1 :+ x, i + 1)
            }
        }
        aux(lst, List(), 0)
    }    

    def devideByValue(lst:List[Double], value:Double):(List[Double], List[Double]) = {
        def aux(l:List[Double], l1:List[Double], l2:List[Double]):(List[Double],List[Double]) = {
            l match{
                case Nil => (l1, l2)
                case x::tail => {
                    if (x < value)
                        aux(tail, l1 :+ x, l2)
                    else
                        aux(tail, l1, l2 :+ x)
                }
            }
        }
        aux(lst, List(), List())
    }
    
    def devideByAvg(lst:List[Double]):(List[Double], List[Double]) = {
        def avg(l:List[Double], c:Double, i:Int):Double = {
            l match{
                case Nil => if (i == 0) 0 else c / i
                case x::tail => avg(tail, c + x, i + 1)
            }
        }
        val a = avg(lst, 0, 0)
        println("Average: " + a)
        devideByValue(lst, a)
    }

    /*
    Consider that we want to define methods for manipulating a phone book. So we decided that
    the information for each entry in the phone book will contain the name, the phone number and
    e-mail address. We can then make the following definitions:

        type Entry = (String, String, String)
        type LTelef = List[Entry]

    The method that calculates the known email addresses can be defined as:
        def emails(lst : LTelef) : List[String] = {
            lst match {
                case Nil => Nil
                case (_ , _ , email):: tail => email :: (emails(tail))
            }
        }
        
        j) Define a method that, given a phone book, produces the list of email addresses of the
        entries whose telephone numbers are from the fixed network (prefix ’2’).
        k) Define a method that given a phone book and a name, returns the pair with the phone
        number and the email address associated with that name, in the phone book.
    */
    type Entry = (String, String, String)
    type LTelef = List[Entry]
    def emails(lst : LTelef) : List[String] = {
        lst match {
            case Nil => Nil
            case (_ , _ , e):: tail => e :: emails(tail)
        }
    }
    
    def emailsFromFixedNetwork(lst : LTelef) : List[String] = {
        lst match {
            case Nil => Nil
            case (_ , p , e):: tail => {
                if (p(0) == '2')
                    e :: (emailsFromFixedNetwork(tail))
                else
                    emailsFromFixedNetwork(tail)
            }
        }
    }
    
    def info(lst : LTelef, name:String): (_,_) = {
        lst match {
            case Nil => ("","")
            case (n , p , e):: tail if (n == name) => (p,e)
            case _::tail => info(tail,name)
        }
    }

}