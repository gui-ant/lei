package Ficha5

object Main {
  def main(args: Array[String]): Unit = {
    val t: Turma =
      Turma("id22",
        List(
          (11, "Carlos", RegimeOPT.Ordinario, Some(10), Some(20)),
          (12, "Jose", RegimeOPT.TrabEstud, Some(8), None),
          (12, "Aida", RegimeOPT.TrabEstud, Some(9.5), Some(9.5)),
          (12, "Joaquim", RegimeOPT.TrabEstud, Some(15), Some(10))
        )
      )
    println("Workers-students: " + t.trabs())
    println("Student with number [Int]: ")
    val a = scala.io.StdIn.readInt()
    println("Student with number: " + t.searchStudent(a))
    println("Student evaluation: " + t.finalGrade(a))
    println("Aproved students: " + t.approved())


  }
}

