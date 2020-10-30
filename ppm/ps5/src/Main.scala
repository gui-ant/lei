package Ficha5

object Main {
  def main(args: Array[String]): Unit = {
    val t: Turma =
      Turma("id22",
        List(
          (11, "as", RegimeOPT.Ordinario, Some(10), Some(20)),
          (12, "Jose", RegimeOPT.TrabEstud, Some(13), None)
        )
      )
    println("Workers-students: " + t.trabs())
    println("Student with number [Int]: ")
    val a = scala.io.StdIn.readInt()
    println("Student with number: " + t.searchStudent(a))
    println("Student evaluation: " + t.finalGrade(a))


  }
}

