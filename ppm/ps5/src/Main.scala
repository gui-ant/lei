package Ficha5

object Main {
  def main(args: Array[String]): Unit = {
    val t: Turma =
      Turma("id22",
        List(
          (11, "Carlos", RegimeOPT.Ordinario, Some(10), Some(20)),
          (12, "Jose", RegimeOPT.TrabEstud, Some(8), None),
          (13, "Aida", RegimeOPT.TrabEstud, Some(9), Some(9)),
          (14, "Joaquim", RegimeOPT.TrabEstud, Some(15), Some(10))
        )
      )
    println("Workers-students: " + t.trabs())
    println("Student with number [Int]: ")
    val n = scala.io.StdIn.readInt()
    val a = t.searchStudent(n).get
    println("Student with number: " + a)
    println("Student evaluation: " + t.finalGrade(n))
    println("Approved students: " + t.approved())
    println("NP Change for student nr. " + a._1 + ": " + t.changeNP(a, Some(15)))
    println("NT Change for student nr. " + a._1 + ": " + t.changeNT(a, Some(15)))
    print()
  }
}

