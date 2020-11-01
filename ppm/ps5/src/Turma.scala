package Ficha5

import Ficha5.RegimeOPT.RegimeOPT
import Ficha5.Turma._

import scala.jdk.FunctionWrappers.AsJavaLongUnaryOperator

case class Turma(id: String, alunos: Alunos) {

  def trabs(): Alunos = Turma.trabs(this)

  def searchStudent(nr: Turma.Numero): Option[Aluno] = Turma.searchStudent(this, nr)

  def finalGrade(nr: Turma.Numero): Option[Float] = Turma.finalGrade(this, nr)

  def approved(): List[(Nome, Float)] = Turma.approved(this)

  def changeNP(a: Aluno, v: Option[Float]): Alunos = Turma.changeNP(this, a, v)

  def changeNT(a: Aluno, v: Option[Float]): Alunos = Turma.changeNT(this, a, v)
}

object Turma {
  type Nome = String
  type Numero = Int
  type NT = Option[Float]
  type NP = Option[Float]
  type Regime = RegimeOPT
  type Aluno = (Numero, Nome, Regime, NT, NP)
  type Alunos = List[Aluno]

  /*
  a) trabs that filters workers-students only
   */
  def trabs(t: Turma): Alunos = {
    t.alunos.filter(x => x._3.equals(RegimeOPT.TrabEstud))
  }

  /*
  b) searchStudent that performs the search for a student by their student number. Note,
  the type returned is Option[Aluno]
   */
  def searchStudent(t: Turma, nr: Numero): Option[Aluno] = {
    t.alunos find (x => x._1 == nr)
  }

  /*
  c) finalGrade that calculates a student's final grade according to the formula NF = 0.6NT
  + 0.4NP. Note that it is only possible to calculate this note if the number provided exists
  in the class, and both NT and NP components reach the minimum grade of 9.5 values
  otherwise None should be returned. Use the Option get method to access its value.
   */
  def nf(nt: => NT, np: => NP): Option[Float] =
    if (nt != None && np != None && nt.get >= 9.5 && np.get >= 9.5)
      Option(.6f * nt.get +.4f * np.get)
    else
      None

  def finalGrade(t: Turma, nr: Numero): Option[Float] = t.searchStudent(nr) match {
    case None => None
    case s => nf(s.get._4, s.get._5)
  }

  /*
  d) approved: List[(Nome, Float)] that presents all students' grades approved, i.e. with
  a final grade greater than or equal to 10 values.
  */

  def approved(t: Turma): List[(Nome, Float)] = {
    t.alunos.foldRight(List[(Nome, Float)]())((a, l) =>
      if (finalGrade(t, a._1) != None) (a._2, finalGrade(t, a._1).get) :: l else l
    )
  }

  /*
  e) changeNP and changeNT, which allow to change the practical and theoretical notes of a
  student in a class. Suggestion: use the List updated, indexWhere and apply methods
  instead of the searchStudent function.
  */
  def changeNP(t: Turma, a: Aluno, v: NP): Alunos = {
    val i = t.alunos.indexWhere(x => x.equals(a)) //funciona
    t.alunos.updated(i, new Aluno(a._1, a._2, a._3, v, a._5))
  }

  def changeNT(t: Turma, a: Aluno, v: NT): Alunos = {
    val i = t.alunos.indexWhere(x => x.equals(a)) //funciona
    t.alunos.updated(i, new Aluno(a._1, a._2, a._3, a._4, v))
  }
}

