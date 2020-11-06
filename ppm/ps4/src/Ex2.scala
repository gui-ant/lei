object Ex2 {
  type Team = String
  type Goals = Int
  type Match = ((Team, Goals), (Team, Goals))
  type Fixtures = List[Match]

  //a) noItself which checks that no team plays with itself
  def same(m: Match): Boolean = m._1._1.equals(m._2._1)

  def noItself(j: Fixtures): Boolean = {
    (j foldRight true) ((m1, m2) => {
      !same(m1) && m2
    })
  }

  //b) withouRep which checks that no team plays more than one game.
  def teamCount(t: Team, j: Fixtures): Int = {
    (j foldRight 0) ((m1, m2) =>
      if (t.equals(m1._1._1) || t.equals(m1._2._1))
        1 + m2
      else
        m2
    )
  }

  def withoutRep(j: Fixtures): Boolean = {
    (j foldRight true) ((m1, m2) =>
      teamCount(m1._1._1, j) <= 1 && teamCount(m1._2._1, j) <= 1
    )
  }

  //c) teams which gives the list of teams participating in the Fixtures.
  def teams(f: Fixtures): List[Team] = {
    (f foldRight List[Team]()) (
      (m, n) => (List(m._1, m._2) foldRight n) (
        (t, n) => if (!n.contains(t._1)) t._1 :: n else n
      )
    )
  }

  // d) draws which gives lists of pairs of teams that tied for the day.
  def draws(f: Fixtures): List[(Team,Team)] = {
    (f foldRight List[Team]()) (
      (m, n) => if (m._1._2 == m._2._2) (m._1._1, m._2._1) :: n else n
    )
  }

  // e) points which calculates the points that each team obtained in the Fixtures (won - 3
  // points; lost – 0 points; tied - 1 point). The function should return a value of type:
  // List[(Team, Int)]
  def countResults(f: Fixtures, t:Team,f:(Goals,Goals) => Boolean):Int = {
    (f foldRight List[Team]()) ((m, n) => if(f(m._1._2,m._2._2)) 1 + n else n
  }
  def points(f: Fixtures, t:Team ): List[(String,Int)] = {
     (f foldRight List[Team]()) (
      (m, n) => (List(m._1, m._2) foldRight n) (
        (t, n) => if (!t.equals(t._1)) s"(won - ${countResults()}"
      )
    )
  }

}
