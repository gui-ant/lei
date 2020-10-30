object Ex2 {
    type Team = String
    type Goals = Int
    type Match = ((Team, Goals), (Team, Goals))
    type Fixtures = List[Match]

    //a) noItself which checks that no team plays with itself
    def same(m: Match):Boolean = m._1._1.equals(m._2._1)
    def noItself(j: Fixtures):Boolean = {
        (j foldRight true) ((m1,m2) => {
            !same(m1) && m2
        })
    }
    //b) withouRep which checks that no team plays more than one game.
    def teamCount(t:Team, j:Fixtures):Int = {
        (j foldRight 0) ((m1,m2) => 
            if(t.equals(m1._1._1) || t.equals(m1._2._1))
                1 + m2
            else
                m2
        )
    }
    def withoutRep(j:Fixtures):Boolean = {
        (j foldRight true) ((m1, m2) => 
            teamCount(m1._1._1, j) <= 1 && teamCount(m1._2._1, j) <= 1
        )
    }
}