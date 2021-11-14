@main def main: Unit =

  import scala.math

  def evalPolynomial(x: Int, coefficients: List[Int]): Int = {
    coefficients.foldRight(0)((head,tail) => head + x*tail)
  }
  test("evalPolynomial", evalPolynomial _, "x", "coefficients")

  def segments(list: List[Int]): List[List[Int]] = {
    list.foldRight[List[List[Int]]] (List[List[Int]]())((h,t)=>
      if (t.isEmpty || t.head.isEmpty || h>= t.head.head) List(h)::t
      else (h::t.head)::t.tail
    )

    /*list.foldRight(List[List[Int]]()){(x,lol)=>       RIGHT ANSWER CLASS
      if (lol.nonEmpty && x<lol.head.head) (x::lol.head)::lol.head
      else List(x) :: lol
    }*/
  }
  test("segments", segments _, "list")

  def subsets(set: Set[Int]): Set[Set[Int]] = {
    set.foldRight[Set[Set[Int]]](Set[Set[Int]](Set()))((h,r)=> r.flatMap(e=> (r+e) + (e+h) ) )

    /*set.foldRight()((x,sos)=> ALL RIGHT ANSWERS
      sos union sos.map(_ + x)
    )*/

    /*set.foldRight(Set(Set.empty[Int]))((x,sos)=> sos.map(_+x).foldRight(sos)((s,sos2)=> sos2 + s))*/
    /*set.foldRight(Set(Set.empty[Int]))((x,sos)=>sos.flatMap(s=>Set(s,s+x))*/

  }
  test("subsets", subsets _, "set")

  def intersperse(x: Int, list: List[Int]): List[List[Int]] = {
    list.foldRight[List[List[Int]]](List[List[Int]](List(x)))((h,r)=> (x::
      (r.map(e=>h::e)).head.filter(_!=x))::(r.map(e=>h::e)))
    /*list.foldRight(List(List(x))){ (item,lol)=>
      (x :: item :: lol.head.tail) :: lol.map(item :: _)
    }*/
  }
  test("intersperse", intersperse _, "x", "list")

  def permutations(list: List[Int]): Set[List[Int]] = {

    list.foldRight(Set(List.empty[Int]){(s,sol)=>
      sol.flatMap(intersperse(x,_))
    }

    /*list.foldRight[Set[List[Int]]](Set(List()))  ((h,r)=> r.flatMap(e=>   YOUR ANSWER AND IS WRONG
      intersperse(h,e).flatMap(a=> r+a).filter(_!=h))
    )*/
  }
  test("permutations", permutations _, "list")
