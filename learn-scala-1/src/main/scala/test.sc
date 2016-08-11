
object test {
  def combinations(chars: List[Char], acc: String): Unit = {
    if (chars.isEmpty) {
      println(acc)
      return
    }
    combinations(chars.tail, acc);
    combinations(chars.tail, acc + chars.head.toString);
  }

  combinations(List('a', 'b', 'c', 'd'), "")

  val list1 = "aa".toList
  val list2 = "baaa".toList

  //  aa, baaa -> baa == baaa.startswith baa 1
  //  aa, baaa -> aba == baaa.startswith aba 0
  //  aa, baaa -> aab == baaa.starswith aab 0


  //  aa, aaa -> aaa = aaa.startswith aaa 1
  //  aa, aaa -> aaa = aaa.startswith aaa 1
  //  aa, aaa -> aaa = aaa.startswith aaa 1
  //  aa, aa -> aaa = aa.startswith aaa 0
  //  aa, aa -> aaa = aa.startswith aaa 0
  //  aa, aa -> aaa = aa.startswith aaa 0
  //  aa, a -> aaa = a.startswith aaa 0
  //  aa, a -> aaa = a.startswith aaa 0
  //  aa, a -> aaa = a.startswith aaa 0

  def something(list1: List[Char], list2: List[Char], n: Int, acc: String): Unit = {
    if (list2.isEmpty || list1.isEmpty) {
      println(acc)
      return
    }
    println((list1 take n) ::: (list2.head :: Nil) ::: (list1 drop n + 1))
    something(list1, list2.tail, n + 1, acc)
    something(list1.tail, list2, n + 1, acc)

  }


  something(list1, list2, 1, "")
  val n = 1
  (list1 take n) ::: (list2.head :: Nil) ::: (list1 drop n + 1)


  //take <==> drop
  //head <==> last
  //tail <==> init
  //zip
  //

}