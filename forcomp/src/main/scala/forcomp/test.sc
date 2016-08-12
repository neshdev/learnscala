import forcomp._

type Word = String

type Sentence = List[Word]

type Occurrences = List[(Char, Int)]

val dictionary: List[Word] = loadDictionary

def wordOccurrences(w: Word): Occurrences = w.groupBy(x=> x.toLower).map(x => (x._1,x._2.length)).toList.sorted

def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.reduce(_ + _ ))

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(x=> wordOccurrences(x))

def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List())
    case x :: xs =>
      for {
        z <- combinations(xs)
        n <- 0 to x._2
      } yield (if (n == 0) z else (x._1, n) :: z)
}

def subtract(x: Occurrences, y: Occurrences): Occurrences = (x ++ y).groupBy(z=> z).toList.filter(z => z._2.length == 1).map(x=> x._1).sorted

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def subSentence(occ: Occurrences ) : List[Sentence] = {
    if (occ.isEmpty  ) List()
    else {
      for {
        x <- combinations(occ)
        y <- dictionaryByOccurrences getOrElse(x, List())
        z <- subSentence(subtract(occ, x))
      } yield y :: z
    }
  }
  subSentence(sentenceOccurrences(sentence))
}

val temp = List("Yes", "Man")

//combinations(sentenceOccurrences(temp))
//
//for {
//  occ <- combinations(sentenceOccurrences(temp))
//  y <- dictionaryByOccurrences getOrElse(occ, List())
//} yield y


sentenceAnagrams(temp)
//*    List(
//  *      List(en, as, my),
//  *      List(en, my, as),
//  *      List(man, yes),
//  *      List(men, say),
//  *      List(as, en, my),
//  *      List(as, my, en),
//  *      List(sane, my),
//  *      List(Sean, my),
//  *      List(my, en, as),
//  *      List(my, as, en),
//  *      List(my, sane),
//  *      List(my, Sean),
//  *      List(say, men),
//  *      List(yes, man)
//    *    )