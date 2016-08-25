import common._

//import patmat.Huffman.{Fork, Leaf, _}

abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

def weight(tree: CodeTree): Int = tree match {
  case Fork(_, _, _, weight) => weight
  case Leaf(_, weight) => weight
}
def chars(tree: CodeTree): List[Char] = tree match {
  case Fork(_, _, chars, _) => chars
  case Leaf(char, _) => List(char)
}

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
)

def string2Chars(str: String): List[Char] = str.toList

def times(chars: List[Char]): List[(Char, Int)] = chars match {
  case Nil => Nil
  case x :: xs => (x, chars.count(_ == x)) :: times(xs.filterNot(_ == x))
}

times(List('a', 'b', 'a'))

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  for {
    s <- freqs.sortWith(_._2 < _._2)
  } yield new Leaf(s._1, s._2)

val leafs = makeOrderedLeafList(List(('a', 1), ('b', 2), ('c', 3), ('d', 3), ('e', 1), ('f', 12)))

def singleton(trees: List[CodeTree]): Boolean = trees match {
  case t :: Nil => true
  case _ => false
}

def combine(trees: List[CodeTree]): List[CodeTree] = {
  def insert(t: CodeTree, temp: List[CodeTree]): List[CodeTree] = {
    temp.takeWhile(weight(_) < weight(t)) ::: t :: temp.dropWhile(weight(_) < weight(t))
  }
  val tree1 = trees.head
  val tree2 = trees.tail.head
  val newTree = makeCodeTree(tree1, tree2)
  insert(newTree, trees.tail.tail)
}


val pairs = times("adadfsasddfgdsaadf".toList)
val leaves = makeOrderedLeafList(pairs)
val trees = combine(leaves)
combine(trees)



def until(condition: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(input: List[CodeTree]): List[CodeTree] = {
  if (condition(input)) input
  else until(condition, combine)(combine(input))
}

def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head


// Part 3: Decoding

type Bit = Int

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def decodeChar(subTree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = subTree match {
    case Leaf(char, _) => (char, bits)
    case Fork(left, right, _, _) if (!bits.isEmpty) => {
      decodeChar(if (bits.head == 0) left else right, bits.tail)
    }
    case _ => throw new Exception("Not enought bits")
  }

  def decodeAcc(bits: List[Bit], acc: List[Char]): List[Char] = {
    if (bits.isEmpty) acc.reverse
    else {
      val charBits = decodeChar(tree, bits)
      decodeAcc(charBits._2, charBits._1 :: acc)
    }
  }

  decodeAcc(bits, List())
}


val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)


val decodedSecret: List[Char] = decode(frenchCode, secret)




// Part 4a: Encoding using Huffman tree


def encodeChar(subtree: CodeTree, char: Char): List[Bit] = {
  if (!chars(subtree).contains(char)) throw new Exception(char + "not int CodeTree")
  else subtree match {
    case Leaf(charAtLeaf, _) => Nil
    case Fork(left, right, _, _) =>
      if (chars(left).contains(char)) 0 :: encodeChar(left, char)
      else 1 :: encodeChar(right, char)
  }
}

/**
  * This function encodes `text` using the code tree `tree`
  * into a sequence of bits.
  */
def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def encodeAcc(plainText: List[Char], acc: List[List[Bit]]): List[Bit] = {
    if (plainText.isEmpty) acc.reverse.flatten
    else encodeAcc(plainText.tail, encodeChar(tree, plainText.head) :: acc)
  }
  def encoder(plainText: List[Char]): List[Bit] = encodeAcc(plainText, List())
  encoder(text)
}

encode(frenchCode)(decodedSecret) == secret

// Part 4b: Encoding using code table

type CodeTable = Map[Char, List[Bit]]

/**
  * This function returns the bit sequence that represents the character `char` in
  * the code table `table`.
  */
def codeBits(table: CodeTable)(char: Char): List[Bit] = table(char)

/**
  * Given a code tree, create a code table which contains, for every character in the
  * code tree, the sequence of bits representing that character.
  *
  * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
  * a valid code tree that can be represented as a code table. Using the code tables of the
  * sub-trees, think of how to build the code table for the entire tree.
  */


def convert(tree: CodeTree): CodeTable = tree match {
  case Leaf(char, _) => Map(char -> List())
  case Fork(left, right, _, _) => {
    val leftCodeTree = convert(left).mapValues(0 :: _)
    val rightCodeTree = convert(right).mapValues(1 :: _)
    mergeCodeTables(leftCodeTree, rightCodeTree)
  }
}

val codeTree = createCodeTree("aaaabbbccd".toList)
val codeTable = convert(codeTree)

/**
  * This function takes two code tables and merges them into one. Depending on how you
  * use it in the `convert` method above, this merge method might also do some transformations
  * on the two parameter code tables.
  */
def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b

/**
  * This function encodes `text` according to the code tree `tree`.
  *
  * To speed up the encoding process, it first converts the code tree to a code table
  * and then uses it to perform the actual encoding.
  */
def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val table = convert(tree)
  text.map(x => table(x)).flatten
  //text.map(x=> encodeChar(tree, x)).flatten
}

frenchCode

decodedSecret
encode(frenchCode)(decodedSecret) == secret

val map = convert(frenchCode)
map('h')
map('u')
map('f')
map('f')


quickEncode(frenchCode)(decodedSecret) == secret