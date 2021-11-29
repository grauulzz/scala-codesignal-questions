package org.grauulzz
package practicequestions

object Main extends App {
  val startTime = System.nanoTime()
  println(s"${Console.GREEN}----------------------")

  /** [makeArrayConsecutive2]
    * Ratiorg got statues of different sizes as a present from CodeMaster for his birthday,
    * each statue having an non-negative integer size.
    * Since he likes to make things perfect, he wants to arrange them from smallest to largest so that each statue
    * will be bigger than the previous one exactly by 1. He may need some additional statues to be able to accomplish that.
    * Help him figure out the minimum number of additional statues needed.
    *
    * [Example]
    * For statues = [6, 2, 3, 8], the output should be
    * solution(statues) = 3.
    * Ratiorg needs statues of sizes 4, 5 and 7.
    *
    * @param statues
    * @return
    */
  def makeArrayConsecutive(statues: Array[Int]): Int = {
    statues.max - statues.min - statues.length
  }
  /** [makeArrayConsecutive2_2]
    *
    * @param statues
    * @return
    */
  def makeArrayConsecutive2(statues: Array[Int]): Int = {
    var count: Int = 0
    val sorted: Array[Int] = statues.sortWith(_ < _)

    for (i <- 0 to sorted.length - 2)
      count += (sorted(i + 1) - sorted(i) - 1)
    count
  }
  /** [almostIncreasingSequence]
    * Given a sequence of integers as an array,
    * determine whether it is possible to obtain a strictly
    * increasing sequence by removing no more than one element from the array.
    *
    * Note: sequence a0, a1, ..., an is considered to be a strictly increasing
    * if a0 < a1 < ... < an
    * Sequence containing only one element is also considered to be strictly increasing.
    *
    * [Example]
    * For sequence = [1, 3, 2, 1], the output should be
    * solution(sequence) = false
    *
    * For sequence = [1, 3, 2], the output should be
    * solution(sequence) = true
    *
    * You can remove 3 from the array to get the strictly increasing sequence [1, 2]
    * Alternately, you can remove 2 to get the strictly increasing sequence [1, 3]
    *
    * [test]
    * println(almostIncreasingSequence(Array(1, 3, 2, 1)))              // false
    * println(almostIncreasingSequence(Array(1, 3, 2)))                 // true
    * println(almostIncreasingSequence(Array(1, 2, 1, 2)))              // true
    * println(almostIncreasingSequence(Array(3, 6, 5, 8, 10, 20, 15)))  // false
    * println(almostIncreasingSequence(Array(0, -2, 5, 6)))             // true
    *
    * @param sequence
    * @return
    */
  def almostIncreasingSequence(sequence: Array[Int]): Boolean = {
      (1 until sequence.length).map(i => if (sequence(i - 1) < sequence(i)) 0 else 1)
      .sum <= 1 && (2 until sequence.length)
      .map(i => if (sequence(i - 2) < sequence(i)) 0 else 1).sum <= 1
  }
  /** [almostIncreasingSequence2]
    *
    * @param sequence
    * @return
    */
  def almostIncreasingSequence2(sequence: Array[Int]): Boolean = {
    sequence.sortWith((a, b) => a <= b)
    val isSeq = (sequence: Array[Int]) => (sequence.max - sequence.min - sequence.length + 1 >= 0)
    val x = sequence.filter(_ != sequence.max)
    val y = sequence.filter(_ != sequence.min)
    isSeq(sequence) || isSeq(x) || isSeq(y)
  }
  /** [matrixElementsSum]
    * Given matrix, a rectangular matrix of integers,
    * where each value represents the cost of the room,
    * your task is to return the total sum of all rooms that are suitable for the CodeBots
    * (ie: add up all the values that don't appear below a 0)
    *
    * [suitable rooms for codebots]
    * non-free rooms                         [any room not 0 in the matrix]
    * not below one of the free rooms [any room not below 0 int the matrix]
    *
    * return the total sum of all rooms that are suitable
    * [sum of all values not below a zero value]
    *
    * [example1]
    * matrix = [[0, 1, 1, 2],
    *           [0, 5, 0, 0],
    *           [2, 0, 3, 3]]
    *
    * [output]: solution(matrix) = 9
    * [reason]: 1 + 5 + 1 + 2 = 9
    *
    * [example2]
    * matrix = [[1, 1, 1, 0],
    *           [0, 5, 0, 1],
    *           [2, 1, 3, 10]]
    *
    * [output]: solution(matrix) = 9
    * [reason]: 1 + 1 + 1 + 5 + 1 = 9
    *   [note]: the last col of the first row of matrix is 0, all rooms under are useless
    *
    * [test]
    * println(matrixElementsSum(Array(Array(0, 1, 1, 2), Array(0, 5, 0, 0), Array(2, 0, 3, 3))))
    *
    * [one-liners]
    * matrix.transpose.map( col => col.reduceRight( (l,r) => if (l == 0) 0 else l + r ) ).sum
    *
    * @param matrix
    * @return
    */
  def matrixElementsSum(matrix: Array[Array[Int]]): Int = {
    val rooms = matrix.transpose
    val res = for {
      row <- rooms
      valid = row.takeWhile(_ != 0)
    } yield valid.sum
    res.sum
  }
  /** matrixElementsSum2
    *
    * @param matrix
    * @return
    */
  def matrixElementsSum2(matrix: Array[Array[Int]]): Int = {
    matrix
      .transpose
      .flatMap(_.takeWhile(_ != 0))
      .sum
    }
  /** [allLongestStrings]
    * Given an array of strings, return another array containing all of its longest strings
    *
    * [example]
    * For inputArray = ["aba", "aa", "ad", "vcd", "aba"] the output should be:
    * solution(inputArray) = ["aba", "vcd", "aba"]
    *
    * [test]
    * println(allLongestStrings(Array("aba", "aa", "ad", "vcd", "aba")).toList)
    * println(allLongestStrings(Array("abc", "eeee", "abcd", "dcd")).toList)
    *
    * [ideas]
    * def sortBySize(x: String, y: String) = x.sizeCompare(y.size) != 1
    *
    * @param inputArray
    * @return
    */
  def allLongestStrings(inputArray: Array[String]): Array[String] = {
    val sortByLength = (s1: String, s2: String) => s1.length() > s2.length()
    val sortedByLength = inputArray.sortWith(sortByLength)
    val ll = sortedByLength(0).length()
    val filterByLength = sortedByLength.filter(x => x.length >= ll)
    filterByLength
  }
  /** [allLongestStrings2]
    *
    * best answer on codesignal for this question
    * println(allLongestStrings(Array("abc", "eeee", "abcd", "dcd")).toList)
    * 
    * @param inputArray
    * @return
    */
  def allLongestStrings2(inputArray: Array[String]): Array[String] = {
    inputArray.groupBy(_.length)
      .maxBy(_._1)
      ._2
  }

  /** [commonCharacterCount]
    * Given two strings, find the number of common characters between them
    * 
    * For s1 = "aabcc" and s2 = "adcaa" the output should be:
    * solution(s1, s2) = 3
    * 
    * Strings have 3 common characters - 2 "a"s and 1 "c"
    *
    * recursivly call head of string unitl last 
    * 
    * val s1 = "aabcc"; val s2 = "adcaa";
    * println(commonCharacterCount(s1, s2))
    * 
    * @param s1
    * @param s2
    * @return
    */
  def commonCharacterCount(s1: String, s2: String): Int = {
    s1.intersect(s2).length
  }
  def commonCharacterCount2(s1: String, s2: String): Int = {
    val map1 = mapToHashmap(s1.toSeq)
    val map2 = mapToHashmap(s2.toSeq)
    (map1.keySet intersect map2.keySet)
      .toList
        .map( key => (map1(key) min map2(key)) )
          .foldLeft(0)(_ + _)
  } 
  def mapToHashmap(stringSeq: Seq[Char]): Map[Char, Int] = {
      stringSeq
        .foldLeft(Map[Char, Int]()
          .withDefaultValue(0)) { case (acc, char) => acc + (char -> (acc(char) + 1))
    }
  } 
  def commonCharacterCount3(s1: String, s2: String): Int = {
    val strBuffer = s2.toBuffer
    s1.foldLeft(0) {
      case (count, ch) =>
        val idx = strBuffer.indexOf(ch)
        if (idx != - 1) {
          strBuffer.remove(idx)
          count + 1
        }
        else count
    }
  }

  /** [isLucky] 
    * https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook/
    * 
    * Ticket numbers usually consist of an even number of digits
    * A ticket number is considered lucky if the sum of the first half of the digits is equal to the sum of the second half
    * 
    * Given a ticket number n, determine if it's lucky or not
    * 
    * [Example]
    * 
    * For (n = 1230), the output should be: (solution(n) = true)
    * 
    * For (n = 239017), the output should be: (solution(n) = false)
    * 
    * [Tests]
    * isLucky(12122)
    * isLucky(134008)
    * 
    * @param n
    * @return
    */
  def isLucky(n: Int): Boolean = {
    val inputAsList = n.toString().map(_.asDigit).toList
    val (a, b) = inputAsList.splitAt(inputAsList.length / 2)
    a.sum == b.sum
  }

  println(s"----<${System.nanoTime() - startTime}>-<ns>-----")
  println(Console.RESET)
}
