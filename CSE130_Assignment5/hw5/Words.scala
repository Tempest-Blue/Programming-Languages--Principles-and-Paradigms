import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] =  
    iterator(file).map(_.toLowerCase()).toIterator;
  
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] =
  {
    var freqMap = new HashMap[B,Int]
    for (c <- xs) {
      freqMap = freqMap + (f(c) -> (freqMap.getOrElse(f(c),0) + 1))
    }
    freqMap
  }

  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = 
  {
    val words = apply(file)
    groupFreq[String,Int](words,(x:String)=>x.length)
  }
    
  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars = iterator(file).mkString.toIterator
    groupFreq[Char,Char](chars,(x:Char)=>x.toLower)
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = 
   apply(file).toList.filter(x=>x.length == size).toIterator

  def wordsWithAllVowels(file: String): Iterator[String] = 
    apply(file).toList.filter(x => (x contains 'a')&&(x contains 'e')&&(x contains 'i')&&(x contains 'o')&&(x contains 'u')).toIterator
 
  def wordsWithNoVowels(file: String): Iterator[String] = 
    apply(file).toList.filter(x => (!(x contains 'a'))&&(!(x contains 'e'))&&(!(x contains 'i'))&&(!(x contains 'o'))&&(!(x contains 'u'))).toIterator
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = 
    apply(file).toList.filter(x => (re findAllIn x).size >= 1).toIterator

}

// vim: set ts=2 sw=2 et:

