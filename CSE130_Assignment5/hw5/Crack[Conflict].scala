import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  
  def apply(line: String) : Entry = {
    val lines = line.split(":")
    Entry(lines(0),lines(1),Integer.valueOf(lines(2)),Integer.valueOf(lines(3)),lines(4),lines(5),lines(6))
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    List(w,w.reverse).iterator
  }
  
  def transformCapitalize(w: String) : Iterator[String] = {
    if (w == "")
    {
      Iterator("")
    }
    else
    {
      val head = List(w.head,w.head.toUpper).iterator
      for (hd <- head; tl <- transformCapitalize(w.tail))
        yield (hd+tl)
    }
  }
  
  def transformDigits(w:String) : Iterator[String] = {
    if (w == "")
    {
      Iterator("")
    }
    else
    {
      val head = transformDigitsHelper(w.head)
      for (hd <- head; tl <- transformDigits(w.tail))
        yield(hd+tl)
    }
  }

  def transformDigitsHelper(w:Char) : Iterator[Char] = 
    w.toLower match {
      case 'o' => Iterator(w,'0')
      case 'z' => Iterator(w,'2')
      case 'a' => Iterator(w,'4')
      case 'b' => Iterator(w,'6','8')
      case 'g' => Iterator(w,'9')
      case 'q' => Iterator(w,'9')
      case 'i' => Iterator(w,'1')
      case 'l' => Iterator(w,'1')
      case 'e' => Iterator(w,'3')
      case 's' => Iterator(w,'5')
      case 't' => Iterator(w,'7')
      case _   => Iterator(w)
    }

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    val users = Lines.list(pwdFile).map(Entry.apply)
    val words = candidateWords(wordsFile).toList.sortBy(_.length())
    val writer = new PrintWriter(outFile)
    var foundUsers = List[String]()
    for(user <- users) {
      for(b <- 0 until words.length) {
          val wordList = words.toIterator
          if(foundUsers.contains(user.account) == false && crackPW(wordList, user.password)==true) {
            foundUsers = user.account :: foundUsers
            println(user.account + "=" + words(b))
            writer.write(user.account + "=" + words(b) + "\n")
            writer.flush()
        }
      }
    }
    println("Finished initial pass. Running through transformations.  ")
    /*for(b <- 0 until words.length){
      for(user <- users){
        val wordList = (transformCapitalize(words(b)).toList ::: transformDigits(words(b)).toList).toIterator
        if(foundUsers.contains(user.account) == false && crackPW(wordList, user.password)==true){
          foundUsers = user.account :: foundUsers
          println(user.account+"="+words(b))
          writer.write((user.account+"="+words(b) + "\n"))
          writer.flush()
        }
      }
    }*/

  }
   def crackPW(word:Iterator[String], password:String):Boolean = {

    for(enc <- word){
      if(checkPassword(enc, password) == true){
        return true
      }
    }
    return false
  }
  
  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:

