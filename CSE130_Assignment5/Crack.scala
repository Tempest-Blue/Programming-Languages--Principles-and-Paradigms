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
    var users = Lines.list(pwdFile).map(Entry.apply)
    val possiblePasswords = candidateWords(wordsFile).toList.sortBy(_.length())
    val writer = new PrintWriter(outFile)
    var foundUsers = List[String]()

    println("Starting Untransformed\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        val possiblePassword = possiblePasswords.toIterator
        for (password <- possiblePassword) {
          if(checkPassword(password, user.password)) {
            foundUsers = user.account :: foundUsers
            println(user.account + "=" + password)
            writer.write(user.account + "=" + password + "\n")
            writer.flush()
          }
        }
      }
    }

    println("\nStarting Reverse.\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        for (x <- possiblePasswords) {
          val newPossiblePassword = transformReverse(x)
          for (password <- newPossiblePassword) {
            if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
              foundUsers = user.account :: foundUsers
              println(user.account + "=" + password)
              writer.write(user.account + "=" + password + "\n")
              writer.flush()
            }
          }
        }
      }
    }
    
    println ("\nStarting Digit.\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        for (x <- possiblePasswords) {
          val newPossiblePassword = transformDigits(x)
          for (password <- newPossiblePassword) {
            if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
              foundUsers = user.account :: foundUsers
              println(user.account + "=" + password)
              writer.write(user.account + "=" + password + "\n")
              writer.flush()
            }
          }
        }
      }
    }

    println ("\nStarting Capitalize.\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        for (x <- possiblePasswords) {
          val newPossiblePassword = transformCapitalize(x)
          for (password <- newPossiblePassword) {
            if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
              foundUsers = user.account :: foundUsers
              println(user.account + "=" + password)
              writer.write(user.account + "=" + password + "\n")
              writer.flush()
            }
          }
        }
      }
    }

    println ("\nStarting reverseDigit/reverseCapitalize.\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        for (x <- possiblePasswords) {
          val newPossiblePassword = transformReverse(x)
          for (y <- newPossiblePassword) {
            val newnewPossiblePassword = transformDigits(y)
            for (password <- newPossiblePassword) {
              if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
                foundUsers = user.account :: foundUsers
                println(user.account + "=" + password)
                writer.write(user.account + "=" + password + "\n")
                writer.flush()
              }
            }
          }
          for (z <- newPossiblePassword) {
            val newnewPossiblePassword = transformCapitalize(z)
            for (password <- newPossiblePassword) {
              if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
                foundUsers = user.account :: foundUsers
                println(user.account + "=" + password)
                writer.write(user.account + "=" + password + "\n")
                writer.flush()
              }
            }
          }
        }
      }
    }

    println ("\nStarting digitReverse/digitCapitalize.\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        for (x <- possiblePasswords) {
          val newPossiblePassword = transformDigits(x)
          for (y <- newPossiblePassword) {
            val newnewPossiblePassword = transformReverse(y)
            for (password <- newPossiblePassword) {
              if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
                foundUsers = user.account :: foundUsers
                println(user.account + "=" + password)
                writer.write(user.account + "=" + password + "\n")
                writer.flush()
              }
            }
          }
          for (z <- newPossiblePassword) {
            val newnewPossiblePassword = transformCapitalize(z)
            for (password <- newPossiblePassword) {
              if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
                foundUsers = user.account :: foundUsers
                println(user.account + "=" + password)
                writer.write(user.account + "=" + password + "\n")
                writer.flush()
              }
            }
          }
        }
      }
    }

    println ("\nStarting capitalizeReverse/capitalizeDigit.\n")
    for(user <- users) {
      if (!(foundUsers contains user.account)) {
        for (x <- possiblePasswords) {
          val newPossiblePassword = transformCapitalize(x)
          for (y <- newPossiblePassword) {
            val newnewPossiblePassword = transformReverse(y)
            for (password <- newPossiblePassword) {
              if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
                foundUsers = user.account :: foundUsers
                println(user.account + "=" + password)
                writer.write(user.account + "=" + password + "\n")
                writer.flush()
              }
            }
          }
          for (z <- newPossiblePassword) {
            val newnewPossiblePassword = transformDigits(z)
            for (password <- newPossiblePassword) {
              if(!(foundUsers contains user.account) && checkPassword(password, user.password)) {
                foundUsers = user.account :: foundUsers
                println(user.account + "=" + password)
                writer.write(user.account + "=" + password + "\n")
                writer.flush()
              }
            }
          }
        }
      }
    }
  }

  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et: