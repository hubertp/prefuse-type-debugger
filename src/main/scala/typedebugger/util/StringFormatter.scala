package scala.typedebugger
package util

case class StringFormatter(text: List[String], title: Option[String], args: List[CustomArg]) {
  def normalize: String = {
    args match {
      case Nil => text.mkString
      case _   =>
        (text zip args).foldRight(text.last){(x, y) => x._1 + x._2 + y}    
    }
  }  
}

object StringFormatter {
  implicit def toSimpleFormatter(text: String): StringFormatter = {
    StringFormatter(List(text), None, Nil)
  }
  
  implicit def toFormatter(text: String): FormatableText = new FormatableText(text)
  
  class FormatableText(text0: String) {
    
    def dFormat(args: String*): StringFormatter = dFormat(None, (args: _*))
    
    def dFormat(title: Option[String], args: String*): StringFormatter = {
      // count special tags in text0
      // categorize all the arguments appropriately
      val patt = List(TreeText, TypeText, SymText).map(_.tag).mkString("(", "|", ")").r
      val allTags = patt findAllIn text0 toList
      
      if (allTags.length == args.length) {
        val args1 = (allTags zip args) map {
          case (TreeText.tag, arg)  => TreeText(arg)
          case (TypeText.tag, arg)  => TypeText(arg)
          case (SymText.tag, arg)   => SymText(arg)
          case (tag, _)               => throw new Exception("Invalid formatting tag in event description: " + tag)
        }
        StringFormatter(patt split text0 toList, title, args1)
      } else {
        throw new Exception("Number of tags and arguments don't match. Text: " + text0 + "\nArgs: " + args)
      }
    }
  }
}

abstract class CustomArg {
  def text: String
}

trait Tag {
  def tag: String
}

case class TreeText(text: String) extends CustomArg {
  override def toString = text
} 
object TreeText extends Tag {
  val tag = "%tree"
}

case class TypeText(text: String) extends CustomArg {
  override def toString = text
}
object TypeText extends Tag {
  val tag = "%tpe"
}

case class SymText(text: String) extends CustomArg {
  override def toString = text
}
object SymText extends Tag {
  val tag = "%sym"
}
