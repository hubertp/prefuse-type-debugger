package scala.typedebugger
package util

case class StringFormatter(text: List[String], args: List[CustomArg]) {
  def normalize: String = {
    // perform the substitution of tags
    // text.lenght = args.length + 1
    (text zip args).foldRight(text.last){(x, y) => x._1 + x._2 + y}
  }
}

object StringFormatter {
  implicit def toSimpleFormatter(text: String): StringFormatter = {
    StringFormatter(List(text), Nil)
  }
  
  implicit def toFormatter(text: String): FormatableText = new FormatableText(text)
  
  class FormatableText(text0: String) {
    def dFormat(args: String*): StringFormatter = {
      // count special tags in text0
      // categorize all the arguments appropriately
      val patt = ("(" + TreeText.tag + "|" + TypeText.tag + "|" + SymText.tag + ")").r
      val allTags = patt findAllIn text0 toList
      
      if (allTags.length == args.length) {
        val args1 = (allTags zip args) map {
          case (TreeText.tag, arg) => TreeText(arg)
          case (TypeText.tag, arg) => TypeText(arg)
          case (SymText.tag, arg)  => SymText(arg)
        }
        StringFormatter(patt split text0 toList, args1)
      } else {
        println("Invalid formatted text")
        StringFormatter(List(text0), Nil)
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
