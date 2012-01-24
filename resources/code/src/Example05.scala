trait Base {
  type Rep[T]
}

trait Expressions {
  // constants/symbols (atomic)
  abstract class Exp[T]
  // ...
  case class Sym[T](n: Int) extends Exp[T]

  // operations (composite, defined in subtraits)
  abstract class Def[T]

  // additional members for managing encountered definitions
  def findOrCreateDefinition[T](rhs: Def[T]): Sym[T]
  implicit def toExp[T](d: Def[T]): Exp[T] = findOrCreateDefinition(d)
}

trait Matrices {
  abstract class Matrix[T]
}

trait BaseExp extends Base with Expressions {
  type Rep[T] = Exp[T]
}

trait MatrixOps extends Base with Matrices {
  object Matrix {
    def apply[T](numRows: Rep[Int], numCols: Rep[Int]) 
      = matrix_new(numRows, numCols)
  }
  def matrix_new[T](m: Rep[Int], n: Rep[Int]): Rep[Matrix[T]]
}

trait MatrixOpsExp extends BaseExp with MatrixOps {
  case class MatrixNew[T](m: Exp[Int], n: Exp[Int])
    extends Def[Matrix[T]]
  def matrix_new[T](m: Exp[Int], n: Exp[Int]) = MatrixNew[T](m, n) 
}

object MyApplication extends MatrixOps {
  def main(args: Array[String]) {
    val x = Matrix[Int](10,20)
    println(x)
  }
}
