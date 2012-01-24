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
  implicit def toExp[R](d: Def[R]): Exp[R] = findOrCreateDefinition(d)
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
  def matrix_new[T](m: Exp[Int], n: Exp[Int])= MatrixNew(m, n)
}
