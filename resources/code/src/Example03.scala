object ImplicitAmbiguity {

  class N[T]
  class NC[T] extends N[T]
  class ND[T] extends N[T]
  class NE[T] extends N[T]
  class NF[T] extends N[T]

  class AA[A]
  class BB[A]

  class Foo

  implicit def conv1(i: Float) = new NC[Float]
  implicit def conv2(i: Float) = new NF[Float]
  implicit def conv3(op: AA[String]) = new N[String]
  implicit def conv4(op: AA[Float]) = new N[Float]
  implicit def conv5(op: AA[Foo]) = new N[Foo]

  implicit def conv6(e: BB[String]) = new N[String]

  def aFunc[A](a: NC[A]) = new AA[A]

  def aFunc[A](a: NF[A]) = new AA[Float]

  def aFunc[A](a: ND[A]) = new BB[A]

  def aFunc[A](a: NE[A]) = new BB[A]

  def bFunc[T](e1: N[T]) = {}
  
  // Fails
  def bar01 {
    val x = aFunc(4F)
    bFunc(x)
  }

  // OK
  def bar02 {
    bFunc(aFunc(4F))
  }
}
