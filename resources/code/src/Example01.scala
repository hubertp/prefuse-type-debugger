object SimpleExample {

  class Foo(val x: Int)

  def test01 {
    val a = new Foo(1)
    val b: Int = a
    ()
  }

  def test02 {
    implicit def fromFooToInt(x: Foo): Int = x.x

    val a = new Foo(1)
    val b: Int = a
    ()
  }

}
