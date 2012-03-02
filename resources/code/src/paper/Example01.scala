class Test {
  class Foo(val x: Int)

  def withoutImplicit {
    val a = new Foo(1)
    val b: Int = a  // Found Foo, expected Int.
  }

  def withImplicit {
    implicit def fromFooToInt(f: Foo):Int =f.x

    val a = new Foo(1)
    val b: Int = a
  }
}
