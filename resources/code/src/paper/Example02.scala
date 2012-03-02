class A {
  def foo[T](a: Int)(b: T): T = b

  def bar {
    val par = foo(10) _
    par(2)
  }
}
