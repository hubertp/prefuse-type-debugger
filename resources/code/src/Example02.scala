class A {
  def foo[T >: Int](a: Int)(b: T): T = b

  def bar {
    val a = foo(10) _
    a(2)
  }
}
