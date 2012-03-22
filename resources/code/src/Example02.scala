class A {
  def foo[T](a: Int)(b: T): T = b

  def sth() {
    val another = 12
  }

  def bar {
    val b: Int = 1
    val c: String = "abc"
    val a = foo(10) _
    a(2)
    ()
  }
}
