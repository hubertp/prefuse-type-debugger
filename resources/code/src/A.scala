class Foo(v: Int) {
  def talk() {}
}



object T {

  def foo {
    val x: Int = 12
    x.talk()
  }

  def bar {
    implicit def trans(a: Int): Foo = new Foo(a)
    val y: Int = 12
    y.talk()
  }
}
