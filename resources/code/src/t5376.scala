object Test {
  object O1 { implicit def f(s: String): Int = 1 }
  object O2 { implicit def f(s: String): Int = 2 }
/*  object O3 { implicit def f1(s: String): Int = 3 }
  object O4 { implicit def f2(s: String): Int = 4 }*/
  
  // Import two implicits with the same name in the same scope.
  def m1 = {
    import O1._
    import O2._

    // Implicit usage compiles.
    "a": Int
    // Explicit call to f fails.
    f("a")
    // ./c.scala:9: error: reference to f is ambiguous;
    // it is imported twice in the same scope by
    // import O2._
    // and import O1._
    //     f("a")
    //     ^
  }

  // Import two with different names in the same scope.
/*  def m2 = {
    import O3._
    import O4._
  
    // Explicit calls compile.
    f1("a")
    f2("a")

    // Implicit usage fails.
    "a": Int
    // ./a.scala:36: error: type mismatch;
    //  found   : String("a")
    //  required: Int
    // Note that implicit conversions are not applicable because they are ambiguous:
    //  both method f1 in object O3 of type (s: String)Int
    //  and method f2 in object O4 of type (s: String)Int
    //  are possible conversion functions from String("a") to Int
    //     "a": Int
    //     ^
  }*/
}
