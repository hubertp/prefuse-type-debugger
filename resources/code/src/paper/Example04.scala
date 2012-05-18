class A { def f: Any }
class B extends A { def f: Int = 5 }
class C extends A { def f: Long = 5L }

object Test {
 def universalComp[T](t1: T, t2: T)(implicit evidence: Ordering[T])=1

 implicit val AOrdering: Ordering[A] = null

 universalComp(new B, new C)
 // error: No implicit Ordering defined for A{def f: AnyVal}.
 //   universalComp(new B, new C)
 //                ^
  
 universalComp[A](new B, new C) // works
}
