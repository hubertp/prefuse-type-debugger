object A {
/*
 def foo {
   val a = Array(1,2)
   val b = a.toSet
   b.map(x => x)
   ()
 }
*/
 def foo2 {
   val a = Array(1,2)
   a.toList.map(x => x)
   ()
 }

 def bar {
   val a = Array(1,2)
   a.toSet.map(x => x)
   ()
 }

}
