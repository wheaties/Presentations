trait Foo{
  class Bar(x: Int)

  def use[A](bar: Bar)(f: Bar => A): A
}

val foo = new Foo{}
val fooBar = new foo.Bar(1)
val oh = new Foo{}
val dear = new oh.Bar(1)

trait Foo{
  type Bar

  def apply(x: Int): Bar
}

def bar(foo: Foo): foo.Bar = foo(42)

trait Foo[Bar]{
  def apply(x: Int): Bar
}

trait Function1[-T1, +R] extends AnyRef { self =>

  def apply(v1: T1): R

  def compose[A](g: A => T1): A => R = { x => apply(g(x)) }

  def andThen[A](g: R => A): T1 => A = g andThen self

  override def toString() = "<function1>"
}

trait DepFun1[-T] { self =>
  type R

  def apply(x: T): R
}

def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }

def andThen(dep: DepFun1[R]) = new DepFun1[T] {
  type R = dep.R

  def apply(x: T): R = dep(self(x))
}

trait DepFun1[-T]{ self =>
  type R

  def andThen(dep: DepFun1[R]) = new DepFun1[T] {
    type R = dep.R

    def apply(x: T): R = dep(self(x))
  }

  def compose[A](dep: DepFun1[A]) = ???

  def apply(x: T): R
}

def compose[A](g: A => T1): A => R = { x => apply(g(x)) }

def compose[A](dep: DepFun1[A])(implicit ev: dep.R <:< T) = new DepFun1[A]{
  type R = self.R

  def apply(x: A): R = self(ev(dep(x)))
 }

trait DepFun1[-T] { self =>
  type R

  def andThen(dep: DepFun1[R]) = new DepFun1[T] {
    type R = dep.R

    def apply(x: T): R = dep(self(x))
  }

  def compose[A](dep: DepFun1[A])(implicit ev: dep.R <:< T) = new DepFun1[A]{
  	type R = self.R

  	def apply(x: A): R = self(ev(dep(x)))
  }

  def apply(x: T): R
}


package scala.collection.generic

trait IsTraversableOnce[Repr]{
  type A
  val conversion: Repr => GenTraversableOnce[A]
}

trait IsTraversableLike[Repr]{
  type A
  def conversion(repr: Repr): GenTraversableLike[A, Repr]
}