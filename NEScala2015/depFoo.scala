trait Foo{
  class Bar(x: Int)

  def use[A](bar: Bar)(f: Bar => A): A
}

val foo = new Foo{}
val fooBar = new foo.Bar(1)
val oh = new Foo{}
val dear = new oh.Bar(1)

trait Foo[-T]{
  type R

  def apply(x: T): R
}

def bar(foo: Foo): foo.Bar = foo(42)

trait Foo[-T, +R]{
  def apply(x: T): R
}

trait Function1[-T1, +R] extends AnyRef { self =>

  def apply(v1: T1): R

  def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }

  def compose[A](g: A => T1): A => R = g andThen self

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

//type guards and converters

package scala.collection.generic

trait IsTraversableOnce[Repr]{
  type A
  val conversion: Repr => GenTraversableOnce[A]
}

trait IsTraversableLike[Repr]{
  type A
  def conversion(repr: Repr): GenTraversableLike[A, Repr]
}

trait IsTraversableLike[Repr, A]{
  def conversion(repr: Repr): GenTraversableLike[A, Repr]
}

class Foo[T](x: T){
  def magnitude[A](implicit itl: IsTraversableLike[T, A]) = itl(x).size
}

trait IsHigherKind[MA]{
  type A
  type M[X]

  def from(ma: MA): M[A]
  def to(ma: M[A]): MA
}

//lalalalala

trait Cont[+T, R]{ self =>
  def apply(f: T => R): R

  def map[B](f: T => B): Cont[B, R] = new Cont[B, R]{
    def apply(g: B => R): R = self(f andThen g)
  }

  def flatMap[B](f: T => Cont[B, R]): Cont[B, R] = new Cont[B, R] {
    def apply(g: B => R): R = self(f(_)(g))
  }
}

trait DepCont[+T] { self =>
  def apply(dep: DepFun1[T]): dep.R

  def map(f: DepFun1[T]) = new DepCont[f.R] {
    def apply(dep: DepFun1[f.R]): dep.R = self(f andThen dep)
  }

  def flatMap(f: DepFun1[T])(implicit ids: IsDepCont[f.R]) = new DepCont[ids.R] {
    def apply(g: DepFun1[ids.R]): g.R = self(new DepFun1[T] {
      type R = g.R

      def apply(x: T) = ids(f(x))(g)
    })
  }
}

//signatures

def map(f: DepFun1[T])(implicit tie: Tie[f.R]): M[tie.In]

def flatMap(f: DepFun1[T])(implicit tie: Tie[f.R]): M[tie.In]

//dmap

trait DepCont[T] { self =>
  def apply(dep: DepFun1[T]): dep.R

  def map(f: DepFun1[T])(implicit tie: Tie[f.R]) = dmap(f)(tie)

  def flatMap(f: DepFun1[T])(implicit tie: Tie[f.R]) = dmap(f)(tie)

  def dmap(f: DepFun1[T])(implicit tie: Tie[f.R]) = new DepCont[tie.In]{
    def apply(dep: DepFun1[tie.In]): dep.R = self(new DepFun1[T]{
      type R = dep.R

      def apply(x: T) = tie(f(x), dep)
    })
  }
}

trait Tie[R]{
  type In

  def apply(r: R, f: DepFun1[In]): f.R
}

object Tie extends LowPriorityTie{
  def apply[R](implicit tie: Tie[R]): Aux[R, tie.In] = tie

  implicit def tieFM[R]: Aux[DepCont[R], R] =
    new Tie[DepCont[R]]{
      type In = R

      def apply(r: DepCont[R], f: DepFun1[R]): f.R = r(f)
    }
}

trait LowPriorityTie{
  type Aux[R, In0] = Tie[R]{ type In = In0 }

  implicit def tieF[R]: Aux[R, R] =
    new Tie[R]{
      type In = R

      def apply(r: R, f: DepFun1[R]): f.R = f(r)
    }
}

//reasoning

List[List[A]]

List[List[List[List[A]]]]

StateMonad[S, A]

StateMonad[S1, StateMonad[S2, StateMonad[S3, StateMonad[S4, A]]]]