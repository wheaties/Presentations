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

def foo[T](that: T)(implicit bar: Bar[T]): bar.R

def foo(that: That)(implicit ord: Ordering[that.R])

//functions

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

trait IsFuture[FA]{
  type A

  def from(ma: FA): Future[A]
  def to(ma: Future[A]): FA
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


//errors

error: No implicit view available from A => scala.collection.GenTraversableOnce[B].
def flatSize[A](l: List[A]) = foo(l, {x: List[A] => x.flatten}).size

//flatten
[B] => Int

def flatten[B](implicit asTraversable: A => GenTraversableOnce[B]): List[B] = {
  val b = genericBuilder[B]
  for (xs <- sequential)
    b ++= asTraversable(xs).seq
  b.result()
}

implicit class ListOps[A](l: List[A]){
  def flatSize: Int = l.flatten.size
}

def flatSize[B](l: List[A])
  (implicit ast: A => GenTraversableOnce[B]): Int = 
    l.flatten.size

def flatten(implicit itl: IsTraversableLike[A]): List[itl.A] ={
  val b = genericBuilder[B]
  for (xs <- sequential)
    b ++= itl.conversion(xs).seq
  b.result()
}

def flatSize(l: List[A])
  (implicit itl: IsTraversableLike[A]): Int =
    l.flatten.size


trait IsMyType[MA]{
  type A

  def apply(ma: MA): SomeType[A]
}

implicit def isMT[A0] = new IsMyType[SomeType[A]]{
  type A = A0

  def apply(ma: MA) = ma
}

//different

def map[B](f: A => B): M[B]
def flatMap[B](f: A => B)(implicit ism: IsM[B]): M[ism.A]

opt map f match{
  case None => //...code
  case Some(Some(x)) => //...code
  case Some(None) => //...!?
}


def map[B](f: T => B)(implicit tie: Tie[B]): M[tie.A] = 
  dmap(f)(tie)
def flatMap[B](f: T => B)(implicit tie: Tie[B]): M[tie.A] = 
  dmap(f)(tie)
def dmap[B](f: T => B)(implicit tie: Tie[B]): M[tie.A]

trait Cont[+T, R]{ self =>
  def apply(f: T => R): R

  def map[B](f: T => B)(implicit tie: Tie[B, R]): Cont[tie.In, R] = dmap(f)(tie)
  def flatMap[B](f: T => B)(implicit tie: Tie[B, R]): Cont[tie.In, R] = dmap(f)(tie)

  def dmap[B](f: T => B)(implicit tie: Tie[B, R]) = new Cont[tie.In, R]{
    def apply(g: tie.In => R): R = self(x => tie(f(x), g))
  }
}

trait Tie[C, R]{
  type In

  def apply(c: C, f: In => R): R
}

object Tie extends LowPriorityTie{
  def apply[C, R](implicit tie: Tie[C, R]): Aux[C, R, tie.In] = tie

  implicit def tieFM[B, R]: Aux[Cont[B, R], R, B] =
    new Tie[Cont[B, R]]{
      type In = B

      def apply(c: Cont[B, R], f: B => R): R = r(f)
    }
}

trait LowPriorityTie{
  type Aux[C, R, In0] = Tie[C, R]{ type In = In0 }

  implicit def tieF[B, R]: Aux[B, R, R] =
    new Tie[B, R]{
      type In = B

      def apply(r: B, f: B => R): R = f(r)
    }
}

def dmap[B](f: T => B)(implicit tie: Tie[B, R]) = new Cont[tie.In, R]{
  def apply(g: tie.In => R): R = self(x => tie(f(x), g))
}

def apply(c: Cont[B, R], f: B => R): R = r(f)

def apply(c: B, f: B => R): R = f(c)

//final discussion

val futfut: Future[Future[Unit]] = yourMethod(f)
val futtry: Future[Try[A]] = yourMethod(t)

futtry onComplete{
  case Failure(ex) => //...
  case Success(Success(x)) => //...
  case Sucess(Failure(ex)) => //...!?
}

def yourMethod[B](f: A => B): Future[B]

def yourMethod[B](f: A => B)(implicit knot: Knot[B]): Future[knot.R]

trait Knot[FA]{
  type R

  def apply[A](in: Future[A], f: A => FA]): Future[R]
}

object Knot extends LowPriorityKnot{
  def apply[FA](implicit knot: Knot[FA]): Aux[FA, knot.R] = knot

  implicit def futFM[B]
    (implicit ex: ExecutionContext): Aux[Future[B], B] =
      new Knot[Future[B]]{
        type R = B

        def apply[A](in: Future[A], fa: A => Future[B]) = in flatMap fa
      }

  implicit def tryM[B]
    (implicit ex: ExecutionContext): Aux[Try[B], B] =
      new Knot[Try[B]]{
        type R = B

        def apply[A](in: Futurte[A], f: A => Try[B]) = in flatMap { x => 
          f(x) match{
            case Success(x) => Future successful x
            case Failure(ex) => Future failed ex
          }
        }
      }
}