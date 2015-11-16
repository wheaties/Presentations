def square(x: Int): Int = x*x

val obj = List(1, 2, 3)

val squared = obj.map(square)

def use(f: Int => Int)(obj: List[Int]) = obj.map(f)

def lift(obj: List[A]): List[B] = obj.map(f)

//generalize - part 1
def lift[F[_]] = new{
	def apply[A, B](f: A => B)(implicit fn: Functor[F]) = fn.map(_)(f)
} 

val listSquare: List[Int] => List[Int] = lift[List](square)

//lifts

def addMap(list1: List[Int], list2: List[Int]): List[Int] =
	liftM2(_ + _)(lift1, lift2)

def addMap(list1: List[Int], list2: List[Int]): List[Int] =
	for{
		item1 <- list1
		item2 <- list2
	} yield item1 + item2


def liftM2[A, B, C](f: (A, B) => C) = new{
	def apply[M[_]](ma: M[A], mb: M[B])(implicit m: Monad[M]) =
		m.bind(ma){ a: A => m.map(mb){ b: B => f(a, b) }}
}

def notperfect(fl1: Future[List[Int]], fl2: Future[List[Int]]) = for{
	list1 <- fl1
	list2 <- fl2
} yield liftM2(_ + _)(list1, list2)

def better(fl1: Future[List[Int]], fl2: Future[List[Int]]): Future[List[Int]] =
	liftM2(_ + _)(fl1, fl2)

def apply[G[_], H[_]](gha: G[H[A]])(implicit g: Monad[G], h: Monad[H]) = 
	g.map(gha){ ha: H[A] => h.map(ha)(f) }

//more stuff

def liftM[M[_] : Monad]: M[A] => M[B] =
	{ f: (A => B) => implicitly[Monad[M]].map(_)(f) }

val listSquare: List[Int] => List[Int] = liftM[List](squared)

//more ideas

val listOpt = List(Option(1), None, Option(3))
val stuff = listOpt.map{
	_.map(square)
}

val flOpt = Future{ List(Option(1), None, Option(3)) }
val moreStuff = flOpt.map{ _.map{ _.map(square) } }

val srlsly: Future[Option[Validation[List[Things]]]] = ...

val crazy = liftM[Future] compose 
	liftM[Option] compose 
	liftM[Validation] compose 
	liftM[List]

val yeah = crazy(square)
val orly = yeah(srlsly)

//the big sell

val srlsly: Future[Option[Validation[List[Things]]]] = ...
val yupRly = srlsly.liftMap(square)

whenReady(yupRly){ value =>
	same[Option[Validation[List[Int]]]](value, Some(Success(List(1,4,9))))
}

val flOpt: Future[List[Option[Int]]] = ...

val flInt = flOpt.liftMap{ x: Option[Int] => x.getOrElse(0) }
whenReady(flInt){ value =>
	same[List[Int]](value, List(0))
}

flOpt.liftFlatMap{ x: Int =>
	if(x % 4 == 1) Some(x) else None
}

//mapped

def liftMap[B, C](f: B => C)(implicit lift: LiftF[F[A], B => C]): lift.Out = lift(fa, f)

def liftAp[B, C, M[_]](f: M[B => C])
def liftFlatMap[B, C, M[_]](f: B => M[C])
def liftFoldLeft[B, Z](z: Z)(f: (Z, B) => Z)
def liftFoldRight[B, Z](z: Z)(f: (B, => Z) => Z)
def liftFold
def liftFoldMap[B, C](f: B => C)
def liftFoldAt[M[_]]
def liftFilter[B](f: B => Boolean)
def liftFlatten[M[_]]

//lifters

sealed class LiftedMap[A, B](f: A => B){
	def andThen[C >: B, D](that: LiftedMap[C, D]) = that compose this

	def compose[C, D <: A](that: LiftedMap[C, D]) = that map f

	def map[C](g: B => C): LiftedMap[A, C] = new LiftedMap(f andThen g)

	def apply[That](that: That)(implicit lift: LiftF[That, A => B]): lift.Out = lift(that, f)
}

def liftMap[B, C](f: B => C) = new LiftedMap(f)
def liftAp[A, B, F[_]](f: F[A => B])(implicit ap: Apply[F]) = new LiftedAp(f)
def liftFlatMap[A, B, M[_]](f: A => M[B])(implicit bind: Bind[M]) = new LiftedFlatMap(f)
def liftFoldMap[A, B](f: A => B)(implicit m: Monoid[B]) = new LiftedFoldMap(f)
def liftFilter[A](f: A => Boolean) = new LiftedFilter(f)
def liftA2[A, B, C](f: (A, B) => C) = new LiftedA2(f)
def liftA3[A, B, C, D](f: (A, B, C) => D) = new LiftedA3(f)
def liftM2[A, B, C](f: (A, B) => C) = new LiftedM2(f)
def liftM3[A, B, C, D](f: (A, B, C) => D) = new LiftedM3(f)

val add = liftM2{ (x: Int, y: Int) => x+y }

val one = add(List(0, 1), List(1, 2))
val two = add(List(Option(1), None), List(Option(3)))

same[List[Option[Int]]](two, List(Option(4)))

def apply[That](that: That)(implicit lift: LiftF[That, A => B]): lift.Out = 
	lift(that, f)

//final

val out = for{
	item1 <- list1 //flatMap
	item2 <- list2 //map
} yield item1 + item2

val out = for{
	item1 <- futList1 //liftFlatMap
	item2 <- futList2 //liftMap
} yield item1 + item2

//compiler error
Option(List(1)).liftMap{
	case List(v) => v
	case _ => 0
}

//

libraryDependencies += "com.github.wheaties" %% "autolift" % "0.2"

//plugins

resolvers += Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.4.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.5.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.4")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.3")