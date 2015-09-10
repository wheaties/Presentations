def square(x: Int): Int = x*x

val obj = List(1, 2, 3)

val squared = obj.map(square)

def use(f: Int => Int)(obj: List[Int]) = obj.map(f)

def lift(obj: List[A]): List[B] = obj.map(f)

def lift[A, B](f: A => B) = new{
	def apply[F[_]](ma: F[A])(implicit fn: Functor[F]) = fn.map(ma)(f)
}