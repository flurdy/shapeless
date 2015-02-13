package shapeless.examples

import shapeless._
import ops.coproduct.IsCCons
import ops.hlist.IsHCons

trait Generic1[F[_], R[_]] {
  def to[T](ft: F[T]): R[T]
  def from[T](rt: R[T]): F[T]
}

object Generic1 {
  type ListRepr[T] = scala.::[T] :+: Nil.type :+: CNil

  implicit val gen1List: Generic1[List, ListRepr] =
    new Generic1[List, ListRepr] {
      def to[T](ft: List[T]): ListRepr[T] = ???
      def from[T](rt: ListRepr[T]): List[T] = ???
    }

  type ConsRepr[T] = T :: List[T] :: HNil

  implicit val gen1Cons: Generic1[scala.::, ConsRepr] =
    new Generic1[scala.::, ConsRepr] {
      def to[T](ft: scala.::[T]): ConsRepr[T] = ???
      def from[T](rt: ConsRepr[T]): scala.::[T] = ???
    }

  type OptionRepr[T] = Some[T] :+: None.type :+: CNil

  implicit val gen1Option: Generic1[Option, OptionRepr] =
    new Generic1[Option, OptionRepr] {
      def to[T](ft: Option[T]): OptionRepr[T] = ???
      def from[T](rt: OptionRepr[T]): Option[T] = ???
    }

  type SomeRepr[T] = T :: HNil

  implicit val gen1Some: Generic1[Some, SomeRepr] =
    new Generic1[Some, SomeRepr] {
      def to[T](ft: Some[T]): SomeRepr[T] = ???
      def from[T](rt: SomeRepr[T]): Some[T] = ???
    }
}

trait FunctorA[FA] {
  type F[_]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorA2 {
  type Aux[FA, F0[_]] = FunctorA[FA] { type F[t] = F0[t] }

  trait Arbitrary

  implicit def constFunctorA[T]: FunctorA[T] { type F[t] = T } =
    new FunctorA[T] {
      type F[t] = T
      def map[A, B](fa: T)(f: A => B): T = fa
    }
}

trait FunctorA1 extends FunctorA2 {
  implicit val idFunctorA: FunctorA[Arbitrary] { type F[t] = t } =
    new FunctorA[Arbitrary] {
      type F[t] = t
      def map[A, B](fa: A)(f: A => B): B = f(fa)
    }
}

trait FunctorA0 extends FunctorA1 {
  implicit def baseFunctorA[F0[_]]
    (implicit ff: Lazy[Functor[F0]]): FunctorA[F0[Arbitrary]] { type F[t] = F0[t] } =
      new FunctorA[F0[Arbitrary]] {
        type F[t] = F0[t]
        def map[A, B](fa: F[A])(f: A => B): F[B] = ff.value.map(fa)(f)
      }
}

object FunctorA extends FunctorA0 {
  def apply[FA](implicit f: Lazy[FunctorA[FA]]): Aux[FA, f.value.F] = f.value

  implicit def hnil: FunctorA[HNil] =
    new FunctorA[HNil] {
      type F[t] = HNil
      def map[A, B](fa: HNil)(f: A => B): HNil = fa
    }

  implicit def hcons[HTA <: HList, HA, TA <: HList]
    (implicit
      ihc: IsHCons.Aux[HTA, HA, TA],
      fh: Lazy[FunctorA[HA]],
      ft: Lazy[FunctorA[TA] { type F[t] <: HList }]
    ): FunctorA[HTA] { type F[t] = fh.value.F[t] :: ft.value.F[t] } =
    new FunctorA[HTA] {
      type F[t] = fh.value.F[t] :: ft.value.F[t]
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        fh.value.map(fa.head)(f) :: ft.value.map(fa.tail)(f)
    }

  implicit def cnil: FunctorA[CNil] =
    new FunctorA[CNil] {
      type F[t] = CNil
      def map[A, B](fa: CNil)(f: A => B): CNil = fa
    }

  implicit def ccons[HTA <: Coproduct, HA, TA <: Coproduct]
    (implicit
      icc: IsCCons.Aux[HTA, HA, TA],
      fh: Lazy[FunctorA[HA]],
      ft: Lazy[FunctorA[TA] { type F[t] <: Coproduct }]
    ): FunctorA[HTA] { type F[t] = fh.value.F[t] :+: ft.value.F[t] } =
    new FunctorA[HTA] {
      type F[t] = fh.value.F[t] :+: ft.value.F[t]
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        fa match {
          case Inl(ha) => Inl(fh.value.map(ha)(f))
          case Inr(ta) => Inr(ft.value.map(ta)(f))
        }
    }
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  implicit def constFunctor[T]: Functor[Const[T]#λ] =
    new Functor[Const[T]#λ] {
      def map[A, B](t: T)(f: A => B): T = t
    }

  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }

  implicit def mkFunctor[F[_], R[_]]
    (implicit
      gen: Generic1[F, R],
      far: Lazy[FunctorA[R[FunctorA.Arbitrary]] { type F[t] = R[t] }]
    ): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        gen.from(far.value.map(gen.to(fa))(f))
    }
}

case class Foo[T](t: T)
object Foo {
  type FooRepr[T] = T :: HNil
  implicit val gen1: Generic1[Foo, FooRepr] =
    new Generic1[Foo, FooRepr] {
      def to[T](ft: Foo[T]): FooRepr[T] = ???
      def from[T](rt: FooRepr[T]): Foo[T] = ???
    }
}

trait Box[T]
object Box {
  implicit def bf: Functor[Box] =
    new Functor[Box] {
      def map[A, B](fa: Box[A])(f: A => B): Box[B] = ???
    }
}

case class Bar[T](t: Box[T])
object Bar {
  type BarRepr[T] = Box[T] :: HNil
  implicit val gen1: Generic1[Bar, BarRepr] =
    new Generic1[Bar, BarRepr] {
      def to[T](ft: Bar[T]): BarRepr[T] = ???
      def from[T](rt: BarRepr[T]): Bar[T] = ???
    }
}

case class Baz[T](t: T, s: String)
object Baz {
  type BazRepr[T] = T :: String :: HNil
  implicit val gen1: Generic1[Baz, BazRepr] =
    new Generic1[Baz, BazRepr] {
      def to[T](ft: Baz[T]): BazRepr[T] = ???
      def from[T](rt: BazRepr[T]): Baz[T] = ???
    }
}

sealed trait Cp[+T]
object Cp {
  type CpRepr[T] = CpA[T] :+: CpB[T] :+: CpC.type :+: CpD[T] :+: CNil
  implicit val gen1: Generic1[Cp, CpRepr] =
    new Generic1[Cp, CpRepr] {
      def to[T](ft: Cp[T]): CpRepr[T] = ???
      def from[T](rt: CpRepr[T]): Cp[T] = ???
    }
}

case class CpA[+T](t: T) extends Cp[T]
object CpA {
  type CpARepr[T] = T :: HNil
  implicit val gen1: Generic1[CpA, CpARepr] =
    new Generic1[CpA, CpARepr] {
      def to[T](ft: CpA[T]): CpARepr[T] = ???
      def from[T](rt: CpARepr[T]): CpA[T] = ???
    }
}

case class CpB[+T](t: T) extends Cp[T]
object CpB {
  type CpBRepr[T] = T :: HNil
  implicit val gen1: Generic1[CpB, CpBRepr] =
    new Generic1[CpB, CpBRepr] {
      def to[T](ft: CpB[T]): CpBRepr[T] = ???
      def from[T](rt: CpBRepr[T]): CpB[T] = ???
    }
}

case object CpC extends Cp[Nothing]

case class CpD[+T](t: T, n: Cp[T]) extends Cp[T]
object CpD {
  type CpDRepr[T] = T :: Cp[T] :: HNil
  implicit val gen1: Generic1[CpD, CpDRepr] =
    new Generic1[CpD, CpDRepr] {
      def to[T](ft: CpD[T]): CpDRepr[T] = ???
      def from[T](rt: CpDRepr[T]): CpD[T] = ???
    }
}

object TestFunctor {
  implicitly[FunctorA[Foo.FooRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[Foo]]

  implicitly[FunctorA[Bar.BarRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[Bar]]

  implicitly[FunctorA[Baz.BazRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[Baz]]

  implicitly[FunctorA[CpA.CpARepr[FunctorA.Arbitrary]]]
  implicitly[Functor[CpA]]

  implicitly[FunctorA[CpB.CpBRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[CpB]]

  implicitly[Functor[Const[CpC.type]#λ]]

  implicitly[FunctorA[CpD.CpDRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[CpD]]

  implicitly[FunctorA[Cp.CpRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[Cp]]

  implicitly[FunctorA[Generic1.OptionRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[Option]]

  implicitly[Functor[scala.::]]
  implicitly[Functor[Const[Nil.type]#λ]]

  implicitly[FunctorA[Generic1.ListRepr[FunctorA.Arbitrary]]]
  implicitly[Functor[List]]
}
