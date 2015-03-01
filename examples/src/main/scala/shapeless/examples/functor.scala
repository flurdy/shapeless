/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.examples

import shapeless._
import ops.coproduct.IsCCons
import ops.hlist.IsHCons
import test._

package FunctorDemoDefns {
  case class Foo[T](t: T, ts: List[T])
  case class Foo2[T](i: Int, t: T, ts: Tree[T], t2: T, l: List[T], o: Option[T], s: String)

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  sealed trait Bar[T]
  case class Bar0[T](t: T) extends Bar[T]
  case class Bar1[T](t: T, u: T) extends Bar[T]
}

object FunctorDemo extends App {
  import Functor._
  import FunctorDemoDefns._

  Functor[Foo2]
  val foo2 = Foo2(23, "foo", Node(Leaf("quux"), Leaf("wibble")), "bar", List("foo", "bar", "quux"), None, "blah blah")
  println(foo2.map(_.length))
  

  def transform[F[_]: Functor, A, B](ft: F[A])(f: A => B): F[B] = ft.map(f)

  Functor[Const[None.type]#λ]
  Functor[Some]
  Functor[Option]
  Functor[Leaf]
  Functor[Tree]

  Functor[Bar0]
  Functor[Bar1]
  Functor[Bar]

  FunctorAux[HNil]
  FunctorAux[K1Const[Int] :: HNil]
  FunctorAux[K1Id :: HNil]
  FunctorAux[K1Id :: K1Id :: HNil]

  FunctorAux[CNil]
  FunctorAux[K1Const[Int] :+: CNil]
  FunctorAux[K1Id :+: CNil]
  FunctorAux[K1Id :+: K1Id :+: CNil]

  Functor[Option]
  Functor[List]
  Functor[Tree]
  Functor[Foo2]

  val gen = Generic1[Bar1]
  val b0 = gen.to(Bar1(13, 23))
  typed[Int :: Int :: HNil](b0)
  val b1 = gen.from(b0)
  typed[Bar1[Int]](b1)

  // Option has a Functor
  val o = transform(Option("foo"))(_.length)
  assert(o == Some(3))

  // List has a Functor
  val l = transform(List("foo", "wibble", "quux"))(_.length)
  assert(l == List(3, 6, 4))

  // Any case class has a Functor
  val foo = Foo("Three", List("French", "Hens"))

  val f0 = transform(foo)(_.length)
  val f1 = foo.map(_.length)           // they also have Functor syntax ...

  val expectedFoo = Foo(5, List(6, 4))
  assert(f0 == expectedFoo)
  assert(f1 == expectedFoo)

  // Any ADT has a Functor ... even with recursion
  val tree =
    Node(
      Leaf("quux"),
      Node(
        Leaf("foo"),
        Leaf("wibble")
      )
    )

  val t0 = transform(tree)(_.length)
  val t1 = tree.map(_.length)          // they also have Functor syntax ...

  val expectedTree =
    Node(
      Leaf(4),
      Node(
        Leaf(3),
        Leaf(6)
      )
    )
  assert(t0 == expectedTree)
  assert(t1 == expectedTree)
}

/**
 * Illustrative subset of the Cats Functor type class
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor extends Functor0 {
  def apply[F[_]](implicit f: Lazy[Functor[F]]): Functor[F] = f.value

  implicit def mkFunctor[F[_], R1]
    (implicit
      gen: Generic1.Aux1[F, R1],
      far: Lazy[FunctorAux[R1]]
    ): Functor[F] =
    new Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = ???
        //gen.from(far.value.map(gen.to(fa))(f))
    }

  // Functor syntax
  implicit def functorSyntax[F[_]: Functor, A](fa: F[A]): FunctorOps[F, A] =
    new FunctorOps[F, A](fa)

  class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }
}

trait Functor0 extends Functor1 {
  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def map[A, B](a: A)(f: A => B): B = f(a)
    }
}

trait Functor1 {
  implicit def constFunctor[T]: Functor[Const[T]#λ] =
    new Functor[Const[T]#λ] {
      def map[A, B](t: T)(f: A => B): T = t
    }
}

trait FunctorAux[F1] {
  type F[_]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorAux {
  type Aux[F1, F0[_]] = FunctorAux[F1] { type F[t] = F0[t] }

  def apply[F1](implicit f: Lazy[FunctorAux[F1]]): Aux[F1, f.value.F] = f.value

  implicit def functorP[L <: HList](implicit fp: FunctorP[L]): Aux[L, fp.F] = ???
  implicit def functorC[C <: Coproduct](implicit fc: FunctorC[C]): Aux[C, fc.F] = ???
}

trait FunctorP[F1] {
  type F[_] <: HList
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorP {
  type Aux[F1, F0[_] <: HList] = FunctorP[F1] { type F[t] = F0[t] }

  def apply[F1](implicit f: Lazy[FunctorP[F1]]): Aux[F1, f.value.F] = f.value

  // Base case for deriving products
  implicit def hnil: FunctorP[HNil] { type F[t] = HNil } =
    new FunctorP[HNil] {
      type F[t] = HNil
      def map[A, B](fa: HNil)(f: A => B): HNil = fa
    }

  // Induction step for products
  implicit def hcons[H <: K1, T <: HList]
    (implicit
      fh: Lazy[FunctorK[H]],
      ft: Lazy[FunctorP[T]]
    ): FunctorP[H :: T] { type F[t] = fh.value.F[t] :: ft.value.F[t] } =
    new FunctorP[H :: T] {
      type F[t] = fh.value.F[t] :: ft.value.F[t]
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        fh.value.F.map(fa.head)(f) :: ft.value.map(fa.tail)(f)
    }
}

trait FunctorC[F1] {
  type F[_] <: Coproduct
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorC {
  type Aux[F1, F0[_] <: Coproduct] = FunctorC[F1] { type F[t] = F0[t] }

  def apply[F1](implicit f: Lazy[FunctorC[F1]]): Aux[F1, f.value.F] = f.value

  // Base case for deriving coproducts
  implicit def cnil: FunctorC[CNil] { type F[t] = CNil } =
    new FunctorC[CNil] {
      type F[t] = CNil
      def map[A, B](fa: CNil)(f: A => B): CNil = fa
    }

  // Induction step for coproducts
  implicit def ccons[H <: K1, T <: Coproduct]
    (implicit
      fh: Lazy[FunctorK[H]],
      ft: Lazy[FunctorC[T]]
    ): FunctorC[H :+: T] { type F[t] = fh.value.F[t] :+: ft.value.F[t] } =
    new FunctorC[H :+: T] {
      type F[t] = fh.value.F[t] :+: ft.value.F[t]
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        fa match {
          case Inl(ha) => Inl(fh.value.F.map(ha)(f))
          case Inr(ta) => Inr(ft.value.map(ta)(f))
        }
    }
}

trait FunctorK[K <: K1] {
  type F[_]
  val F: Functor[F]
}

object FunctorK {
  type Aux[K <: K1, F0[_]] = FunctorK[K] { type F[t] = F0[t] }

  def apply[K <: K1](implicit fk: Lazy[FunctorK[K]]): Aux[K, fk.value.F] = fk.value

  // Case for atoms for which a Functor can be obtained (either independently, or
  // recursively by derivation)
  implicit def baseFunctorK[F0[_]](implicit ff: Lazy[Functor[F0]]): Aux[K1TC[F0], F0] =
      new FunctorK[K1TC[F0]] {
        type F[t] = F0[t]
        val F = Functor[F]
      }

  // FunctorK for Id
  implicit val idFunctorK: Aux[K1Id, Id] =
    new FunctorK[K1Id] {
      type F[t] = t
      val F = Functor[F]
    }

  // FunctorK for Const[T]#λ
  implicit def constFunctorK[T]: Aux[K1Const[T], Const[T]#λ] =
    new FunctorK[K1Const[T]] {
      type F[t] = T
      val F = Functor[F]
    }
}
