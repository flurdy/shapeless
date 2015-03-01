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

package shapeless

import scala.language.existentials
import scala.language.experimental.macros

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.api.Universe
import scala.reflect.macros.{ blackbox, whitebox }

import ops.{ hlist, coproduct }

trait K1
trait K1Id extends K1
trait K1Const[T] extends K1
trait K1TC[T[_]] extends K1

trait Generic1[F[_]] {
  type R[t]
  type R1

  def to[T](ft: F[T]): R[T]
  def from[T](rt: R[T]): F[T]
}

object Generic1 extends Generic10 {
  //type Aux[F[_], R0[_]] = Generic1[F] { type R[t] = R0[t] }
  type Aux1[F[_], R0] = Generic1[F] { type R1 = R0 }

  def apply[F[_]](implicit gen: Generic1[F]): Generic1[F] { type R[t] = gen.R[t] ; type R1 = gen.R1 } = gen

  implicit def materialize[T[_]]: Generic1[T] = macro Generic1Macros.materialize[T]
}

trait Generic10 extends Generic11 {
  implicit def id: Generic1[Id] { type R[T] = T :: HNil ; type R1 = K1Id :: HNil } =
    new Generic1[Id] {
      type R[T] = T :: HNil
      type R1 = K1Id :: HNil

      def to[T](t: T): R[T] = t :: HNil
      def from[T](t: R[T]): T = t.head
    }
}

trait Generic11 {
  implicit def const[C]: Generic1[Const[C]#λ] { type R[T] = C :: HNil ; type R1 = K1Const[C] :: HNil } =
    new Generic1[Const[C]#λ] {
      type R[T] = C :: HNil
      type R1 = K1Const[C] :: HNil

      def to[T](t: C): R[T] = t :: HNil
      def from[T](t: R[T]): C = t.head
    }
}

class Generic1Macros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType
  import Flag._

  def materialize[T[_]](implicit tTag: WeakTypeTag[T[_]]): Tree = {
    val tpe = weakTypeOf[T[_]]
    val nme = TypeName(c.freshName)

    if(isReprType(tpe))
      abort("No Generic instance available for HList or Coproduct")

    def mkCoproductCases(tpe: Type, index: Int): (CaseDef, CaseDef) = {
      val param = param1(tpe)

      val name = TermName(c.freshName("pat"))

      def mkCoproductValue(tree: Tree): Tree =
        (0 until index).foldLeft(q"_root_.shapeless.Inl($tree)": Tree) {
          case (acc, _) => q"_root_.shapeless.Inr($acc)"
        }

      val tpeTpt = appliedTypTree1(tpe, param, nme)
      val body = mkCoproductValue(q"$name: $tpeTpt")
      val pat = mkCoproductValue(pq"$name")
      (
        cq"$name: $tpeTpt => $body",
        cq"$pat => $name"
      )
    }

    def mkProductCases(tpe: Type): (CaseDef, CaseDef) = {
      if(tpe =:= typeOf[Unit])
        (
          cq"() => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => ()"
        )
      else if(isCaseObjectLike(tpe.typeSymbol.asClass)) {
        val singleton =
          tpe match {
            case SingleType(pre, sym) =>
              c.internal.gen.mkAttributedRef(pre, sym)
            case TypeRef(pre, sym, List()) if sym.isModule =>
              c.internal.gen.mkAttributedRef(pre, sym.asModule)
            case TypeRef(pre, sym, List()) if sym.isModuleClass =>
              c.internal.gen.mkAttributedRef(pre, sym.asClass.module)
            case other =>
              abort(s"Bad case object-like type $tpe")
          }

        (
          cq"_: $tpe => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => $singleton: $tpe"
        )
      } else {
        val sym = tpe.typeSymbol
        val isCaseClass = sym.asClass.isCaseClass
        def hasNonGenericCompanionMember(name: String): Boolean = {
          val mSym = sym.companion.typeSignature.member(TermName(name))
          mSym != NoSymbol && !isNonGeneric(mSym)
        }

        val binders = fieldsOf(tpe).map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }

        val to =
          if(isCaseClass || hasNonGenericCompanionMember("unapply")) {
            val lhs = pq"${companionRef(tpe)}(..${binders.map(x => pq"${x._1}")})"
            val rhs =
              binders.foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((bound, name, tpe), acc) => q"_root_.shapeless.::($bound, $acc)"
              }
            cq"$lhs => $rhs"
          } else {
            val lhs = TermName(c.freshName("pat"))
            val rhs =
              fieldsOf(tpe).foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((name, tpe), acc) => q"_root_.shapeless.::($lhs.$name, $acc)"
              }
            cq"$lhs => $rhs"
          }

        val from = {
          val lhs =
            binders.foldRight(q"_root_.shapeless.HNil": Tree) {
              case ((bound, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
            }

          val rhs = {
            val ctorArgs = binders.map { case (bound, name, tpe) => Ident(bound) }
            if(isCaseClass || hasNonGenericCompanionMember("apply"))
              q"${companionRef(tpe)}(..$ctorArgs)"
            else
              q"new $tpe(..$ctorArgs)"
          }

          cq"$lhs => $rhs"
        }

        (to, from)
      }
    }

    val (toCases, fromCases) =
      if(isProduct(tpe)) {
        val (to, from) = mkProductCases(tpe)
        (List(to), List(from))
      } else {
        val (to, from) = (ctorsOf1(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled).unzip
        (to, from :+ cq"_ => _root_.scala.Predef.???")
      }

    val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)
    val reprTpt = reprTypTree1(tpe, nme)

    def mkK1(t: Type, param: Type): Tree =
      t match {
        case TypeRef(_, _, List(param)) => tq"_root_.shapeless.K1TC[${t.typeConstructor}]"
        case t if t =:= param => tq"_root_.shapeless.K1Id"
        case t => tq"_root_.shapeless.K1Const[$t]"
      }

    val r1Tpe = {
      val param = param1(tpe)
      if(isProduct(tpe))
        mkHListTypTree(fieldsOf(tpe).map(x => mkK1(x._2, param)))
      else
        mkCoproductTypTree(ctorsOf1(tpe).map(mkK1(_, param)))
    }

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends _root_.shapeless.Generic1[$tpe] {
        type R[$nme] = $reprTpt
        type R1 = $r1Tpe

        def to[$nme](ft: $tpeTpt): R[$nme] = ft match { case ..$toCases }
        def from[$nme](rt: R[$nme]): $tpeTpt = rt match { case ..$fromCases }
      }
      new $clsName()
    """
  }
}
