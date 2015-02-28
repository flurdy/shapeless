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

package shapeless.test

import scala.language.experimental.macros

import scala.reflect.macros.{ blackbox, whitebox }

trait TypeTrace[T]
object TypeTrace {
  implicit def apply[T]: TypeTrace[T] = macro TypeTraceMacros.mkTypeTraceImpl[T]
}

trait TypeTrace1[T[_]]
object TypeTrace1 {
  implicit def apply[T[_]]: TypeTrace1[T] = macro TypeTraceMacros.mkTypeTraceImpl1[T]
}

trait TypeTrace2[T[_, _]]
object TypeTrace2 {
  implicit def apply[T[_, _]]: TypeTrace2[T] = macro TypeTraceMacros.mkTypeTraceImpl2[T]
}

class TypeTraceMacros(val c: blackbox.Context) {
  import c.universe._

  def mkTypeTraceImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
    val tTpe = tTag.tpe
    println(s"Trace: $tTpe ${tTpe.getClass.getName}")

    q"""new TypeTrace[$tTpe] {}"""
  }

  def mkTypeTraceImpl1[T[_]](implicit tTag: WeakTypeTag[T[_]]): Tree = {
    val tTpe = tTag.tpe.typeConstructor
    println(s"Trace1: $tTpe ${tTpe.getClass.getName} ${tTpe.takesTypeArgs}")

    q"""new TypeTrace1[$tTpe] {}"""
  }

  def mkTypeTraceImpl2[T[_, _]](implicit tTag: WeakTypeTag[T[_, _]]): Tree = {
    val tTpe = tTag.tpe.typeConstructor
    println(s"Trace2: $tTpe ${tTpe.getClass.getName} ${tTpe.takesTypeArgs}")

    q"""new TypeTrace2[$tTpe] {}"""
  }
}

trait Normalize[T[_], +U[_]]

object Normalize {
  implicit def mkNormalize[T[_]]: Normalize[T, Any] = macro NormalizeMacros.mkNormalizeImpl[T]
}

class NormalizeMacros(val c: whitebox.Context) {
  import c.universe._

  def mkNormalizeImpl[T[_]](implicit tTag: WeakTypeTag[T[_]]): Tree = {
    val tTpe = tTag.tpe.typeConstructor
    println(s"Normalize: $tTpe ${tTpe.getClass.getName} ${tTpe.takesTypeArgs}")

    val nTpe =
      tTpe match {
        case TypeRef(pre, sym, args) =>
          println(s"pre: $pre ${pre.getClass.getName} sym: $sym args: $args")
          sym.infoIn(pre)
        case other =>
          other
      }
    println(s"nTpe: $nTpe")
    println

    q"""new Normalize[$tTpe, $nTpe] {}"""
  }
}
