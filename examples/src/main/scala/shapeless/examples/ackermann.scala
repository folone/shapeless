/*
 * Copyright (c) 2011 Miles Sabin 
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

/**
 * Type-level encoding of ackermann function.
 * 
 * @author George Leontiev
 */

object AckermannExamples {
  import shapeless._
  import Nat._

  def typed[T](t : => T) {}

  trait Ackermann[X <: Nat, Y <: Nat] {
    type A <: Nat
  }

  trait AckermannAux[X <: Nat, Y <: Nat, A <: Nat]

  object AckermannAux {
    implicit def ack0[N <: Nat] = new AckermannAux[_0, N, Succ[N]] {}
    implicit def ack1[M <: Nat, V <: Nat](implicit ev : AckermannAux[M, _1, V]) =
      new AckermannAux[Succ[M], _0, V] {}
    implicit def ack2[M <: Nat, N <: Nat, V <: Nat, V1 <: Nat]
      (implicit ev0 : AckermannAux[Succ[M], N, V1], ev1 : AckermannAux[M, V1, V]) =
       new AckermannAux[Succ[M], Succ[N], V] {}
  }

  object Ackermann {
    implicit def ackermann1[X <: Nat, Y <: Nat, A0 <: Nat](implicit ack : AckermannAux[X, Y, A0]) =
      new Ackermann[X, Y] {
        type A = A0
      }

    def ackermann[X <: Nat, Y <: Nat, A <: Nat](x : X, y : Y)(implicit ack : AckermannAux[X, Y, A], a : A) = a
  }

  import Ackermann._

  val a0 = ackermann(_0, _0)
  typed[_1](a0)

  val a1 = ackermann(_1, _1)
  typed[_3](a1)

  val a2 = ackermann(_2, _1)
  typed[_5](a2)

  //val a3 = ackermann(_3, _1)
  //typed[_13](a3)

}
