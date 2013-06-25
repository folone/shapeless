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

package shapeless

import org.junit.Test
import org.junit.Assert._

class SNatTests {
  @Test
  def testSNat {
    import SNat._
    import SingletonTypes._

    val s1 = ^+(1, 3)
    val s2 = ^-(3, 2)
    val s3 = ^*(2, 3)
    val s4 = ^/(10, 5)
    val s5 = ^%(3, 2)

    implicitly[s1.Result =:= ^(4)]
    implicitly[s2.Result =:= ^(1)]
    implicitly[s3.Result =:= ^(6)]
    implicitly[s4.Result =:= ^(2)]
    implicitly[s5.Result =:= ^(1)]

    val c1 = ^<(3, 2)
    val c2 = ^<=(2, 3)
    val c3 = ^>(2, 3)
    val c4 = ^>=(3, 2)

    implicitly[c1.Result =:= ^(false)]
    implicitly[c2.Result =:= ^(true)]
    implicitly[c3.Result =:= ^(false)]
    implicitly[c4.Result =:= ^(true)]

    val max1 = Max(3, 2)
    val min1 = Min(3, 2)

    implicitly[max1.Result =:= ^(3)]
    implicitly[min1.Result =:= ^(2)]

    val p1 = Pow(3, 2)
    implicitly[p1.Result =:= ^(9)]

    val g1 = GCD(15, 6)
    val g2 = GCD(4, 10)
    val g3 = GCD(2, 3)

    implicitly[g1.Result =:= ^(3)]
    implicitly[g2.Result =:= ^(2)]
    implicitly[g3.Result =:= ^(1)]

    val a1 = Ack(0, 0)
    val a2 = Ack(1, 1)
    val a3 = Ack(2, 1)
    val a4 = Ack(3, 1)
    val a5 = Ack(2, 2)

    implicitly[a1.Result =:= ^(1)]
    implicitly[a2.Result =:= ^(3)]
    implicitly[a3.Result =:= ^(5)]
    implicitly[a4.Result =:= ^(13)]
    implicitly[a5.Result =:= ^(7)]
  }
}
