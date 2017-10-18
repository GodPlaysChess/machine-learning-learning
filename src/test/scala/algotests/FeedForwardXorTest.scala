package algotests

import algorithms.FeedForwardXor
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FeedForwardXorTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  "Trained xor" should "behave as the normal XOR" in {
    forAll { (a: Boolean, b: Boolean) ⇒
      FeedForwardXor.trainedXor(a)(b) shouldBe (a ^ b)

    }
  }
}
