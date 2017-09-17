package apitests

import breeze.linalg.DenseVector
import org.scalatest.{FlatSpec, Matchers}
import breeze.linalg._
import breeze.numerics._
import breeze.plot._


class SimpleMatrixOperationTests extends FlatSpec with Matchers with VectorsFixture {

  "breeze" should "dot and scalar multiply" in {
    vector1 dot vector2 shouldBe 550
    vector1.t * vector2 shouldBe 550
  }

  it should "multiply by number" in {
    vector1 * 5 shouldBe DenseVector(5, 10, 15, 20, 25)
  }

  it should "add a number" in {
    vector1 + 10 shouldBe DenseVector(11, 12, 13, 14, 15)
  }

  it should "add 2 vectors" in {
    vector1 + vector2 shouldBe DenseVector(11, 22, 33, 44, 55)
  }

  "Matrices" should "be multiplied" in {
    mat1 * mat2 shouldBe DenseMatrix(DenseVector(6, 6), DenseVector(12, 12))
  }


}

trait VectorsFixture {
  val vector1: DenseVector[Int] = DenseVector(1, 2, 3, 4, 5)
  val vector2: DenseVector[Int] = DenseVector(10, 20, 30, 40, 50)

  //  1 1 1
  //  2 2 2
  val mat1 = DenseMatrix(DenseVector(1, 1, 1), DenseVector(2, 2, 2))

  val mat2 = DenseMatrix(DenseVector(1, 1), DenseVector(2, 2), DenseVector(3, 3))
}
