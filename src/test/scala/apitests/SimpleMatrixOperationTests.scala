package apitests

import breeze.linalg.DenseVector
import org.scalatest.{FlatSpec, Matchers}
import breeze.linalg._
import breeze.linalg.eig.Eig
import breeze.linalg.eigSym.EigSym
import breeze.linalg.svd.SVD
import breeze.numerics._
import breeze.plot._
import org.apache.commons.math3.linear.EigenDecomposition


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

  it should "give cols and rows" in {
    mat1(::, 0) shouldBe DenseVector(1, 2)
    mat1(0, ::) shouldBe DenseVector(1, 1, 1).t
    println(mat1(0, ::).t * 2)
    mat1(0, 0 to 1) shouldBe DenseVector(1, 1).t
  }

  it should "calculate eigenvalues and eigenvectors" in {
    val Eig(values, complex, vectors) = eig(mat3.map(_.toDouble))
    println(values)
    println(complex)
    println(vectors)
  }

  it should "calculate singular value decomposition" in {
    val SVD(left, singular, right) = svd(mat1.map(_.toDouble))
    println(left)
    println(singular)
    println(right)
  }

  it should "prepend the column" in {
    DenseMatrix.horzcat(DenseVector.zeros[Int](2).toDenseMatrix.t, mat1) shouldBe DenseMatrix(DenseVector(0, 1, 1, 1), DenseVector(0, 2, 2, 2))
  }

  "Plot" should "be nice" in {
    1 shouldBe 1
  }


}

trait VectorsFixture {
  val vector1: DenseVector[Int] = DenseVector(1, 2, 3, 4, 5)
  val vector2: DenseVector[Int] = DenseVector(10, 20, 30, 40, 50)

  //  1 1 1
  //  2 2 2
  val mat1 = DenseMatrix(DenseVector(1, 1, 1), DenseVector(2, 2, 2))

  val mat2 = DenseMatrix(DenseVector(1, 1), DenseVector(2, 2), DenseVector(3, 3))

  val mat3 = DenseMatrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
}
