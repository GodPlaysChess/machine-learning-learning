package hackerrank

import java.nio.file.Paths

import breeze.linalg.{DenseMatrix, DenseVector}
import cats.effect.IO
import fs2._
import breeze.linalg._
import breeze.numerics._
import breeze.plot._

import scala.annotation.tailrec

object BreezeGradientDescent extends App {
  val learningRate = 0.1d
  val err = 0.000001d

  val data: Stream[IO, DenseVector[Double]] = io.file
    .readAll[IO](Paths.get("src/main/resources/testdata.txt"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(DenseData.vectorized)

  val inputMatrix: DenseMatrix[Double] = DenseMatrix(data.runLog.unsafeRunSync: _*)


  def train(inputMatrix: DenseMatrix[Double]): DenseVector[Double] ⇒ Double = xs ⇒ {
    val thetas: DenseVector[Double] = DenseVector.zeros(inputMatrix.cols)
    val readyThetas = refineModel(thetas, inputMatrix)


    //    val totalLoss = all.map { case (xx, y) ⇒ hypo(readyThetas)(xx) - y }.sum
    //    println( s"Total loss: $totalLoss" )

    readyThetas.t * xs
  }

              /*
              * Algorithm:
              * Tj := Tj - a/m * Sum( (hypo(xi) - yi)*xji )
              * */
  @tailrec
  def refineModel(thetas: DenseVector[Double], inputMatrix: DenseMatrix[Double]): DenseVector[Double] = {
    val coef: Double = learningRate / inputMatrix.rows

    val descent: DenseVector[Double] = (for {
      i ← 0 until inputMatrix.rows
      rowi: Transpose[DenseVector[Double]] = inputMatrix(i, ::)
      loss = (thetas + -1d) * rowi // this is a number, so need to downcast from matrix
      l: Double = loss(0, 0)
      x: Transpose[DenseVector[Double]] = inputMatrix(i, 0 until (inputMatrix.cols - 1)) // taking into account only X part
    } yield x.t * l).reduce(_ + _)

    val updThetas: DenseVector[Double] = thetas - descent * coef

    if ((thetas - updThetas) forall { s ⇒ Math.abs(s) < err }) updThetas
    else refineModel(updThetas, inputMatrix)
  }

  //
  //  val predict = train(data)
  //
  //  println(predict(Vector(0.44, 0.68))) // should be 511.14
  //  println(predict(Vector(1.0, 0.23)))  // should be 726.69

  //  println(predict(Vector(0.05, 0.54))) // should be 180.38
  //  println(predict(Vector(0.91, 0.91))) // should be 1312.07
  //  println(predict(Vector(0.31, 0.76))) // should be 440.13
  //  println(predict(Vector(0.51, 0.31))) // should be 343.72
}

object DenseData {
  def vectorized(s: String): DenseVector[Double] = {
    DenseVector(s.split(' ').map(_.toDouble))
  }
}