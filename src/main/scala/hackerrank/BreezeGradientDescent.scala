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

  def matLoss(thetas: DenseVector[Double], dataset: DenseMatrix[Double]): DenseVector[Double] = {
    // broadcast those thetas and multiply on dataset
//    val lossVector: DenseVector[Double] = DenseMatrix(thetas + (-1d)) dot dataset
//    lossVector
    ???
  }

  @tailrec
  def refineModel(thetas: DenseVector[Double], inputMatrix: DenseMatrix[Double]): DenseVector[Double] = {
    val coef = learningRate / inputMatrix.rows

    val updThetas: DenseVector[Double] = ??? /*DenseVector(for {
      (j, θj) ← thetas.activeIterator
      l = loss(j, thetas, inputMatrix)
    } yield θj - l * coef
    )*/

    if ((thetas - updThetas) forall { s ⇒ Math.abs(s) < err }) updThetas
    else refineModel(updThetas, inputMatrix)
  }

  private def loss(j: Int, thetas: DenseVector[Double], inputMatrix: DenseMatrix[Double]): Double = {
    matLoss(thetas, inputMatrix)
//    for {
//      i ← 0 to inputMatrix.rows
//
//    }
//    val l: DenseVector[Double] = (thetas + (-1d)) dot inputMatrix(j)
//    val xj = 1d +: inputMatrix(j)
    ???
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