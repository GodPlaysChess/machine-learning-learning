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


//  val x = linspace(0.0,10.0)
//  p += plot(x, x ^:^ 2.0)
//  p += plot(x, x ^:^ 3.0, '.')
//  f.saveas("lines.png") // save current figure as a .png, eps and pdf also supported

  val data: Stream[IO, DenseVector[Double]] = io.file
    .readAll[IO](Paths.get("src/main/resources/testdata.txt"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(DenseData.vectorized)

  val inputMatrix: DenseMatrix[Double] = DenseMatrix(data.runLog.unsafeRunSync: _*)

  val f = Figure()
  val p = f.subplot(0)
  p.xlabel = "x axis"
  p.ylabel = "y axis"
  val X1 = inputMatrix(::, 0)
  val X2 = inputMatrix(::, 1)
  val Y = inputMatrix(::, 2)
  p += plot(X1, Y, '+', name = "x1")
  p += plot(X2, Y, '+', name = "x2")

  def train(inputMatrix: DenseMatrix[Double]): DenseVector[Double]= {
    val thetas: DenseVector[Double] = DenseVector.zeros(inputMatrix.cols)
    // prepend x0s = 1 to inputMatrix
    val inputMatrixModel: DenseMatrix[Double] = DenseMatrix.horzcat(DenseVector.ones[Double](inputMatrix.rows).toDenseMatrix.t, inputMatrix)
    val readyThetas = refineModel(thetas, inputMatrixModel)


    //    val totalLoss = all.map { case (xx, y) ⇒ hypo(readyThetas)(xx) - y }.sum
    //    println( s"Total loss: $totalLoss" )

    readyThetas
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
      thetasWithExtra: DenseVector[Double] = DenseVector.vertcat(thetas, DenseVector(-1d))
      loss = rowi * thetasWithExtra // loss is a number, so need to downcast from matrix
      xi: DenseVector[Double] = inputMatrix(i, 0 until (inputMatrix.cols - 1)).t // taking into account only X part
    } yield xi * loss).reduce(_ + _)

    val updThetas: DenseVector[Double] = thetas - descent * coef

    if ((thetas - updThetas) forall { s ⇒ Math.abs(s) < err }) updThetas
    else refineModel(updThetas, inputMatrix)
  }

  //
    val readyThetas = train(inputMatrix)
    def predict(xs: DenseVector[Double]) = readyThetas.t * DenseVector.vertcat(DenseVector(1d), xs)
  //
   println(predict(DenseVector(0.44, 0.68))) // should be 511.14
   println(predict(DenseVector(1.0, 0.23)))  // should be 726.69

   println(predict(DenseVector(0.05, 0.54))) // should be 180.38
   println(predict(DenseVector(0.91, 0.91))) // should be 1312.07
   println(predict(DenseVector(0.31, 0.76))) // should be 440.13
   println(predict(DenseVector(0.51, 0.31))) // should be 343.72
}

object DenseData {
  def vectorized(s: String): DenseVector[Double] = {
    DenseVector(s.split(' ').map(_.toDouble))
  }
}
