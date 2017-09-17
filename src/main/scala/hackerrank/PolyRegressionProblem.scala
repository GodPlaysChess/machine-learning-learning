package hackerrank

import java.nio.file.Paths

import breeze.linalg.{DenseVector, Matrix}
import breeze.optimize.FirstOrderMinimizer
import breeze.optimize.FirstOrderMinimizer.State
import cats.effect.IO
import fs2._

import scala.annotation.tailrec

object PolyRegressionProblem extends App {
  val learningRate = 0.1d
  val err = 0.000001d

  val data: Stream[IO, (Vector[Double], Double)] = io.file
    .readAll[IO](Paths.get("src/main/resources/testdata.txt"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(ReadData.vectorized)

  // approximation y = B0 + B1*x1 + B2*x2
  // y = B0 + [Xt]*[B]
  // B = [b0, b1, b2 .. bn]
  // h = BT * X
  def train(data: Stream[IO, (Vector[Double], Double)]): Vector[Double] ⇒ Double = {
    val all: Vector[(Vector[Double], Double)] = data.runLog.unsafeRunSync()
    val (xs, ys): (Vector[Vector[Double]], Vector[Double]) = all.unzip
    val thetas = Vector.fill(xs.head.size + 1)(0d)

    val readyThetas = go(thetas, xs, ys)
    val totalLoss = all.map { case (xx, y) ⇒ hypo(readyThetas)(xx) - y } .sum
    println( s"Total loss: $totalLoss" )
    hypo(readyThetas)
  }

  // it is our matrix. xs is [x1, x2] for 2 features
  def hypo(thetas: Vector[Double]): Vector[Double] ⇒ Double = xs ⇒ {
    (thetas zip xs map (d ⇒ d._1 * d._2)).sum
  }

  // X.size = 100; x.head.size = 3
  @tailrec
  def go(thetas: Vector[Double], X: Vector[Vector[Double]], Y: Vector[Double]): Vector[Double] = {
    val coef = learningRate / Y.size
    val updThetas = for {
      j ← thetas.indices
      θj = thetas(j)  // X - hundred elems, here it should be one  (XJS -> vector of 2 elems)
      l = loss(j, thetas, X, Y)
    } yield {
      θj - l * coef
    }
    val totalLoss = (X zip Y).map {case (xx, y) ⇒ hypo(updThetas.toVector)(xx) - y} .sum
    println(s"updated model: $totalLoss" )
    if ((thetas zip updThetas) forall { case (o, n) ⇒ Math.abs(o - n) < err }) updThetas.toVector
    else go(updThetas.toVector, X, Y)
  }

  private def loss(j: Int, thetas: Vector[Double], X: Vector[Vector[Double]], Y: Vector[Double]): Double = (for {
    i ← Y.indices
    xi = 1d +: X(i)
    l = hypo(thetas)(xi) - Y(i)
  } yield l * xi(j)).sum

  val predict = train(data)

  println(predict(Vector(0.44, 0.68))) // should be 511.14
  println(predict(Vector(1.0, 0.23)))  // should be 726.69

//  println(predict(Vector(0.05, 0.54))) // should be 180.38
//  println(predict(Vector(0.91, 0.91))) // should be 1312.07
//  println(predict(Vector(0.31, 0.76))) // should be 440.13
//  println(predict(Vector(0.51, 0.31))) // should be 343.72
}

object ReadData {
  def vectorized(s: String): (Vector[Double], Double) = {
    val x = s.split(' ').map(_.toDouble).toVector
    (x.init, x.last)
  }
}