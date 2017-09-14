package hackerrank

import java.nio.file.Paths

import cats.effect.IO
import fs2._

import scala.annotation.tailrec

object PolyRegressionProblem extends App {
  val learningRate = 1d

  val data: Stream[IO, (Vector[Double], Double)] = io.file
    .readAll[IO](Paths.get("src/main/resources/testdata.txt"), 4096)
    .through(text.utf8Decode)
    .through(text.lines)
    .map(Data.vectorized)

  // approximation y = B0 + B1*x1 + B2*x2
  // y = B0 + [Xt]*[B]
  // B = [b0, b1, b2 .. bn]
  // h = BT * X
  def train(data: Stream[IO, (Vector[Double], Double)]): Vector[Double] ⇒ Double = {
    /*
    * Calculate the hypothesis h = X * theta
    * Calculate the loss = h - y and maybe the squared cost (loss^2)/2m
    * Calculate the gradient = X' * loss / m
    * Update the parameters theta = theta - alpha * gradient
    * */
    val all: Vector[(Vector[Double], Double)] = data.runLog.unsafeRunSync()
    val (xs, ys): (Vector[Vector[Double]], Vector[Double]) = all.unzip
    val thetas = Vector.fill(xs.head.size + 1)(0d)

    val readyThetas = go(thetas, xs, ys)
    hypo(readyThetas)
  }


  // it is our matrix. xs is [x1, x2] for 2 features
  def hypo(thetas: Vector[Double]): Vector[Double] ⇒ Double = xs ⇒ {
    (thetas zip xs map (d ⇒ d._1 * d._2)).sum
  }

  @tailrec
  def go(thetas: Vector[Double], X: Vector[Vector[Double]], Y: Vector[Double]): Vector[Double] = {
    val x0 = Vector.fill(X.head.size)(1d)
    val xs = x0 +: X
    val err = 0.000001d

    val updThetas = for {
      (θ, xjs) ← thetas zip xs
      coef = learningRate / X.size
      summ = for {
        (xi, yi) ← X zip Y
        xj ← xjs
        diff = hypo(thetas)(xi) - yi
      } yield diff * xj
    } yield θ - summ.sum * coef
    if ((thetas zip updThetas) forall { case (o, n) ⇒ Math.abs(o - n) < err }) updThetas
    else {
//      println(printlns"refined model: $updThetas")
      go(updThetas, X, Y)
    }
  }


  println(train(data)(Vector(0.05, 0.54)))
  println(train(data)(Vector(0.91, 0.91)))
  println(train(data)(Vector(0.31, 0.76)))
  println(train(data)(Vector(0.51, 0.31)))
}

object Data {
  def vectorized(s: String): (Vector[Double], Double) = {
    val x = s.split(' ').map(_.toDouble).toVector
    (x.init, x.last)
  }
}