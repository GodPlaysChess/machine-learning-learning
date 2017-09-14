import breeze.linalg.Matrix

import scala.reflect.io.File

object Main extends App {

  // first you shall import your dataset
  def readDataSet(file: File): List[Matrix[Double]] = ???



  // Here's some configuration
  val numExamples: Long = ???   // dataSet Size
  val inputDimensions: Int = 2
  val outputDimensions: Int = 2

  // Gradient descent parameters
  val ε: Double = 0.01  // learning rate for gradient descent
  val λ: Double = 0.01  // regularization strength


  type Model = Matrix[Double]

  def calculateLoss(model: Model, dataSet: Model): Double = {
    ???
  }

}
