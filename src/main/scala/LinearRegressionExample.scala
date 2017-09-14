object LinearRegressionExample extends App {
  // y = B0 + B1 * x

  val dataSet = Map(
    1d → 1d,
    2d → 3d,
    4d → 3d,
    3d → 2d,
    5d → 5d
  )

  def train(data: Map[Double, Double]): Double ⇒ Double = x ⇒ {
    val meanX = data.keys.sum / data.size
    val meanY = data.values.sum / data.size
    val (bl, br) = data.toSeq.map { case (xi, yi) ⇒
      (xi - meanX) * (yi - meanY) -> (xi - meanX) * (xi - meanX)
    }.unzip

    val B1 = bl.sum / br.sum
    val B0 = meanY - B1 * meanX

    x * B1 + B0
  }

  def predict(x: Double): Double = {
    train(dataSet)(x)
  }

  println(predict(3))

}
