package pl.edu.agh.water.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class WaterMetrics(waterCount: Long) extends Metrics {
  override def log: String = {
    s"$waterCount"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Water" -> waterCount
  )

  override def +(other: Metrics): WaterMetrics = {
    other match {
      case WaterMetrics.EMPTY => this
      case WaterMetrics(otherWaterCount) => WaterMetrics(waterCount + otherWaterCount)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-WaterMetrics to WaterMetrics")
    }  }
}

object WaterMetrics {
  private val EMPTY = WaterMetrics(0)

  def empty(): WaterMetrics = EMPTY
}
