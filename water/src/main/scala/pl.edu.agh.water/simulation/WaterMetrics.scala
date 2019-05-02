package pl.edu.agh.water.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class WaterMetrics() extends Metrics {
  override def log: String = {
    s""
  }

  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): WaterMetrics = {
    this
  }
}

object WaterMetrics {
  private val EMPTY = WaterMetrics()

  def empty(): WaterMetrics = EMPTY
}
