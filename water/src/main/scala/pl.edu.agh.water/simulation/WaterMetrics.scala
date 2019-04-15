package pl.edu.agh.water.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class WaterMetrics(peopleCount: Long,
                              fireCount: Long,
                              escapeCount: Long,
                              peopleDeaths: Long,
                              peopleEscaped: Long) extends Metrics {
  override def log: String = {
    s"$peopleCount;$fireCount;$escapeCount;$peopleDeaths;$peopleEscaped"
  }

  override def series: Vector[(String, Double)] = Vector(
    "People" -> peopleCount,
    "Fire" -> fireCount,
    "Escape" -> escapeCount,
    "PeopleDeaths" -> peopleDeaths
  )

  override def +(other: Metrics): WaterMetrics = {
    other match {
      case WaterMetrics.EMPTY => this
      case WaterMetrics(otherPeopleCount, otherFireCount, otherEscapeCount, otherPeopleDeaths, otherPeopleEscaped) =>
        WaterMetrics(peopleCount + otherPeopleCount, fireCount + otherFireCount, escapeCount + otherEscapeCount,
          peopleDeaths + otherPeopleDeaths, peopleEscaped + otherPeopleEscaped)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-WaterMetrics to WaterMetrics")
    }
  }
}

object WaterMetrics {
  private val EMPTY = WaterMetrics(0, 0, 0, 0, 0)

  def empty(): WaterMetrics = EMPTY
}
