package pl.edu.agh.water.model.parallel

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.water.model.{OutflowCell, WaterCell, CannonCell}
import pl.edu.agh.water.simulation.WaterMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object WaterConflictResolver extends ConflictResolver[WaterConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: WaterConfig): (GridPart, WaterMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), WaterMetrics.empty())
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), WaterMetrics.empty())
      case (OutflowCell(currentSmell), WaterCell(_, _)) =>
        (OutflowCell(currentSmell), WaterMetrics.empty())
      case (WaterCell(_, _), OutflowCell(currentSmell)) =>
        (OutflowCell(currentSmell), WaterMetrics.empty())
      case (CannonCell(currentSmell), WaterCell(_, _)) =>
        (CannonCell(currentSmell), WaterMetrics.empty())
      case (WaterCell(_, _), CannonCell(currentSmell)) =>
        (CannonCell(currentSmell), WaterMetrics.empty())
      case (WaterCell(currentSmell, currentSpeed), WaterCell(incomingSmell, _)) =>
        (WaterCell(currentSmell + incomingSmell, currentSpeed), WaterMetrics.empty())
      case (Obstacle, _) => (Obstacle, WaterMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
