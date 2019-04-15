package pl.edu.agh.water.model.parallel

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.water.model.{EscapeCell, HumanCell, CannonCell}
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
      case (EscapeCell(currentSmell), HumanCell(_, _, _)) =>
        (EscapeCell(currentSmell), WaterMetrics(0, 0, 0, 0, 1))
      case (CannonCell(currentSmell), HumanCell(_, _, _)) =>
        (CannonCell(currentSmell), WaterMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, currentCrowd, currentSpeed), another@HumanCell(incomingSmell, incomingCrowd, _)) =>
        (HumanCell(currentSmell + incomingSmell, currentCrowd ++ incomingCrowd ++ List(another), currentSpeed), WaterMetrics.empty())
      case (Obstacle, _) => (Obstacle, WaterMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
