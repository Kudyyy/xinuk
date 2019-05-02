package pl.edu.agh.water.model

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, SmellingCell}

final case class OutflowCell(smell: SmellArray) extends SmellingCell {
  override type Self = OutflowCell

  override def withSmell(smell: SmellArray): OutflowCell = copy(smell = smell)
}

trait OutflowAccessible[+T <: GridPart] {
  def withEscape(): T
}
object OutflowAccessible {

  def unapply(arg: EmptyCell)(implicit config: WaterConfig): OutflowAccessible[OutflowCell] =
    new OutflowAccessible[OutflowCell] {
      override def withEscape(): OutflowCell = OutflowCell(arg.smellWith(config.outflowInitialSignal))
    }

  def unapply(arg: GridPart)(implicit config: WaterConfig): Option[OutflowAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }
}
