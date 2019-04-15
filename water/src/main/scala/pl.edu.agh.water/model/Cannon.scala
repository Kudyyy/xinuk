package pl.edu.agh.water.model

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, SmellingCell}

final case class CannonCell(smell: SmellArray) extends SmellingCell {
  override type Self = CannonCell

  override def withSmell(smell: SmellArray): CannonCell = copy(smell = smell)
}

trait CannonAccessible[+T <: GridPart] {
  def withCannon(): T
}
object CannonAccessible {

  def unapply(arg: EmptyCell)(implicit config: WaterConfig): CannonAccessible[CannonCell] =
    new CannonAccessible[CannonCell] {
      override def withCannon(): CannonCell = CannonCell(arg.smellWith(config.cannonInitialSignal))
    }

  def unapply(arg: GridPart)(implicit config: WaterConfig): Option[CannonAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }
}
