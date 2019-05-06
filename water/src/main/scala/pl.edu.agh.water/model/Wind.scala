package pl.edu.agh.water.model

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, SmellingCell}

final case class WindCell(smell: SmellArray) extends SmellingCell {
  override type Self = WindCell

  override def withSmell(smell: SmellArray): WindCell = copy(smell = smell)
}

trait WindAccessible[+T <: GridPart] {
  def withWind(): T
}
object WindAccessible {

  def unapply(arg: EmptyCell)(implicit config: WaterConfig): WindAccessible[WindCell] =
    new WindAccessible[WindCell] {
      override def withWind(): WindCell = WindCell(arg.smellWith(config.windInitialSignal))
    }

  def unapply(arg: GridPart)(implicit config: WaterConfig): Option[WindAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }
}
