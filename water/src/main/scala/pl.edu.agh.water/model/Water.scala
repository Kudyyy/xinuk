package pl.edu.agh.water.model

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class WaterCell(smell: SmellArray, crowd : List[WaterCell], speed : Int)(implicit config: WaterConfig) extends SmellingCell {

  override type Self = WaterCell

  override def withSmell(smell: SmellArray): WaterCell = copy(smell = smell)
}

trait WaterAccessible[+T <: GridPart] {
  def withHuman(crowd : List[WaterCell], speed : Int): T
}
object WaterAccessible {

  def unapply(arg: EmptyCell)(implicit config: WaterConfig): WaterAccessible[WaterCell] =
    new WaterAccessible[WaterCell] {
      override def withHuman(crowd: List[WaterCell], speed: Int): WaterCell = WaterCell(arg.smellWith(config.waterInitialSignal), crowd, speed)
    }

  def unapply(arg: OutflowCell): WaterAccessible[OutflowCell] =
    new WaterAccessible[OutflowCell] {
      override def withHuman(crowd: List[WaterCell], speed: Int): OutflowCell = OutflowCell(arg.smell)
    }

  def unapply(arg: CannonCell): WaterAccessible[CannonCell] =
    new WaterAccessible[CannonCell] {
      override def withHuman(crowd: List[WaterCell], speed: Int): CannonCell = CannonCell(arg.smell)
    }

  def unapply(arg: BufferCell)(implicit config: WaterConfig): WaterAccessible[BufferCell] =
    new WaterAccessible[BufferCell] {
      override def withHuman(crowd: List[WaterCell], speed: Int): BufferCell = BufferCell(WaterCell(arg.smellWith(config.waterInitialSignal), crowd, speed))
    }

  def unapply(arg: GridPart)(implicit config: WaterConfig): Option[WaterAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: OutflowCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case cell: CannonCell => Some(unapply(cell))
    case _ => None
  }
}
