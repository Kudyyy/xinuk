package pl.edu.agh.water.model

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.xinuk.model.Cell._
import pl.edu.agh.xinuk.model._

final case class NetCell(smell: SmellArray, speed : Int)(implicit config: WaterConfig) extends SmellingCell {

  override type Self = NetCell

  override def withSmell(smell: SmellArray): NetCell = copy(smell = smell)
}

trait NetAccessible[+T <: GridPart] {
  def withNet(speed : Int): T
}
object NetAccessible {

  def unapply(arg: EmptyCell)(implicit config: WaterConfig): NetAccessible[NetCell] =
    new NetAccessible[NetCell] {
      override def withNet(speed: Int): NetCell = NetCell(arg.smell, speed)
    }

  def unapply(arg: OutflowCell): NetAccessible[OutflowCell] =
    new NetAccessible[OutflowCell] {
      override def withNet(speed: Int): OutflowCell = OutflowCell(arg.smell)
    }

  def unapply(arg: CannonCell): NetAccessible[CannonCell] =
    new NetAccessible[CannonCell] {
      override def withNet(speed: Int): CannonCell = CannonCell(arg.smell)
    }

  def unapply(arg: BufferCell)(implicit config: WaterConfig): NetAccessible[BufferCell] =
    new NetAccessible[BufferCell] {
      override def withNet(speed: Int): BufferCell = BufferCell(NetCell(arg.smell, speed))
    }

  def unapply(arg: GridPart)(implicit config: WaterConfig): Option[NetAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: OutflowCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case cell: CannonCell => Some(unapply(cell))
    case _ => None
  }
}
