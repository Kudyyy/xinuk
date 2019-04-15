package pl.edu.agh.water.model

import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class HumanCell(smell: SmellArray, crowd : List[HumanCell], speed : Int) (implicit config: WaterConfig) extends SmellingCell {

  override type Self = HumanCell

  override def withSmell(smell: SmellArray): HumanCell = copy(smell = smell)
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(crowd : List[HumanCell], speed : Int): T
}
object HumanAccessible {

  def unapply(arg: EmptyCell)(implicit config: WaterConfig): HumanAccessible[HumanCell] =
    new HumanAccessible[HumanCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): HumanCell = HumanCell(arg.smellWith(config.humanInitialSignal), crowd, speed)
    }

  def unapply(arg: EscapeCell): HumanAccessible[EscapeCell] =
    new HumanAccessible[EscapeCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): EscapeCell = EscapeCell(arg.smell)
    }

  def unapply(arg: CannonCell): HumanAccessible[CannonCell] =
    new HumanAccessible[CannonCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): CannonCell = CannonCell(arg.smell)
    }

  def unapply(arg: BufferCell)(implicit config: WaterConfig): HumanAccessible[BufferCell] =
    new HumanAccessible[BufferCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): BufferCell = BufferCell(HumanCell(arg.smellWith(config.humanInitialSignal), crowd, speed))
    }

  def unapply(arg: GridPart)(implicit config: WaterConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case cell: CannonCell => Some(unapply(cell))
    case _ => None
  }
}
