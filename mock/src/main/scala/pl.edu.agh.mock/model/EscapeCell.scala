package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class EscapeCell(smell: SmellArray) extends SmellingCell {
  override type Self = EscapeCell

  override def withSmell(smell: SmellArray): EscapeCell = copy(smell = smell)
}

object EscapeCell {
  def create(initialSignal: Signal): EscapeCell = EscapeCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}