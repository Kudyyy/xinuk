package pl.edu.agh.water

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.water.algorithm.WaterMovesController
import pl.edu.agh.water.model.parallel.WaterConflictResolver
import pl.edu.agh.water.model.{EscapeCell, HumanCell, CannonCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, SmellingCell}

object WaterMain extends LazyLogging {
  private val configPrefix = "water"
  private val metricHeaders = Vector(
    "peopleCount",
    "fireCount",
    "escapeCount",
    "peopleDeaths",
    "peopleEscapes"
  )

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0) Color.getHSBColor(1f, 1f, brightness) else Color.getHSBColor(0.7f, 0.5f, brightness)
//    if (smellValue < 0.00001) {
//      val hue = 1f
//      val saturation = 1f
//      Color.getHSBColor(hue, saturation, brightness)
//    } else if (smellValue < 0.001) {
//      val hue = 0.65f
//      val saturation = 1f
//      Color.getHSBColor(hue, saturation, brightness)
//    } else if (smellValue < 0.1) {
//      val hue = 0.28f
//      val saturation = 1f
//      Color.getHSBColor(hue, saturation, brightness)
//    } else {
//      val hue = 0.11f
//      val saturation = 0.69f
//      Color.getHSBColor(hue, saturation, brightness)
//    }
  }

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case HumanCell(_, _, _) => Color.BLUE
      case CannonCell(_) => Color.RED //cellToColorRegions(cell, 0.7f)
      case EscapeCell(_) => new Color(139, 69, 19)
      case cell: SmellingCell => cellToColorRegions(cell)
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(configPrefix, metricHeaders, WaterConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new WaterMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }

}

