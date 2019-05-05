package pl.edu.agh.water

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.water.algorithm.WaterMovesController
import pl.edu.agh.water.model.parallel.WaterConflictResolver
import pl.edu.agh.water.model.{CannonCell, OutflowCell, WaterCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

object WaterMain extends LazyLogging {
  private val configPrefix = "water"
  private val metricHeaders = Vector()

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = (cell.smell.map(_.map(_.value).sum).sum/(cell.smell.length * cell.smell.length)).toFloat
    val brightness = Math.pow(math.abs(smellValue), 0.1).toFloat
    if (smellValue < 0) Color.getHSBColor(1f, 1f, brightness) else Color.getHSBColor(0.7f, 0.5f, brightness)
  }

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case WaterCell(_, _) => Color.BLUE
      case CannonCell(_) => Color.YELLOW
      case OutflowCell(_) => Color.RED
      case cell: SmellingCell => cellToColorRegions(cell)
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(configPrefix, metricHeaders, WaterConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new WaterMovesController(_)(_),
      {
        case cell: SmellingCell => cellToColor(cell)
        case Obstacle => Color.WHITE
      }
    ).start()
  }

}

