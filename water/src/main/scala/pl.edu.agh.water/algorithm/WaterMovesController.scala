package pl.edu.agh.water.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.water.model._
import pl.edu.agh.water.simulation.WaterMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random
import scala.math.min

final class WaterMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: WaterConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, WaterMetrics) = {
    val grid = Grid.empty(bufferZone)
    var waterCount = 0L
    var spawnedEs = false
    var spawnedFi = false
    for {
      x <- 1 until config.gridSize - 1
      y <- 1 until config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance && grid.cells(x)(y).isInstanceOf[EmptyCell]) {
        grid.cells(x)(y) =
          random.nextInt(4) match {
            case 0 =>
              if (random.nextDouble() < config.waterSpawnChance) {
                waterCount += 1
                val speed = random.nextInt(config.waterMaxSpeed) + 1
                WaterAccessible.unapply(EmptyCell.Instance).withWater(speed)
              } else {
                grid.cells(x)(y)
              }
            case 1 =>
              if (config.gridSize/2 > x && random.nextDouble() < config.outflowSpawnChance && !spawnedEs) {
                spawnedEs = true
                OutflowAccessible.unapply(EmptyCell.Instance).withOutflow()
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (config.gridSize/2 < x && random.nextDouble() < config.cannonSpawnChance && !spawnedFi) {
                spawnedFi = true
                CannonAccessible.unapply(EmptyCell.Instance).withCannon()
              } else {
                grid.cells(x)(y)
              }
            case 3 =>
              if (random.nextDouble() < config.obstacleSpawnChance) {
                for {
                  i <- x until min(x + config.heightOfObstacle, config.gridSize - 1)
                  j <- y until min(y + config.widthOfObstacle, config.gridSize - 1)
                } grid.cells(i)(j) = Obstacle
                Obstacle
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }

    val metrics = WaterMetrics(waterCount)
    (grid, metrics)
  }


  def calculatePossibleDestinations(cell: WaterCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    var minus = false
    val values = Grid.SubcellCoordinates
      .map {
        case (i, j) => {
          if (cell.smell(i)(j).value < 0.0)
            minus = true
          cell.smell(i)(j)
        }
      }
      .zipWithIndex
    if (minus){
      values.sorted(implicitly[Ordering[(Signal, Int)]])
        .iterator
        .map {
          case (_, idx) =>
            val (i, j) = neighbourCellCoordinates.reverse(idx)
            (i, j, grid.cells(i)(j))
        }
    }
    else {
      values.sorted(implicitly[Ordering[(Signal, Int)]].reverse)
        .iterator
        .map {
          case (_, idx) =>
            val (i, j) = neighbourCellCoordinates(idx)
            (i, j, grid.cells(i)(j))
        }
    }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map {
        case (i, j, current) => (i, j, current, newGrid.cells(i)(j))
      }
      .collectFirstOpt {
        case (i, j, currentCell@WaterAccessible(_), WaterAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, WaterMetrics) = {
    val newGrid = Grid.empty(bufferZone)
    var waterCount = 0L

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            if (Random.nextDouble() < 0.005) newGrid.cells(x)(y) = WaterAccessible.unapply(EmptyCell.Instance).withWater(random.nextInt(config.waterMaxSpeed) + 1)
            else newGrid.cells(x)(y) = cell
          }
        case OutflowCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = OutflowAccessible.unapply(EmptyCell.Instance).withOutflow()
          }
        case CannonCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = CannonAccessible.unapply(EmptyCell.Instance).withCannon()
          }
        case cell: WaterCell =>
          if (Random.nextDouble() < 0.1) {
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          }
          else {
            newGrid.cells(x)(y) match {
              case _ => if (iteration % cell.speed == 0 &&
                            math.abs(cell.smell.map(_.map(_.value).sum).sum/(cell.smell.length * cell.smell.length)) > 0.000000001) {
                moveWater(cell, x, y)
              } else {
                stayInPlace(cell, x, y)
              }
            }
          }
      }
    }

    def stayInPlace(cell: WaterCell, x: Int, y: Int): Unit = {
      newGrid.cells(x)(y) = cell.copy(cell.smell, cell.speed)
      grid.cells(x)(y)
    }

    def moveWater(cell: WaterCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)

      destination match {
        case Opt((i, j, WaterAccessible(dst))) =>
          newGrid.cells(i)(j) = dst.withWater(cell.speed)
        case Opt((i, j, inaccessibleDestination)) =>
          throw new RuntimeException(s"Water selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.smell, cell.speed)
        }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    }
      {
        grid.cells(x)(y) match {
          case WaterCell(_, _) =>
            waterCount += 1
          case _ =>
        }
        makeMove(x, y)
      }

    val metrics = WaterMetrics(waterCount)
    (newGrid, metrics)
  }
}