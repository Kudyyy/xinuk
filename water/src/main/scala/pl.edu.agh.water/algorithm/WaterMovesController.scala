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

final class WaterMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: WaterConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, WaterMetrics) = {
    val grid = Grid.empty(bufferZone)
    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    var spawnedEs = false
    var spawnedFi = false
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grid.cells(x)(y) =
          random.nextInt(3) match {
            case 0 =>
              if (random.nextDouble() < config.humanSpawnChance) {
                humanCount += 1
                val speed = random.nextInt(config.humanMaxSpeed) + 1
                HumanAccessible.unapply(EmptyCell.Instance).withHuman(List.empty, speed)
              } else {
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.escapeSpawnChance && !spawnedEs) {
                escapesCount += 1
                spawnedEs = true
                EscapeAccessible.unapply(EmptyCell.Instance).withEscape()
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (random.nextDouble() < config.cannonSpawnChance && !spawnedFi) {
                fireCount += 1
                spawnedFi = true
                CannonAccessible.unapply(EmptyCell.Instance).withCannon()
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }

    val metrics = WaterMetrics(humanCount, fireCount, escapesCount, 0, 0)
    (grid, metrics)
  }


  def calculatePossibleDestinations(cell: HumanCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map {
        case (i, j) => cell.smell(i)(j)
      }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map {
        case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map {
        case (i, j, current) => (i, j, current, newGrid.cells(i)(j))
      }
      .collectFirstOpt {
        case (i, j, currentCell@HumanAccessible(_), HumanAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, WaterMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    var peopleDeaths = 0L
    var peopleEscaped = 0L

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
            if (Random.nextDouble() < 0.001) newGrid.cells(x)(y) = HumanAccessible.unapply(EmptyCell.Instance).withHuman(List.empty, random.nextInt(config.humanMaxSpeed) + 1)
            else newGrid.cells(x)(y) = cell
          }
        case EscapeCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = EscapeAccessible.unapply(EmptyCell.Instance).withEscape()
          }
        case CannonCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = CannonAccessible.unapply(EmptyCell.Instance).withCannon()
          }
        case cell: HumanCell =>
          if (Random.nextDouble() < 0.01) {
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          }
          else {
            newGrid.cells(x)(y) match {
              case _ => if (iteration % cell.speed == 0 && cell.smell.map(_.map(_.value).max).max.toFloat > 0.000001) {
                moveHuman(cell, x, y)
              } else {
                stayInPlace(cell, x, y)
              }
            }
          }
      }
    }

    def stayInPlace(cell: HumanCell, x: Int, y: Int): Unit = {
      newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
      grid.cells(x)(y)
    }

    def moveHuman(cell: HumanCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      if (cell.crowd.isEmpty) {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.cells(i)(j) = destination.withHuman(cell.crowd, cell.speed)
            newGrid.cells(i)(j) match {
              case EscapeCell(_) | CannonCell(_) => peopleEscaped += 1
              case _ =>
            }
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
        }
      } else {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.cells(i)(j) = destination.withHuman(cell.crowd.head.crowd, cell.crowd.head.speed)
            newGrid.cells(x)(y) = cell.copy(cell.smellWithoutArray(cell.crowd.head.smell), cell.crowd.drop(1), cell.speed)
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
        }
      }

    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      grid.cells(x)(y) match {
        case HumanCell(_, crowd, _) =>
          humanCount += 1 + crowd.size
        case CannonCell(_) =>
          fireCount += 1
        case EscapeCell(_) =>
          escapesCount += 1
        case _ =>
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = WaterMetrics(humanCount, fireCount, escapesCount, peopleDeaths, peopleEscaped)
    (newGrid, metrics)
  }
}