package pl.edu.agh.mock.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

//    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal)

//    for {
//      x <- 0 until config.gridSize
//      y <- 0 until config.gridSize
//      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
//    } {
//      grid.cells(x)(y) = MockCell.create(config.mockInitialSignal)
//    }

    for {
      y <- 1 until config.gridSize - 1
      x <- 1 until config.gridSize - 1
    } {
      val rand = random.nextDouble()
      if (rand < config.escapeSpawnChance)
        grid.cells(x)(y) = EscapeCell.create(config.escapeInitialSignal)
      else if (rand < config.waterSpawnChance)
        grid.cells(x)(y) = MockCell.create(config.mockInitialSignal)
    }

    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  def calculatePossibleDestinations(cell: MockCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
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
        case (i, j, currentCell@MockCell(_), MockCell(_)) =>
          (i, j, currentCell)
      }
  }

//  def moveMock(cell: MockCell, x: Int, y: Int): Unit = {
//    val destinations = calculatePossibleDestinations(cell, x, y, grid)
//    val destination = selectDestinationCell(destinations, newGrid)
//    if (cell.crowd.isEmpty) {
//      destination match {
//        case Opt((i, j, HumanAccessible(destination))) =>
//          newGrid.cells(i)(j) = destination.withHuman(cell.crowd, cell.speed)
//          newGrid.cells(i)(j) match {
//            case EscapeCell(_) => peopleEscaped += 1
//            case _ =>
//          }
//        case Opt((i, j, inaccessibleDestination)) =>
//          throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
//        case Opt.Empty =>
//          newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
//      }
//    } else {
//      destination match {
//        case Opt((i, j, HumanAccessible(destination))) =>
//          newGrid.cells(i)(j) = destination.withHuman(cell.crowd.head.crowd, cell.crowd.head.speed)
//          newGrid.cells(x)(y) = cell.copy(cell.smellWithoutArray(cell.crowd.head.smell), cell.crowd.drop(1), cell.speed)
//        case Opt((i, j, inaccessibleDestination)) =>
//          throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
//        case Opt.Empty =>
//          newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
//      }
//    }
//
//  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      grid.cells(x)(y) match {
        case cell: MockCell =>
          val destinations = calculatePossibleDestinations(cell, x, y, grid)
          val destination = selectDestinationCell(destinations, newGrid)
          destination match {
            case Opt((i, j, MockCell(_))) =>
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(i)(j) = cell.copy()
            case Opt.Empty =>
              newGrid.cells(x)(y) = cell.copy()
          }
//          newGrid.cells(x)(y) = cell

        //          if (x < config.gridSize - 2) {
////            if (grid.cells(x + 1)(y) != Obstacle)
//              newGrid.cells(x + 1)(y) = cell
////            else
////              newGrid.cells(x)(y) = cell
//          }
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell: EscapeCell =>
          newGrid.cells(x)(y) = EscapeCell.create(config.escapeInitialSignal)
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y))
            newGrid.cells(x)(y) = cell

      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)




//    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
//      val destination = (x + random.nextInt(3) - 1, y + random.nextInt(3) - 1)
//      val vacatedCell = EmptyCell(cell.smell)
//      val occupiedCell = MockCell.create(config.mockInitialSignal)
//
//      newGrid.cells(destination._1)(destination._2) match {
//        case EmptyCell(_) =>
//          newGrid.cells(x)(y) = vacatedCell
//          newGrid.cells(destination._1)(destination._2) = occupiedCell
//        case BufferCell(EmptyCell(_)) =>
//          newGrid.cells(x)(y) = vacatedCell
//          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
//        case _ =>
//          newGrid.cells(x)(y) = occupiedCell
//      }
//    }

//    val (dynamicCells, staticCells) = (for {
//      x <- 0 until config.gridSize
//      y <- 0 until config.gridSize
//    } yield (x, y, grid.cells(x)(y))).partition({
//      case (_, _, MockCell(_)) => true
//      case (_, _, _) => false
//    })

//    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
//    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}