package pl.edu.agh.water.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.water.WaterMain
import pl.edu.agh.water.config.WaterConfig
import pl.edu.agh.water.model._
import pl.edu.agh.water.simulation.WaterMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random
import scala.math.min
import pl.edu.agh.xinuk.model.Cell.SmellArrayOps

final class WaterMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: WaterConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, WaterMetrics) = {
    val grid = Grid.empty(bufferZone)
    var waterCount = 0L
    var numberOfOutflows = 0
    var numberOfCannons = 0
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
              if (random.nextDouble() < config.outflowSpawnChance && numberOfOutflows < config.numberOfOutflows) {
                numberOfOutflows += 1
                OutflowAccessible.unapply(EmptyCell.Instance).withOutflow()
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (random.nextDouble() < config.cannonSpawnChance && numberOfCannons < config.numberOfCannons) {
                numberOfCannons += 1
                CannonAccessible.unapply(EmptyCell.Instance).withCannon()
              } else {
                grid.cells(x)(y)
              }
            case 3 =>
              if (random.nextDouble() < config.obstacleSpawnChance) {
                for {
                  i <- x until min(x + config.heightOfObstacle, config.gridSize - 1)
                  j <- y until min(y + config.widthOfObstacle, config.gridSize - 1)
                } grid.cells(i)(j) = NetAccessible.unapply(EmptyCell.Instance).withNet(0)
                NetAccessible.unapply(EmptyCell.Instance).withNet(0)
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }

    if (config.spawnWind) {
      if (WaterMain.windPosition < 0)
        WaterMain.windPosition = random.nextInt(4)
      WaterMain.windPosition match {
        case 0 =>
          grid.cells(1)(0) match {
            case Obstacle =>
              for {
                x <- 1 until config.gridSize - 1
              } grid.cells(x)(1) = WindAccessible.unapply(EmptyCell.Instance).withWind()
            case _ =>
          }
        case 1 =>
          grid.cells(0)(1) match {
            case Obstacle =>
              for {
                y <- 1 until config.gridSize - 1
            } grid.cells(1)(y) = WindAccessible.unapply(EmptyCell.Instance).withWind()
            case _ =>
          }
        case 2 =>
          grid.cells(1)(config.gridSize - 1) match {
            case Obstacle =>
              for {
                x <- 1 until config.gridSize - 1
              } grid.cells(x)(config.gridSize - 2) = WindAccessible.unapply(EmptyCell.Instance).withWind()
            case _ =>
          }
        case 3 =>
          grid.cells(config.gridSize - 1)(1) match {
            case Obstacle =>
              for {
                y <- 1 until config.gridSize - 1
              } grid.cells(config.gridSize - 2)(y) = WindAccessible.unapply(EmptyCell.Instance).withWind()
            case _ =>
          }
      }
    }

    val metrics = WaterMetrics(waterCount)
    (grid, metrics)
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

    def calculatePossibleDestinations(cell: WaterCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
      val idxMap = Grid.SubcellCoordinates.zip(neighbourCellCoordinates).toMap
      def getAngle(x: Double, y: Double): Double = math.atan(math.abs(x)/math.abs(y)) * (180/math.Pi)
      def randomness(x: Int, y: Int): (Int, Int) = {
        if (Random.nextDouble() < config.chanceOfRandomWaterMove) {
          if (x == 1) {
            if (Random.nextDouble() < 0.5) (0, y)
            else (2, y)
          }
          else if (y == 1) {
            if (Random.nextDouble() < 0.5) (x, 0)
            else (x, 2)
          }
          else {
            val x_n = 1 - x
            val y_n = 1 - y
            if (Random.nextDouble() < 0.5) (x, y + y_n)
            else (x + x_n, y)
          }
        }
        else (x, y)
      }

      def collisionChecker(x: Int, y: Int, dst_x: Int, dst_y: Int): (Int, Int) = {
         grid.cells(dst_x)(dst_y) match {
           case _: WaterCell => randNearCoordinates(x, y)
           case _: NetCell => randNearCoordinates(x, y)
           case _ => (x, y)
        }
      }

      def randNearCoordinates(x: Int, y: Int): (Int, Int) = {
        if (x == 1) {
          if (Random.nextDouble() < 0.5) (0, x)
          else (2, x)
        }
        else if (y == 1) {
          if (Random.nextDouble() < 0.5) (y, 0)
          else (y, 2)
        }
        else {
          val x_n = 1 - x
          if (Random.nextDouble() < 0.5) (x, x + 2 * x_n)
          else (x + 2 * x_n, y)
        }
      }

      val vec = Grid.SubcellCoordinates
        .foldLeft((Double.MinPositiveValue, Double.MinPositiveValue)) ((acc, cord) => {
          (acc._1 + ((cord._1 - 1) * cell.smell(cord._1)(cord._2).value),
            acc._2 + ((cord._2 - 1) * cell.smell(cord._1)(cord._2).value))
        })

      val angle = getAngle(vec._1, vec._2)
      val v1 = if (angle > 30.0) 1 else 0
      val v2 = if (angle < 60.0) 1 else 0
      val v11 = if (vec._1 < 0.0) 1 - v1 else 1 + v1
      val v22 = if (vec._2 < 0.0) 1 - v2 else 1 + v2
      val (i, j) = idxMap(v11, v22)
      val (res_i, res_j) = collisionChecker(v11, v22, i, j)
      val (dst_i, dst_j) = idxMap(randomness(res_i, res_j))
      Iterator((dst_i, dst_j, grid.cells(dst_i)(dst_j)))
    }

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
             newGrid.cells(x)(y) = cell
          }
        case OutflowCell(_) =>
          newGrid.cells(x)(y) = OutflowAccessible.unapply(EmptyCell.Instance).withOutflow()
        case CannonCell(_) =>
          if (Random.nextDouble() < config.waterSpreadOutFromCannonFrequency) {
            for {
              i <- -1 to 1
              j <- -1 to 1
            } if (grid.cells(x + i)(y + j).isInstanceOf[EmptyCell])
                newGrid.cells(x + i)(y + j) = WaterAccessible.unapply(EmptyCell.Instance).withWater(random.nextInt(config.waterMaxSpeed) + 1)
          }
          newGrid.cells(x)(y) = CannonAccessible.unapply(EmptyCell.Instance).withCannon()
        case cell: WaterCell =>
          newGrid.cells(x)(y) match {
            case _ => if (iteration % cell.speed == 0 &&
                          math.abs(cell.smell.map(_.map(_.value).sum).sum/(cell.smell.length * cell.smell.length)) > 0.000000000000001) {
              moveWater(cell, x, y)
            } else {
              stayInPlace(cell, x, y)
            }
          }
        case WindCell(_) =>
          newGrid.cells(x)(y) = WindAccessible.unapply(EmptyCell.Instance).withWind()
        case cell@NetCell(_, _) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell.withSmell(cell.smell * config.obstacleSuppressionFactor)
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
        case Opt((i, j, WaterAccessible(dst))) => {
          newGrid.cells(i)(j) = dst.withWater(cell.speed)
          newGrid.cells(x)(y) = EmptyCell(cell.smell)
        }
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