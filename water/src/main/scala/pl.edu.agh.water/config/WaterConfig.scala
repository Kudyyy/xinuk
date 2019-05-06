package pl.edu.agh.water.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class WaterConfig(
                               waterMaxSpeed: Int,
                               signalSpeedRatio: Int,
                               signalSuppressionFactor: Double,
                               signalAttenuationFactor: Double,
                               gridSize: Int,
                               spawnChance: Double,
                               waterSpawnChance: Double,
                               outflowSpawnChance: Double,
                               numberOfOutflows: Int,
                               cannonSpawnChance: Double,
                               numberOfCannons: Int,
                               waterSpreadOutFromCannonFrequency: Double,
                               obstacleSpawnChance: Double,
                               widthOfObstacle: Int,
                               obstacleSuppressionFactor: Double,
                               heightOfObstacle: Int,
                               waterInitialSignal: Signal,
                               outflowInitialSignal: Signal,
                               cannonInitialSignal: Signal,
                               guiType: GuiType,
                               guiCellSize: Int,
                               workersRoot: Int,
                               iterationsNumber: Long,
                               isSupervisor: Boolean,
                               shardingMod: Int
                             ) extends XinukConfig