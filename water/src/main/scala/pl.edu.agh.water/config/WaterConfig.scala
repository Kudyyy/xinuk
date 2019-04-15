package pl.edu.agh.water.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class WaterConfig(

                               humanMaxSpeed: Int,
                               fireSpeadingFrequency: Int,
                               signalSpeedRatio: Int,
                               signalSuppressionFactor: Double,
                               signalAttenuationFactor: Double,
                               gridSize: Int,
                               spawnChance: Double,
                               humanSpawnChance: Double,
                               escapeSpawnChance: Double,
                               cannonSpawnChance: Double,
                               humanInitialSignal: Signal,
                               escapeInitialSignal: Signal,
                               cannonInitialSignal: Signal,
                               guiType: GuiType,
                               guiCellSize: Int,
                               workersRoot: Int,
                               iterationsNumber: Long,
                               isSupervisor: Boolean,
                               shardingMod: Int
                             ) extends XinukConfig