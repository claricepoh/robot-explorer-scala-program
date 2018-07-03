/**
  *   Author:        Clarice Poh
  *   Description:   Parse and validate the received input message
  */

import scala.util.matching.Regex

object InputFormatter {
    private val gridPattern = "^\\s*(\\d+)\\s*(\\d+)\\s*$".r
    private val robotPositionPattern = "^\\s*(\\d+)\\s+(\\d+)\\s+([NSEWnsew]+)\\s*$".r
    private val robotInstructionPattern = "^\\s*([LRMlrm]+)\\s*$".r

    def getGrid(message: String): Seq[Int] = {
        val gridPattern(maxCoordinateX, maxCoordinateY) = message
        Seq(maxCoordinateX.toInt, maxCoordinateY.toInt)
    }

    def getRobotCoordinate(message: String): Seq[Int] = {
        val robotPositionPattern(coordinateX, coordinateY, _) = message
        Seq(coordinateX.toInt, coordinateY.toInt)
    }

    def getRobotOrientation(message: String): Char = {
        val robotPositionPattern(_, _, orientation) = message
        orientation.toUpperCase.charAt(0)
    }

    def getRobotInstruction(message: String): String = {
        val robotInstructionPattern(instruction) = message
        instruction.toUpperCase
    }

    def validate(infoType: String, message: String): Boolean = {
        infoType match {
          case "GridInfo" => validate(gridPattern, message)
          case "RobotPositionInfo" => validate(robotPositionPattern, message)
          case "RobotInstructionInfo" => validate(robotInstructionPattern, message)
          case _ => false
        }
    }

    private def validate(pattern: Regex, message: String): Boolean = {
        pattern.findFirstMatchIn(message) match {
          case Some(_) => true
          case _  => false
        }
    }
}
