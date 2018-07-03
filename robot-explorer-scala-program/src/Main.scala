/**
  *   Author:        Clarice Poh
  *   Description:   Main Program Entry Point
  */


import java.util.Scanner
import InputFormatter._
import RobotExplorer.instruct

object Main extends App {

    println(" *** Input the grid size for landing area, follow by a series of position and navigation pertaining to Robot Explorers *** ")

    val scanner = new Scanner(System.in)

    var infoType = "GridInfo"               // Represent different types of info of Input Message

    var gridCoordinate = Seq[Int]()         // Coordinates of landing area
    var coordinate = Seq[Int]()             // Coordinates of robot explorer
    var orientation = ' '                   // Orientation of robot explorer
    var result = ""                         // Final positions for all robot explorers

    while (scanner.hasNextLine) {

        val input = scanner.nextLine()

        if (input.trim.isEmpty) {
            println(" *** Final position for all Robot Explorers *** ")
            println(result)
            result = ""
        } else {

           if (validate(infoType, input)) {

               infoType match {
                   case "GridInfo"  =>
                       gridCoordinate = getGrid(input)
                       infoType = "RobotPositionInfo"

                   case "RobotPositionInfo" =>
                       coordinate = getRobotCoordinate(input)
                       orientation = getRobotOrientation(input)
                       infoType = "RobotInstructionInfo"

                   case "RobotInstructionInfo" =>

                       val instruction = getRobotInstruction(input)
                       val position = instruct(instruction, orientation, coordinate.head, coordinate(1), gridCoordinate.head, gridCoordinate(1), 0, 0)
                       result += position.head + " " + position(1) + " " + position(2).asInstanceOf[Int].toChar + "\n"
                       infoType = "RobotPositionInfo"
               }

           } else {

               infoType match {
                   case "GridInfo" => printInvalidMsg("Grid coordinates must be 2 integer(s), separated by space, e.g: 5 5")
                   case "RobotPositionInfo" => printInvalidMsg("Robot Explorer's position must be 2 integer(s), follow by N/W/S/E orientation, separated by space, e.g. 1 2 N")
                   case "RobotInstructionInfo" => printInvalidMsg("Robot Explorer's instruction must be a string of L, R, or M, without space in between")
               }
           }
        }
    }

    def printInvalidMsg(invalidMsg:String) {
        System.out.println(invalidMsg)
    }
}


