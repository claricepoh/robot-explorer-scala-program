/**
  * Author:       Clarice Poh
  * Description:  Model the position & orientation pertaining to Robot Exploration, based on instruction
  *
  */

object RobotExplorer {

  def moveY(orientation: Char, y: Int, maxY: Int, minY: Int): Int = orientation match {
      case 'N' => if(y < maxY) y + 1 else y
      case 'S' => if(y > minY) y - 1 else y
      case 'E' => y
      case 'W' => y
      case  _  => y
  }

  def moveX(orientation: Char, x: Int, maxX: Int, minX: Int): Int = orientation match {
      case 'N' => x
      case 'S' => x
      case 'E' => if(x < maxX) x + 1 else x
      case 'W' => if(x > minX) x - 1 else x
      case  _  => x
  }

  def orientateAnticlockwise(orientation: Char): Char = orientation match {
      case 'N' => 'W'
      case 'W' => 'S'
      case 'S' => 'E'
      case 'E' => 'N'
  }

  def orientateClockwise(orientation: Char): Char = orientation match {
      case 'N' => 'E'
      case 'E' => 'S'
      case 'S' => 'W'
      case 'W' => 'N'
  }

  def instructCommand(cmd: Char,
                      orientation: Char,
                      x: Int,
                      y: Int,
                      maxX: Int,
                      maxY: Int,
                      minX: Int,
                      minY: Int): List[AnyVal] = cmd match {
                           case 'M' => List(moveX(orientation, x, maxX, minX), moveY(orientation, y, maxY, minY), orientation)
                           case 'L' => List(x, y, orientateAnticlockwise(orientation))
                           case 'R' => List(x, y, orientateClockwise(orientation))
                           case _ => List(x, y, orientation)
                      }

  def instruct(commands: String,
               initialOrientation: Char,
               initialX: Int,
               initialY: Int,
               maxX: Int,
               maxY: Int,
               minX: Int,
               minY: Int): List[AnyVal] = {

     val cmdList = commands.split("").map(_.trim.charAt(0)).toList
     val f = cmdList.foldLeft(List[AnyVal](initialX, initialY, initialOrientation.toInt)) _

     f((position, cmd) => instructCommand(
         cmd,
         position(2).asInstanceOf[Int].toChar,
         position(0).asInstanceOf[Int],
         position(1).asInstanceOf[Int],
         maxX,
         maxY,
         minX,
         minY))
  }
}
