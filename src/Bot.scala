import util.Random

/**
 * Created with IntelliJ IDEA.
 * User: diego
 * Date: 5/2/12
 * Time: 11:13 PM
 * To change this template use File | Settings | File Templates.
 */

class ControlFunctionFactory {
  def create = new Bot().respond _
}

class Bot {
  val rnd = new Random()
  def respond(input: String) = {
    val (opcode, paramMap) = CommandParser(input)
    if (opcode == "React") {
      val generation = paramMap("generation").toInt
      if (generation == 0) {
        if (paramMap("energy").toInt >= 100 && rnd.nextDouble() < 0.25) {
          val heading = XY.random(rnd)
          "Spawn(direction=" + heading + ",energy=100,heading=" + heading +")"
        } else ""
      } else {
        val heading = XY(paramMap("heading"))
        "Move(direction=" + heading + ")"
      }
    } else ""
  }
}

object CommandParser {
  def apply(command: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if( segments.length != 2 )
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0),segments(1))
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)

    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map( splitParam ).toMap
    (segments(0), keyValuePairs)
  }
}

object XY {
  def random(rnd: Random) = (rnd.nextInt(3)-1, rnd.nextInt(3)-1)
  val Zero = XY(0,0)
  val One =  XY(1,1)
  val Right      = XY( 1,  0)
  val RightUp    = XY( 1, -1)
  val Up         = XY( 0, -1)
  val UpLeft     = XY(-1, -1)
  val Left       = XY(-1,  0)
  val LeftDown   = XY(-1,  1)
  val Down       = XY( 0,  1)
  val DownRight  = XY( 1,  1)
  def apply(s: String) : XY = {
    val xy = s.split(':').map(_.toInt) // e.g. "-1:1" => Array(-1,1)
    XY(xy(0), xy(1))
  }
}

case class XY(x: Int, y : Int){
  def isNonZero = x!=0 || y!=0
  def isZero = x==0 && y==0
  def isNonNegative = x>=0 && y>=0

  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)

  def addToX(dx: Int) = XY(x+dx, y)
  def addToY(dy: Int) = XY(x, y+dy)

  def +(pos: XY) = XY(x+pos.x, y+pos.y)
  def -(pos: XY) = XY(x-pos.x, y-pos.y)
  def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)

  def distanceTo(pos: XY) : Double = (this-pos).length
  def length : Double = math.sqrt(x*x + y*y)

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)
}

case class View(cells: String) {
  def apply(index: Int ) = cells.charAt(index)
}
