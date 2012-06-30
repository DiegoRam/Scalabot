import com.sun.xml.internal.bind.v2.TODO
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
    opcode match {
      case "Welcome" => welcome(
                            paramMap("name"),
                            paramMap("path"),
                            paramMap("apocalypse").toInt,
                            paramMap("round").toInt
      )
      case "React" =>
        react(
          paramMap("generation").toInt,
          View(paramMap("view")),
          paramMap
        )
      case "Goodbye" =>
        goodbye(
          paramMap("energy").toInt
        )
      case _ =>
        "" // tipo naa
    }

  }
  def welcome(name: String, path: String, apocalypse: Int, round: Int) = "Status(text=Vamo a " + apocalypse + " turnos)"

  def react(generation: Int, view: View, params: Map[String, String]) =
    if( generation == 0 ) reactAsMaster(view, params) else reactAsSlave(view, params)

  def goodbye(energy: Int) = "Status(text=Tengo " + energy + " guita amigo!!)"

  def reactAsMaster(view: View, params: Map[String, String]) = {
    //TODO armar array de entorno
    //TODO ordenar por prioridad el array de entorno
    //TODO si no hay elementos moverse aleatoria mente Pero no hacia elementos Wall
    //TODO si hay elementos ir por la comida mas cercana si no hay enemigos MOVILES cerca
    //SI HAY ENEMIGO CERCA- Si hay energia reproducirse hacia la direccion del enemigo movil
    "Status(text=Diego)"
  }

  def reactAsSlave(view: View, params: Map[String, String]) = "Status(text=Mico)"
  //TODO si el enemigo esta a mas 3 o mmenos de distancia, explotar.
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
  case class View(cells: String) {
    //use Val instead def cause i wanna cache the value
    val size = math.sqrt(cells.length).toInt
    val center = XY(size/2, size/2)

    def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.apply(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells(indexFromRelPos(relPos))

    def offsetToNearest(c: Char) = {
      val relativePositions =
        cells
          .view
          .zipWithIndex
          .filter(_._1 == c)
          .map(p => relPosFromIndex(p._2))
      if(relativePositions.isEmpty)
        None
      else
        Some(relativePositions.minBy(_.length))
    }
  }
}
