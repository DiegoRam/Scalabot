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
          val direction = (rnd.nextInt(3)-1) + ":" + (rnd.nextInt(3)-1)
          "Spawn(direction=" + direction + ",energy=100,heading=" + direction +")"
        } else ""
      } else {
        val heading =  paramMap("heading")
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
