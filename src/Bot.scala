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
  def respond(input: String) = {
    val tokens = input.split('(')
    val opcode = tokens(0)

    if (tokens(0) == "React") {
//      val rest = tokens(1).dropRight(1)
//      val params = rest.split(',')
//      val strPairs = params.map(s => s.split('='))
//      val kvPairs = strPairs.map(a => (a(0),a(1)))
      val paramMap = tokens(1).dropRight(1).split(',').map(_.split('=')).map(a => (a(0),a(1))) toMap

      val energy = paramMap("energy").toInt
      "Stastus(text=Energy:)" + energy + ")"

    }
  }
}