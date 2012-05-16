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
  def respond(input: String) = "Move(direction=1:1)|Say(text=Eh gato)"
}