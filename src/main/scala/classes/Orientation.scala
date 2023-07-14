package classes

class Orientation(direction: Char) {
  var d: Char = direction
  def change(direction: Char) {
    d = direction
    println("Point direction : " + d);
  }


}
