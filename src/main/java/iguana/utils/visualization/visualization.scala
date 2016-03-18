
trait Shape {
  import Style._
  def style: Style
}



object Style extends Enumeration {
  type Style = Value
  val boxed = Value
}
