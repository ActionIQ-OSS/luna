package co.actioniq.luna.example

trait NameTable {
  def name: slick.lifted.Rep[String]
}
