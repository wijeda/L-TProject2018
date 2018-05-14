package example

import d3v4._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ScalaJSExample {

  @JSExportTopLevel("myproject")
  protected def getInstance(): this.type = this

  def groupTicks(d: ChordGroup, step: Double): js.Array[js.Dictionary[Double]] = {
    val k: Double = (d.endAngle - d.startAngle) / d.value
    d3.range(0, d.value, step).map((v: Double) => js.Dictionary("value" -> v, "angle" -> (v * k + d.startAngle)))
  }

  def groupLabels(d: ChordGroup): js.Array[js.Dictionary[Double]] = {
    val k: Double = (d.endAngle - d.startAngle) / d.value
    d3.range((d.value)/2, d.value, (d.value)/2).map((v: Double) => js.Dictionary("value" -> d.value, "angle" -> (v * k + d.startAngle), "id" -> d.index.toDouble))
  }

  @JSExport
  def main(args: Array[String]): Unit = {

    import d3v4.myDSLchordgroup

    val r = scala.util.Random

    val matrix2 = js.Array[js.Array[Double]](
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000))
    )

    val nameMatrix = js.Array[String]("Antoine","Bernard","Cédric","David","Eleonore")
    val test = new myDSLchordgroup(matrix2)
    //test.defcolors(js.Array("#ABC123", "#AACCBB", "#123456", "#CCCFFF","#DEDDED"))
    test.defnames(nameMatrix)
    test("mergable") = "true"
    test.printgraph()
  }

}
