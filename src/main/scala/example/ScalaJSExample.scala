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

  @JSExport
  def main(args: Array[String]): Unit = {

    import d3v4.myDSLchordgroup

    val r = scala.util.Random

    val matrixSize = r.nextInt(15) + 5

    val myMatrix = new js.Array[js.Array[Double]](matrixSize)
    for(j <- 0 to matrixSize){
      myMatrix(j) = new js.Array[Double](matrixSize)
      for(k <- 0 to matrixSize){
        myMatrix(j)(k) = r.nextInt(20000)
      }
    }

    val randomMatrixFixedSize = js.Array[js.Array[Double]](
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)),
      js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000))
    )

    val testMatrix = js.Array[js.Array[Double]](
      js.Array(11975, 5871, 8916, 2868),
      js.Array(1951, 10048, 2060, 6171),
      js.Array(8010, 16145, 8090, 8045),
      js.Array(1013, 990, 940, 6907)
    )

    val test = new myDSLchordgroup(myMatrix)
    test.defcolors(js.Array("#ABC123", "#AACCBB", "#123456", "#654321", "#FACDEB"))
    test("mergable") = "true"
    test.printgraph()
  }

}
