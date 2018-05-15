package example

import d3v4._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ScalaJSExample {

  @JSExportTopLevel("myproject")
  protected def getInstance(): this.type = this

  @JSExport
  def main(args: Array[String]): Unit = {

    import d3v4.myDSLchordgroup

    val r = scala.util.Random

    val matrixSize = r.nextInt(2) + 5

    val myMatrix = new js.Array[js.Array[Double]](matrixSize)
    for(j <- 0 to matrixSize){
      myMatrix(j) = new js.Array[Double](matrixSize)
      for(k <- 0 to matrixSize){
        myMatrix(j)(k) = r.nextInt(20000)
      }
    }

    /*val randomMatrixFixedSize = js.Array[js.Array[Double]](
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
    )*/
    //val nameMatrix = js.Array[String]("Antoine","Bernard","Cédric","David","Eleonore")
    val test = new myDSLchordgroup(myMatrix)

    test("mergable") = "true"
    //test.defnames(nameMatrix)
    test.setFontSize(20)
    test.printgraph()
  }



}
