package example

import d3v4._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ScalaJSExample {

  @JSExportTopLevel("myproject")
  protected def getInstance(): this.type = this

  @JSExport
  def main(args: Array[String]): Unit = {

    import d3v4.chordDiagram

    val r = scala.util.Random

    val matrixSize = r.nextInt(2) + 5

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

    val nameList = js.Array[String]("Antoine","Bernard","Cédric","David","Eleonore")
    //val colorList = js.Array[String]("#ABC123", "#AACCBB", "#123456", "#654321", "#FACDEB")
    val interactions = js.Array[js.Array[Double]](
      js.Array(0, 5871, 8916, 2868, 250),
      js.Array(1951, 10048, 2060, 6171, 8000),
      js.Array(8010, 16145, 0, 8090, 8045),
      js.Array(1013, 990, 940, 0, 6907),
      js.Array(250, 7894, 32, 10, 0)
    )

    val myDiagram = new chordDiagram(interactions)
    myDiagram.set(Map("names"->nameList,"mergeable"->true,"hoverable"->true,"displayLabels"->"both"))
//    myDiagram.setDisplayLables("labels")
//    myDiagram("mergable") = true
    //myDiagram("hoverable") = true
    //myDiagram.set(Map("padding"->0.05,"mergeable"->true,"font-size"->20,"displayLabels"->"labels","colors"->colorList,"names"->nameList,"background"->"black"))
    myDiagram.printgraph()

    
    //test.defcolors(colorList)
    //test("mergable") = "true"
    //test.set(Map("padding"->0.05,"mergable"->true,"font-size"->20,"displayLabels"->"labels","colors"->colorList,"names"->nameList))
    //test.defnames(nameList)
    //test.setFontSize(20)

    //test.addGroup(js.Array(r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000),r.nextInt(20000)))

  }

}
