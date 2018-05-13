package d3v4

import example.ScalaJSExample.groupTicks
import org.scalajs.dom.raw.WheelEvent

import scalajs.js
import scalajs.js.{`|`, undefined}
import scala.scalajs.js.annotation._
import scala.io.Source

class myDSLchordgroup(matrix : js.Array[js.Array[Double]]){

  val numberRow = matrix.length

  for(a <- 0 to numberRow-1){
    if(matrix(a).length != numberRow){
      println("matrix not square")
      throw new IllegalArgumentException
    }
  }

  // Random generator
  val random = new scala.util.Random
  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  // Generate a random alphabnumeric string of length n
  def randomAlphanumericString(n: Int) =
    randomString("ABCDEF0123456789")(n)

  val svg = d3.select("svg")
  var width = svg.attr("width").toDouble
  var height = svg.attr("height").toDouble
  var outerRadius = Math.min(width, height) * 0.5 - 40
  var innerRadius = outerRadius - 30
  var colors : Option[js.Array[String]] = None
  var ZoomLevel = 1.0
  var padAngle = 0.05
  var mergable = "false"

  val myRandomColors = js.Array[String]("matrix.length-1")
  for(n <- 0 until matrix.length){
    myRandomColors(n) = "#" + randomAlphanumericString(6)
  }

  def update(toChange : String, value : Any): Unit = toChange match{
    case "padding" => padAngle = value.asInstanceOf[Double]
    case "mergable" => mergable = value.asInstanceOf[String]
    case "outerRadius" => outerRadius = value.asInstanceOf[Double]
    case "innerRadius" => innerRadius = value.asInstanceOf[Double]
    case "width" => width = value.asInstanceOf[Double]
    case "height" => height = value.asInstanceOf[Double]

  }
  def defcolors(listofcolors : js.Array[String]): Unit =
  {
    colors = Some(listofcolors)
  }


  def printgraph(): Unit ={


    /*
    .append("svg")
    .attr("width", "100%")
    .attr("height", "100%")
    .call(d3.zoom().on("zoom", () => {
      svg.attr("transform", d3.event.transform)
    }))
    .append("g")
    */

    //svg.call(d3.zoom().on("zoom", () => d3.select("svg").attr("transform", d3.event.transform.toString)))



    val formatValue = d3.formatPrefix(",.0", 1e3)
    val chord = d3.chord().padAngle(padAngle).sortSubgroups(d3.descending)
    val arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius)
    val ribbon = d3.ribbon().radius(innerRadius)
    var color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(myRandomColors)
    val g: Selection[ChordArray] = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")").datum(chord(matrix))

    val zoom = (d: js.Any) => {
      d3.event.preventDefault()
      //zoom
      if(d3.event.asInstanceOf[WheelEvent].deltaY < 0){
        ZoomLevel += 0.1
      }
      //dezoom
      else{
        ZoomLevel -= 0.1
      }

      val mouse = d3.mouse(svg.node())

      val select = d3.select("svg")
        .attr("transform", "translate(" + 0 + ", " + 0 + ") scale(" + ZoomLevel + ") translate(-" + 0 + ", " + 0 + ")")
      //.attr("transform", "translate(" + (width / 2) + "," + (height / 2) + ") scale(" + zoomLevel + ") translate(-" + mouse(0) + ", -" + mouse(1) + ")")
      //.transition()
      //.duration()
    }
    svg.on("wheel", zoom)

    colors match {
      case Some(c) => color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(c)
        println("colors defined")
        println(c)
      case None => color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(myRandomColors)
        println("colors undefined")
        colors = Some(myRandomColors)
    }

    val group = g.append("g").attr("class", "groups")
      .selectAll("g")
      .data((c: ChordArray) => c.groups)
      .enter().append("g")
      //.on("click", giveinfo(_))

    group.append("path").style("fill", (d: ChordGroup) => color(d.index))
      .style("stroke", (d: ChordGroup) => d3.rgb(color(d.index)).darker())
      .attr("d", (x: ChordGroup) => arc(x))
      .attr("id",(d:ChordGroup) => "chordgroup" + d.index)
      .attr("color", (d: ChordGroup) => d3.rgb(color(d.index)))
      .attr("selected", "false")
      .on("click", (d:ChordGroup) => {
        val sel = d3selection.select("#chordgroup"+ d.index).attr("selected")
        if(sel == "false"){
          if(mergable == "true"){
            for(index2 <- 0 until matrix.length){
              val currentSel = d3selection.select("#chordgroup"+ index2).attr("selected")
              if(currentSel == "true"){
                merge(index2, d)
              }
            }
          }
          val mygroup = d3selection.select("#chordgroup"+ d.index)
            .attr("style", "fill : rgb(0,0,0)")
            .attr("selected", "true")
        }
        else if(sel == "true"){
          val color = d3selection.select("#chordgroup"+ d.index).attr("color")
          val mygroup = d3selection.select("#chordgroup"+ d.index)
            .attr("style", "fill : "+color)
            .attr("selected", "false")
        }
        //val color = d3selection.select("#chordgroup"+ d.index).attr("color")
      })

    var groupTick = group.selectAll(".group-tick").data((d: ChordGroup) => groupTicks(d, 1e3))
      .enter().append("g").attr("class", "group-tick")
      .attr("transform", (d: js.Dictionary[Double]) =>  "rotate(" + (d("angle") * 180 / Math.PI - 90) + ") translate(" + outerRadius + ",0)")


    groupTick.append("line").attr("x2", 6)

    groupTick.filter((d: js.Dictionary[Double]) => d("value") % 5e3 == 0).append("text")
      .attr("x", 8)
      .attr("dy", ".35em")
      .attr("transform", (d: js.Dictionary[Double]) => if(d("angle") > Math.PI) "rotate(180) translate(-16)" else null)
      .style("text-anchor", (d: js.Dictionary[Double]) => if(d("angle") > Math.PI) "end" else null)
      .text((d: js.Dictionary[Double]) => formatValue(d("value")))

    g.append("g").attr("class", "ribbons").selectAll("path").data((c: ChordArray) => c)
      .enter().append("path")
      .attr("d", (d: Chord) => ribbon(d))
      .style("fill", (d: Chord) => color(d.target.index))
      .style("stroke", (d: Chord) => d3.rgb(color(d.target.index)).darker())

  }

  //when called, merge the d-th chordgroup of the chord diagram with the index2-th
  def merge(index2 : Int, d : ChordGroup) : Unit ={

    val newMatrix = new js.Array[js.Array[Double]](matrix.length-1)
    for(j <- 0 to newMatrix.length -1){
      newMatrix(j) = new js.Array[Double](matrix.length-1)
    }

    //calculates the new line merging the ones from the merged entries
    val newLine = new Array[Double](matrix.length)
    for(i <- 0 to matrix.length-1){
      newLine(i) = matrix(index2)(i) + matrix(d.index)(i)
    }
    val newColumn = new Array[Double](matrix.length)
    var newIndex = 0
    var deletedIndex = 0
    //we take the smallest index between the two merged, it will be the
    if(d.index<index2){
      newIndex = d.index
      deletedIndex = index2
    }
    else{
      newIndex = index2
      deletedIndex = d.index
    }

    //calculates the new column merging the ones from the merged entries
    for(i <- 0 to matrix.length-1){
      if(i == newIndex)
      {
        newColumn(i) = newLine(index2) + newLine(d.index)
      }
      else if(i != deletedIndex)
      {
        newColumn(i) = matrix(i)(index2) + matrix(i)(d.index)
      }
    }
    //fill the new matrix
    for(abs <- 0 to matrix.length-2){
      for(ord <- 0 to matrix.length-2){
        if(abs == newIndex){
          if(ord >= deletedIndex){
            newMatrix(abs)(ord)= newLine(ord+1)
          }
          else if(ord == newIndex) {
            newMatrix(abs)(ord) = newColumn(ord)
          }
          else{
            newMatrix(abs)(ord)= newLine(ord)
          }

        }
        else if(ord == newIndex){
          if(abs >= deletedIndex){
            newMatrix(abs)(ord) = newColumn(abs+1)
          }
          else{
            newMatrix(abs)(ord) = newColumn(abs)
          }
        }
        else{
          if(abs >= deletedIndex){
            newMatrix(abs)(ord) = matrix(abs+1)(ord+1)
          }
          else{
            newMatrix(abs)(ord) = matrix(abs)(ord)
          }
        }
      }
    }
    d3selection.select("g").remove()
    val newplot = new myDSLchordgroup(newMatrix)
    colors match {
      case Some(c) =>
        val newColors = js.Array[String]("newMatrix.length-1")
        for(i <- 0 until newMatrix.length){
          if(i < deletedIndex){
            newColors(i) = c(i)
          }
          else{
            newColors(i) = c(i+1)
          }
        }
        println("LOOOOOL")
        println(c)
        println(newColors)
        newplot.defcolors(newColors)
      case None => newplot.defcolors(myRandomColors)
    }

    newplot("padding") = padAngle
    newplot("mergable") = mergable

    newplot.printgraph()

  }


}
