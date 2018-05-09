package d3v4

import example.ScalaJSExample.groupTicks

import scalajs.js
import scalajs.js.{`|`, undefined}
import scala.scalajs.js.annotation._


class myDSLchordgroup(matrix : js.Array[js.Array[Double]], padAngle : Double){
  def printmeth() {
    println(matrix)
  }
  var colors : Option[js.Array[String]] = None

  def defcolors(listofcolors : js.Array[String]): Unit =
  {
    colors = Some(listofcolors)
  }

  val mousewheeled = (d: js.Any) => {


    val zoomLevel = 2
    val mouse = d3.mouse(svg.node())

    val select = d3.select("svg")
      //.attr("transform", "translate(" + (width / 2) + "," + (height / 2) + ") scale(" + zoomLevel + ") translate(" + mouse(0) + ", " + mouse(1) + ")")
      .attr("transform", "translate(" + (width / 2) + "," + (height / 2) + ") scale(" + zoomLevel + ") translate(-" + mouse(0) + ", -" + mouse(1) + ")")
        //.transition()
        //.duration()
  }

  val svg = d3.select("svg")
  val width = svg.attr("width").toDouble
  val height = svg.attr("height").toDouble
  val outerRadius = Math.min(width, height) * 0.5 - 40
  val innerRadius = outerRadius - 30

  val formatValue = d3.formatPrefix(",.0", 1e3)

  val chord = d3.chord().padAngle(padAngle).sortSubgroups(d3.descending)

  val arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius)

  val ribbon = d3.ribbon().radius(innerRadius)

  var color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(js.Array("#000000", "#FFDD89", "#957244", "#F26223"))





  val g: Selection[ChordArray] = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")").datum(chord(matrix))

  g.on("click", mousewheeled)

  def printgraph(): Unit ={

    colors match {
      case Some(c) => color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(c)
        println("lol")
      case None => color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(js.Array("#000000", "#FFDD89", "#957244", "#F26223"))
    }

    val group = g.append("g").attr("class", "groups")
      .selectAll("g")
      .data((c: ChordArray) => c.groups)
      .enter().append("g")

    group.append("path").style("fill", (d: ChordGroup) => color(d.index))
      .style("stroke", (d: ChordGroup) => d3.rgb(color(d.index)).darker())
      .attr("d", (x: ChordGroup) => arc(x))

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



}
