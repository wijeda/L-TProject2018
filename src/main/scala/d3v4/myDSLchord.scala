package d3v4

import org.scalajs.dom.raw.WheelEvent

import scalajs.js
import scalajs.js.{`|`, undefined}
import scala.scalajs.js.annotation._
import scala.io.Source
import scala.util.matching.Regex.Match

class myDSLchordgroup(matrix : js.Array[js.Array[Double]]) {

  for (a <- 0 until matrix.length) {
    if (matrix(a).length != matrix.length) {
      println("matrix not square")
      throw new IllegalArgumentException
    }
  }
  //inspired by http://www.bindschaedler.com/2012/04/07/elegant-random-string-generation-in-scala/
  //***********************************************************
  //  GLOBAL VARIABLE DEFINITION
  //***********************************************************
  // Total sum of the flows
  val total = matrix.flatten.sum
  // Random generator
  val random = new scala.util.Random
  // SVG block that will contain the chord graph
  val svg = d3.select("svg")
  // Size of the block that will contain the chord graph
  var width = svg.attr("width").toDouble
  var height = svg.attr("height").toDouble
  // Define the width of the circle
  var outerRadius = Math.min(width, height) * 0.5 - 40
  var innerRadius = outerRadius - 30
  // The color set of this graph
  var colors: Option[js.Array[String]] = None
  // The list of names for the graph elements
  var names: Option[js.Array[String]] = None
  // If the names are not defined, we will show the ticks
  var nameIsDefined: Boolean = false
  // The size of the chord graph within the webpage
  var ZoomLevel = 1.0
  // The curvature of the ribbons
  var padAngle = 0.05
  // Are the groups mergeable?
  var mergable = "false"
  // The size of the names/ticks
  var fontSize = 10
  // Display only ticks, labels, both or nothing
  var displayLabels: String = "both"
  // The block of info shown when hovering the chord graph
  var infobox = d3selection.select("#infobox")
  // Generate a JS.ARRAY of random colors
  val myRandomColors = js.Array[String]("matrix.length-1")
  for(n <- 0 until matrix.length){
    myRandomColors(n) = "#" + randomAlphanumericString(6)
  }

  //***********************************************************
  //  GLOBAL METHODS DEFINITION
  //***********************************************************
  // Set all parameter contained in the param dictionary
  def set(param: Map[String, Any])={
    for(p <- param){
      p match {
        case ("padding",value) => padAngle = value.asInstanceOf[Double]
        case ("mergable",value) => mergable = value.asInstanceOf[String]
        case ("outerRadius",value) => outerRadius = value.asInstanceOf[Double]
        case ("innerRadius",value) => innerRadius = value.asInstanceOf[Double]
        case ("width",value) => width = value.asInstanceOf[Double]
        case ("height",value) => height = value.asInstanceOf[Double]
        case ("font-size",value) => fontSize = value.asInstanceOf[Int]
        case ("displayLabels",value) => displayLabels = value.asInstanceOf[String]
        case ("colors",value) => defcolors(value.asInstanceOf[js.Array[String]])
        case ("names",value) => defnames(value.asInstanceOf[js.Array[String]])
      }
    }
  }
  def setDisplayLables(choice: String)={
    choice match{
      case "ticks" => displayLabels = choice
      case "labels" => displayLabels = choice
      case "both" => displayLabels = choice
      case _ => displayLabels = "nothing"
    }
  }
  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  // Generate a random alphabnumeric string of length n
  def randomAlphanumericString(n: Int) =
    randomString("ABCDEF0123456789")(n)
  // Return a JS.ARRAY which contained the value and rotation angle for each elements/groups
  def groupTicks(d: ChordGroup, step: Double): js.Array[js.Dictionary[Double]] = {
    val k: Double = (d.endAngle - d.startAngle) / d.value
    d3.range(0, d.value, step).map((v: Double) => js.Dictionary("value" -> v, "angle" -> (v * k + d.startAngle)))
  }
  // Return a JS.ARRAY which contained the name/Label, rotation angle and ID of each elements/groups
  def groupLabels(d: ChordGroup): js.Array[js.Dictionary[Double]] = {
    val k: Double = (d.endAngle - d.startAngle) / d.value
    d3.range((d.value)/2, d.value, (d.value)/2).map((v: Double) => js.Dictionary("value" -> d.value, "angle" -> (v * k + d.startAngle), "id" -> d.index.toDouble))
  }
  // Set or get the font-size of the name/labes/ticks
  def setFontSize(ps: Int)={
    fontSize = ps
  }
  def getFontSize():String={
    fontSize.toString
  }

  // Automatically called when a myDSLchord instance is applyied whith one of this cases
  def update(toChange : String, value : Any): Unit = toChange match{
    case "padding" => padAngle = value.asInstanceOf[Double]
    case "mergable" => mergable = value.asInstanceOf[String]
    case "outerRadius" => outerRadius = value.asInstanceOf[Double]
    case "innerRadius" => innerRadius = value.asInstanceOf[Double]
    case "width" => width = value.asInstanceOf[Double]
    case "height" => height = value.asInstanceOf[Double]
    case "font-size" => fontSize = value.asInstanceOf[Int]
    case "displayLabels" => displayLabels = value.asInstanceOf[String]
  }

  // Defines the color list
  def defcolors(listofcolors: js.Array[String]): Unit = {
    colors = Some(listofcolors)
  }
  // Defines the names/labels list
  def defnames(listofnames : js.Array[String]): Unit ={
    while(listofnames.length != 0 && listofnames.length < matrix.length){
      listofnames.push("")
    }
    names = Some(listofnames)
  }

  /**
    * Main function
    * Build the chord graph respecting the index.html structure
    * with the given matrix, colors list and names list
    */
  def printgraph(): Unit = {

    //***********************************************************
    //  VARIABLE DEFINITION
    //***********************************************************
    // Formats the doubles to the given specifier
    val formatValue = d3.formatPrefix(",.0", 1e3)
    val formatValuePercent = d3.formatPrefix(".0%", 1)
    // The chords of the graph
    val chord = d3.chord().padAngle(padAngle).sortSubgroups(d3.descending)
    // The arc represent the groups
    val arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius)
    // The ribbons represent the flows between groups
    val ribbon = d3.ribbon().radius(innerRadius)
    var color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(myRandomColors)
    var name = js.Array[String]()
    // Blocs inside SVG
    val g: Selection[ChordArray] = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")").datum(chord(matrix)).attr("id", "datgroup")

    // This call allow the user to zoom in and out on the chord graph
    svg.call(d3.zoom().on("zoom", () => g.attr("transform", d3.event.transform.toString)))

    // Define the appearing info bloc style
    infobox.style("display", "block")
      .style("visibility", "hidden")
      .style("position", "absolute")
      .style("font-size", getFontSize())
      .style("z-index", "10")
      .style("background", "#333")
      .style("padding", "5px")
      .style("border", "1px solid lightgrey")
      .style("color", "white")

    // If a color list has been given by the user, we will use these colors
    // Else will display a random set.
    // Also, if the given set is smaller than the number of groups, random colors will be added
    colors match {
      case Some(c) => color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(c)
        if(c.length < matrix.length){
          var newColors = js.Array[String]("matrix.length-1")
          for(indexColor <- 0 until matrix.length){
            if(indexColor < c.length){
              newColors(indexColor) = c(indexColor)
            }
            else{
              newColors(indexColor) = "#" + randomAlphanumericString(6)
            }
          }
          colors = Some(newColors)
          color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(newColors)
        }
        else{
          color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(c)
        }
      case None => color = d3.scaleOrdinal[Int, String]().domain(d3.range(4)).range(myRandomColors)
        colors = Some(myRandomColors)
    }
    // If a name list has been given by the user, "nameIsDefined" is set to true and we will use these names
    // Else "nameIsDefined" is set to false and ticks will be displayed instead
    names match {
      case Some(n) => name = names.get
        nameIsDefined = true
      case None => name = js.Array[String]()
        nameIsDefined = false
    }
    /*val zoom = (d: js.Any) => {
      d3.event.preventDefault()
      //zoom
      if(d3.event.asInstanceOf[WheelEvent].deltaY < 0 && ZoomLevel < 5.0){
        ZoomLevel += 0.1
      }
      //dezoom
      else if(ZoomLevel > 0.1){
        ZoomLevel -= 0.1
      }

      val mouse = d3.mouse(svg.node())

      val select = d3.select("svg")
        .attr("transform", "translate(" + 0 + ", " + 0 + ") scale(" + ZoomLevel + ") translate(-" + 0 + ", " + 0 + ")")
      //.attr("transform", "translate(" + (width / 2) + "," + (height / 2) + ") scale(" + zoomLevel + ") translate(-" + mouse(0) + ", -" + mouse(1) + ")")
      //.transition()
      //.duration()
    }
    svg.on("wheel", zoom)*/

    //***********************************************************
    //  METHODS DEFINITION
    //***********************************************************
    // Used when mouse hover over elements/groups to display info
    def groupTip(d: ChordGroup): String = {
      nameIsDefined match{
        case true =>
          return ("Group Info:<br/>"
            + "Name: "+name(d.index)+", Total value: " + d.value+ "<br/>"
            + formatValuePercent((d.value/total)*100) + "% over "+total)
        case false =>
          return ("Group Info:<br/>"
            + "Total value: " + d.value + "<br/>"
            + formatValuePercent((d.value/total)*100) + "% over "+total)
      }
    }
    // Used when mouse hover over ribbons to display info
    def chordTip(d: Chord): String = {
      val s = d.source.index
      val t = d.target.index
      nameIsDefined match{
        case true =>
          return ("Chord Info:<br/>"
            + "Send: " + matrix(s)(t) + " From: " + name(s) + "<br/>"
            + "Receive: " + matrix(t)(s) + " Target: " + name(t))
        case false =>
          return ("Chord Info:<br/>"
            + "Send: " + matrix(s)(t) + "<br/>"
            + "Receive: " + matrix(t)(s))
      }
    }

    //when called, merge the d-th chordgroup of the chord diagram with the index2-th
    def merge(index2: Int, indexD: Int, fromOutside: Int = 0): Unit = {

      val newMatrix = new js.Array[js.Array[Double]](matrix.length-1)
      for(j <- 0 to newMatrix.length -1){
        newMatrix(j) = new js.Array[Double](matrix.length-1)
      }

      //calculates the new line merging the ones from the merged entries
      val newLine = new Array[Double](matrix.length)
      for(i <- 0 to matrix.length-1){
        newLine(i) = matrix(index2)(i) + matrix(indexD)(i)
      }
      val newColumn = new Array[Double](matrix.length)
      var newIndex = 0
      var deletedIndex = 0
      //we take the smallest index between the two merged, it will be the
      if(indexD<index2){
        newIndex = indexD
        deletedIndex = index2
      }
      else{
        newIndex = index2
        deletedIndex = indexD
      }

      //calculates the new column merging the ones from the merged entries
      for(i <- 0 to matrix.length-1){
        if(i == newIndex)
        {
          newColumn(i) = newLine(index2) + newLine(indexD)
        }
        else if(i != deletedIndex)
        {
          newColumn(i) = matrix(i)(index2) + matrix(i)(indexD)
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
      if(fromOutside == 0){
        d3selection.select("svg").html("")
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
          newplot.defcolors(newColors)
        case None => newplot.defcolors(myRandomColors)
      }
      names match {
        case Some(n) =>
          val newNames = js.Array[String]("newMatrix.length-1")
          for(i <- 0 until newMatrix.length){
            if(i < deletedIndex){
              newNames(i) = n(i)
            }
            else{
              newNames(i) = n(i+1)
            }
          }
          newNames(newIndex) = n(newIndex) + ", " + n(deletedIndex)
          println("names defined")
          println(n)
          newplot.defnames(newNames)
        case None =>
          println("names undefined")
      }
      newplot("padding") = padAngle
      newplot("mergable") = mergable
      newplot("font-size") = fontSize
      newplot("displayLabels") = displayLabels
      newplot.printgraph()

    }

    //***********************************************************
    //  CHORDGRAPH BLOCS DEFINITION
    //***********************************************************
    val group = g.append("g").attr("class", "groups")
      .selectAll("g")
      .data((c: ChordArray) => c.groups)
      .enter().append("g")
    //.on("click", giveinfo(_))

    group.append("path").style("fill", (d: ChordGroup) => color(d.index))
      .style("stroke", (d: ChordGroup) => d3.rgb(color(d.index)).darker())
      .attr("d", (x: ChordGroup) => arc(x))
      .attr("id", (d: ChordGroup) => "chordgroup" + d.index)
      .attr("color", (d: ChordGroup) => d3.rgb(color(d.index)))
      .attr("selected", "false")
      // When two groups are selected: merged them. A clic on a group is a selection
      .on("click", (d:ChordGroup) => {
        val sel = d3selection.select("#chordgroup"+ d.index).attr("selected")
        if(sel == "false"){
          if(mergable == "true"){
            for(index2 <- 0 until matrix.length){
              val currentSel = d3selection.select("#chordgroup"+ index2).attr("selected")
              if(currentSel == "true"){
                merge(index2, d.index, 1)
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
      // When hovering over a group, hide the others and display the group info
      .on("mouseover", (d: ChordGroup) => {
        for(i <- 0 until matrix.length){
          d3selection.select("#chordgroup"+i)
            .style("opacity", "0.2")
          for(j <- 0 until matrix.length) {
            d3selection.select("#ribbonID" + j + i)
              .style("opacity", "0.2")
            d3selection.select("#ribbonID" + i + j)
              .style("opacity", "0.2")
          }
        }
        d3selection.select("#chordgroup"+d.index)
          .style("opacity", "1.0")
        for(i <- 0 until matrix.length){
          d3selection.select("#ribbonID"+d.index+i)
            .style("opacity", "1.0")
          var f = d3selection.select("#ribbonID"+i+d.index)
            .style("opacity", "1.0")
        }
        infobox.style("visibility", "visible")
          .style("top", (d3.event.y+10)+"px")
          .style("left", (d3.event.x+10)+"px")
          .html(groupTip(d))
        if(false){
          return
        }
      })
      // When hovering out of a group, hide the group info and display other groups normally
      .on("mouseout", (d: ChordGroup) => {
        /*d3selection.select("#ribbonID"+d.source.index+d.target.index)
          .style("opacity", "1.0")*/
        for(i <- 0 until matrix.length){
          d3selection.select("#chordgroup"+i)
            .style("opacity", "1.0")
          for(j <- 0 until matrix.length) {
            d3selection.select("#ribbonID" + j + i)
              .style("opacity", "1.0")
            d3selection.select("#ribbonID" + i + j)
              .style("opacity", "1.0")
          }
        }
        infobox.style("visibility", "hidden")
        /*d3selection.select("#groupLabel"+d.index)
          .style("visibility", "hidden")*/
        if(false){
          return
        }
      })

    // If the names have been defined, diplay them. Else display ticks
    if(displayLabels == "ticks" || displayLabels == "both") {
      var groupTick = group.selectAll(".group-tick").data((d: ChordGroup) => groupTicks(d, 1e3))
        .enter().append("g").attr("class", "group-tick")
        .attr("transform", (d: js.Dictionary[Double]) => "rotate(" + (d("angle") * 180 / Math.PI - 90) + ") translate(" + outerRadius + ",0)")


      groupTick.append("line").attr("x2", 6)

      groupTick.filter((d: js.Dictionary[Double]) => d("value") % 5e3 == 0).append("text")
        .attr("x", 8)
        .attr("dy", ".35em")
        .attr("transform", (d: js.Dictionary[Double]) => if (d("angle") > Math.PI) "rotate(180) translate(-16)" else null)
        .style("text-anchor", (d: js.Dictionary[Double]) => if (d("angle") > Math.PI) "end" else null)
        .text((d: js.Dictionary[Double]) => formatValue(d("value")))
    }
    if(displayLabels == "labels" || displayLabels== "both") {
      var groupLabel = group.selectAll(".group-label").data((d: ChordGroup) => groupLabels(d))
        .enter().append("g").attr("class", "group-label")
        .attr("transform", (d: js.Dictionary[Double]) => "rotate(" + (d("angle") * 180 / Math.PI - 90) + ") translate(" + (outerRadius ) + ",0)")
        .style("font-size", getFontSize())


      groupLabel.append("div").attr("x2", 6)

      groupLabel.filter((d: js.Dictionary[Double]) => d("value") % 1 == 0).append("text")
        .attr("x", 8)
        .attr("dy", ".8em")
        .attr("id", (d: js.Dictionary[Double]) => "groupLabel" + (d("id")))
        .attr("transform", (d: js.Dictionary[Double]) => if (d("angle") > Math.PI) "rotate(180) translate(-16)" else null)
        .style("text-anchor", (d: js.Dictionary[Double]) => if (d("angle") > Math.PI) "end" else null)
        .text((d: js.Dictionary[Double]) => {
          var n = ""
          if (!name.isEmpty) {
            n = (name(d("id").toInt)+ " ")
          }
          n + "ID:" + (d("id")) + ", Total:" + formatValue(d("value"))
        })
    }
    g.append("g").attr("class", "ribbons").selectAll("path").data((c: ChordArray) => c)
      .enter().append("path")
      .attr("d", (d: Chord) => ribbon(d))
      .attr("id",(d:Chord) => "ribbonID" + d.source.index+d.target.index)
      .style("fill", (d: Chord) => color(d.target.index))
      .style("stroke", (d: Chord) => d3.rgb(color(d.target.index)).darker())
      // When hovering over a ribbon, hide the others and display the ribbon info
      .on("mouseover", (d: Chord) => {
        for(i <- 0 until matrix.length; j <- 0 until matrix.length){
          d3selection.select("#ribbonID"+j+i)
            .style("opacity", "0.2")
          d3selection.select("#ribbonID"+i+j)
            .style("opacity", "0.2")
        }
        d3selection.select("#ribbonID"+d.source.index+d.target.index)
          .style("opacity", "1.0")
        infobox.style("visibility", "visible")
          .style("top", (d3.event.y+10)+"px")
          .style("left", (d3.event.x+10)+"px")
          .html(chordTip(d))
        if(false){
          return
        }
      })
      // When hovering out of a ribbon, hide the ribbon info and display other ribbons normally
      .on("mouseout", (d: Chord) => {
        for(i <- 0 until matrix.length; j <- 0 until matrix.length){
          d3selection.select("#ribbonID"+j+i)
            .style("opacity", "1.0")
          d3selection.select("#ribbonID"+i+j)
            .style("opacity", "1.0")
        }
        infobox.style("visibility", "hidden")
        if(false){
          return
        }
      })
  }
}