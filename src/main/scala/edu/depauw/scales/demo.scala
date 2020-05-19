package edu.depauw.scales

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel
import fastparse._
import scalaparse.Scala

object demo {
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
  }

  def appendPre(targetNode: dom.Node, text: String): Unit = {
    val preNode = document.createElement("pre")
    preNode.textContent = text
    targetNode.appendChild(preNode)
  }

  def main(args: Array[String]): Unit = {
    appendPar(document.body, "Hello world!")
  }

  @JSExportTopLevel("runParser")
  def runParser(): Unit = {
    val src = document.getElementById("src").asInstanceOf[dom.html.TextArea].value

    appendPre(document.body, src)
    
    parse(src, Scala.CompilationUnit(_)) match {
      case f: Parsed.Failure =>
        appendPar(document.body, s"Failed: $f")
      case s: Parsed.Success[_] =>
        appendPar(document.body, s"Succeeded at ${s.index} / ${src.length}")
    }
  }
}