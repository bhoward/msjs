package edu.depauw.scales

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

import scala.meta._

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

    val tree = src.parse[Stat].get
    appendPar(document.body, s"Scalameta produces $tree")
  }
}