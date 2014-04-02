package controllers

import com.faacets._
import comp._
import dec._
import pretty._
import PrettyImplicits._
import PrettyParams.Default._

case class GraphNode(id: Int, label: String, link: Option[String], tex: String)
case class GraphEdge(from: Int, onto: Int, label:String, tooltip: Option[String])

case class Graph(nodes: Seq[GraphNode], edges: Seq[GraphEdge]) {
  def nextID = (0 +: nodes.map(_.id)).max + 1
}

object Graph {
  def empty = Graph(Seq.empty[GraphNode], Seq.empty[GraphEdge])
}

object DecToGraph {
  def apply(c: CanonicalComp, d: Decomposition, forceLabel: Option[String], graph: Graph = Graph.empty)(implicit session: c.CompSession): (Graph, Int) = {
    val thisBE = d.fromCompendium(c).compute
    val (thisLabel, thisLink) = (d, thisBE.shortName) match {
      case (CanonicalExpression(index), Some(sn)) => (sn, Some(s"/$index"))
      case (CanonicalExpression(index), _) => (s"faacets.com/$index", Some(s"/$index"))
      case _ => (thisBE.scenario.toText, None)
    }
    val thisFinalLabel = forceLabel.getOrElse(thisLabel)
    val thisTeX = tex(thisBE.expr.tensor)
    val thisID = graph.nextID
    val thisNode = GraphNode(thisID, thisFinalLabel, thisLink, thisTeX)
    val graphWithThis = graph.copy(nodes = graph.nodes :+ thisNode)
    d match {
      case CanonicalExpression(index) =>
        (graphWithThis, thisID)
      case t: Transform[_] =>
        val (newGraph, childID) = DecToGraph(c, t.of, None, graphWithThis)
        val (edgeLabel, edgeTooltip) = t match {
          case AffineTransform(affine, _) => ("affine", Some(affine.toText))
          case LiftingTransform(lifting, _) => ("lifting", Some(lifting.toText))
          case OppositeTransform(_) => ("opposite", None)
          case PermutationTransform(permutation, _) => ("permutation", Some(permutation.toText))
          case RedundantTransform(coeffs, _) => ("redundant", Some(coeffs.toString))
          case ReorderingTransform(reordering, _) => ("reordering", Some(reordering.toText))
          case RepresentationTransform(representation, _) => 
            ("representation", Some(representation.toText))
          case RepresentativeTransform(representative, _) => ("representative", Some(representative.toString))
        }
        val thisEdge = GraphEdge(childID, thisID, edgeLabel, edgeTooltip)
        (newGraph.copy(edges = newGraph.edges :+ thisEdge), thisID)
      case be: BellExpression =>
        (graphWithThis, thisID)
      case bep: BellExpressionProduct =>
        val finalGraph = (graphWithThis /: bep.of) {
          case (currentGraph, childDec) =>
            val (newGraph, childID) = DecToGraph(c, childDec, None, currentGraph)
            val newEdge = GraphEdge(childID, thisID, "composite", None)
            newGraph.copy(edges = newGraph.edges :+ newEdge)
        }
        (finalGraph, thisID)
    }
  }
}
