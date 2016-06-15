package scala.binomialheap

class BNode(var vertex: Int, var key: Int) {
  var parent: BNode = null
  var child: BNode = null
  var sibling: BNode = null
  var degree: Int = 0
}