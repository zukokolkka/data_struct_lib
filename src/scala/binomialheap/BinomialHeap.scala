package scala.binomialheap

class BinomialHeap {
  var root: BNode = null

  def isEmpty: Boolean = {
    return root == null
  }

  def insert(x: BNode) {
    val h: BinomialHeap = new BinomialHeap
    h.root = x
    val newH: BinomialHeap = this.merge(h)
    root = newH.root
  }

  def merge(h2: BinomialHeap): BinomialHeap = {
    val h: BinomialHeap = new BinomialHeap
    h.root = BinomialHeap.mergeRootList(this, h2)
    root = null
    h2.root = null
    if (h.root == null) return h
    var prevX: BNode = null
    var x: BNode = h.root
    var nextX: BNode = x.sibling
    while (nextX != null) {
      if (x.degree != nextX.degree || (nextX.sibling != null && nextX.sibling.degree == x.degree)) {
        prevX = x
        x = nextX
      }
      else {
        if (x.key < nextX.key) {
          x.sibling = nextX.sibling
          BinomialHeap.pair(nextX, x)
        }
        else {
          if (prevX == null) h.root = nextX
          else prevX.sibling = nextX
          BinomialHeap.pair(x, nextX)
          x = nextX
        }
      }
      nextX = x.sibling
    }
    h
  }

  def extractMin: (Int, BinomialHeap) = {
    if (root == null) return (-1, this)
    var x: BNode = root
    var y: BNode = x.sibling
    var pred: BNode = x
    var xPred: BNode = null
    while (y != null) {
      {
        if (y.key < x.key) {
          x = y
          xPred = pred
        }
        pred = y
        y = y.sibling
      }
    }
    if (x eq root)
      root = x.sibling
    else
      xPred.sibling = x.sibling

    val h: BinomialHeap = new BinomialHeap
    var z: BNode = x.child
    while (z != null) {
      val next: BNode = z.sibling
      z.sibling = h.root
      h.root = z
      z = next
    }
    val newH: BinomialHeap = this.merge(h)
    root = newH.root
    (x.vertex, this)
  }

  def decreaseKey(vertex: Int, k: Int, dist: Array[BNode]) {
    val x: BNode = dist(vertex)
    x.key = k
    var y: BNode = x
    var z: BNode = y.parent
    while (z != null && (y.key < z.key)) {
      var v: Int = y.key
      y.key = z.key
      z.key = v
      v = y.vertex
      y.vertex = z.vertex
      z.vertex = v
      dist(z.vertex) = z
      dist(y.vertex) = y
      y = z
      z = y.parent
    }
  }
}

object BinomialHeap {
  private def mergeRootList(h1: BinomialHeap, h2: BinomialHeap): BNode = {
    if (h1.root == null)
      return h2.root
    else if (h2.root == null)
      return h1.root
    else {
      var head: BNode = null
      var tail: BNode = null
      var h1Next: BNode = h1.root
      var h2Next: BNode = h2.root
      if (h1.root.degree <= h2.root.degree) {
        head = h1.root
        h1Next = h1Next.sibling
      }
      else {
        head = h2.root
        h2Next = h2Next.sibling
      }
      tail = head

      while (h1Next != null && h2Next != null) {
        if (h1Next.degree <= h2Next.degree) {
          tail.sibling = h1Next
          h1Next = h1Next.sibling
        }
        else {
          tail.sibling = h2Next
          h2Next = h2Next.sibling
        }
        tail = tail.sibling
      }
      if (h1Next != null) tail.sibling = h1Next
      else tail.sibling = h2Next
      return head
    }
  }

  def pair(y: BNode, z: BNode) {
    y.parent = z
    y.sibling = z.child
    z.child = y
    z.degree += 1
  }

  // TEST!
  def main(args: Array[String]) {
    val b: BinomialHeap = new BinomialHeap
    var t: Array[BNode] = null
    t = new Array[BNode](10000)
    var i: Int = 0
    for(i <- 0 until 10000) {
      t(i) = new BNode(i, (100000D * Math.random).round.toInt)
      b.insert(t(i))
    }
    while (!b.isEmpty) {
      val x: Int = b.extractMin._1
      System.out.println(t(x).vertex + "--" + t(x).key)
    }
  }
}

