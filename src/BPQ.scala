import scala.collection.mutable.PriorityQueue

sealed trait Heap

case class BPQ(x:Int, pq:PriorityQueue[BPQ]) extends Ordered[BPQ] with  Heap{

	def compare(that:BPQ) = that.x compare this.x
}

case object Empty extends  Heap


object BPQ{
	def main(args: Array[String]){
		val bpq  = (1 to 5).toSeq.map(x => makeSingletonBPQ(x)).reduce((x, y) => merge(x,y))

		val newBpq = (6 to 10).toSeq.map(x => makeSingletonBPQ(x)).reduce((x, y) => merge(x,y))

		val bpq1 = insert(bpq, 0)

		val bpq2 = merge(bpq,  bpq1)

		//val minElem = findMin(bpq2)

		val extractedMin = extractMin(bpq2)
	}

	def makeSingletonBPQ(x:Int):Heap =  BPQ(x, new PriorityQueue())

	def merge(bpq1:Heap, bpq2:Heap):Heap = (bpq1, bpq2) match {

		case (Empty, bpq2) => bpq2
		case (bpq1, Empty) => bpq1
		case (BPQ(x, q) , BPQ(y, r) )=> if (x < y) BPQ(x, q ++ Seq(BPQ(y, r)))
		else  BPQ(y, r ++ Seq(BPQ(x,q)))
	}

	def insert(bpq:Heap, x:Int):Heap = bpq match {

		case Empty => makeSingletonBPQ(x)
		case _ => merge(bpq, makeSingletonBPQ(x))
	}


	def findMin(bpq:BPQ):Int = bpq.x


	def extractMin(bpq:Heap):Tuple2[Int, Heap] = bpq match {

		case Empty => Tuple2(-1 ,Empty)
		case BPQ(x, pq) => {

			val tuple  = Tuple2(pq.head, pq.tail)
			Tuple2(x, BPQ(tuple._1.x, tuple._1.pq ++ tuple._2))

		}
	}
}


//val bpq  = (1 to 5).toSeq.map(x => BPQ.makeSingletonBPQ(x)).reduce((x, y) => BPQ.merge(x,y))
//
//val newBpq = (6 to 10).toSeq.map(x => BPQ.makeSingletonBPQ(x)).reduce((x, y) => BPQ.merge(x,y))
//
//val bpq1 = BPQ.insert(bpq, 0)
//
//val bpq2 = BPQ.merge(bpq,  bpq1)
//
////val minElem = findMin(bpq2)
//
//val extractedMin = BPQ.extractMin(bpq2)