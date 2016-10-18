# Hello-World-
learning Scala

// ======== Merge Sort ============	
  import math.Ordering
	def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
		val n = xs.length/2
		if (n == 0) xs
		else {
			def merge(xs: List[T], ys: List[T]): List[T] = {
			  (xs,ys) match {
			  	case (Nil,ys) => ys
			  	case (xs, Nil) => xs
			  	case (x :: xsl, y :: ysl) =>
			  		if (ord.lt(x, y)) x :: merge(xsl, ys)
			  		else y :: merge(xs, ysl)
			  }
			}
			
			val (fst, snd) = xs splitAt n
			merge(msort(fst)(ord), msort(snd)(ord))
		}
	}

	def a = List(3,5,9292, -1, 9, 2,1,0)
	msort(a)(Ordering.Int)
	
	def b = List("banana", "apple", "Orange", "Peach")
	msort(b)(Ordering.String)
	
// ======= higher order list function =====

	def pack[T](xs: List[T]) : List[List[T]] =
		xs match {
			case Nil => Nil
			case y :: ys =>
				val (first, rest) = xs span (x => x == y)
				first :: pack(rest)
			
		}                                 //> pack: [T](xs: List[T])List[List[T]]
	pack(List(1,1,1,2,2,3))                   //> res0: List[List[Int]] = List(List(1, 1, 1), List(2, 2), List(3))
	
	def encode[T](xs: List[T]) : List[(T,Int)] =
		pack(xs) map (x => (x.head, x.length))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
	
	
	encode(List("a","a","a","b","b","c"))     //> res1: List[(String, Int)] = List((a,3), (b,2), (c,1))
	
// ======= number combiner ====

	1 to 2 flatMap (x => (4 to 5) map (y => (x,y))) //Vector((1,4), (1,5), (2,4), (2,5))
	
// ===== zip list ====
    List(1,2,3) zip List(4,5,6) //> res0: List[(Int, Int)] = List((1,4), (2,5), (3,6)) 

// ===== isPrime ====
	
