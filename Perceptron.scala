package perceptron

import math._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object Perceptron {
	def dot[T <% Double](m1: Iterable[T], m2: Iterable[T]) : Double = {
		require(m1.size == m2.size) 
		return (for ((x, y) <- m1 zip m2) yield x * y).sum
	}

	def perceptronTrain(data: List[(List[Int],Int)]) : (Array[Int],Int,Int) = {
		val learningRate = .1
		var weights = new Array[Int](data.size)
		var k=0
		var iter=0
		//print(weights(87) + weights.size)
		return (weights,k,iter)
	}
}

