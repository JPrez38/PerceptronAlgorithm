package perceptron

import math._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object Perceptron {
	def dot[T <% Double](m1: Iterable[Int], m2: Iterable[Double]) : Int = {
		require(m1.size == m2.size) 
		if (((for ((x, y) <- m1 zip m2) yield x * y).sum) >= .5) return 1 else 0
	}

	def perceptronTrain(data: List[(Array[Int],Int)]) : (Array[Double],Int,Int) = {
		var weights = new Array[Double](data(0)._1.size)
		var k=0 //error count
		var iter=0
		var errorCount = -1
		while (iter < 15 && errorCount!=0) {
			errorCount=0
			for (x <- data) {
				val featureVector = x._1
				val desiredOutput = x._2
				
				val output = dot(featureVector,weights)
				val error = desiredOutput-output
				if (error!=0) {
					errorCount+=1
					weights = updateWeights(weights,featureVector,error)
				} 
			}
			k+=errorCount
			println(iter + ", errors:" + errorCount)
			iter+=1
		}

		return (weights,k,iter)
	}

	def perceptronTest(data: List[(Array[Int],Int)],weights: Array[Double]) : Double = {
		var errorCount=0
		for( x <- data) {
			val featureVector = x._1
			val desiredOutput = x._2

			val output = dot(featureVector,weights)
			val error = desiredOutput-output
			
			if (error != 0) {
				errorCount+=1
				println("Error! desiredOutput: " + desiredOutput + ", output: " + output)
			}
		}
		return errorCount/data.size.toDouble
	}

	def updateWeights(weights: Array[Double], vector: Array[Int], error: Int) : Array[Double] = {
		val learningRate = .25
		for (i <- 0 until vector.length) {
			weights(i) += (learningRate*error*vector(i))
		}
		return weights
	}
}

