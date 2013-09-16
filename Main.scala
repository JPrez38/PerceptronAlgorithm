package perceptron

import math._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object Main {
	def main(args: Array[String]) {
		val helper = Support
		val start = System.currentTimeMillis
		import scala.io.Source 
		val trainingSize = 4000
		val source = Source.fromFile("ps1_data/spam_train.txt")
		val lines = source.getLines mkString ""
		val data = lines.replaceAll("number"," number ")
		val emailList = helper.getEmailList(data)
		val tmp = emailList.splitAt(trainingSize)
		val trainingDataSet = tmp._1
		val validationDataSet = tmp._2
		//trainingDataSet.foreach(email => println(email))
		val vocabTally = helper.buildVocabulary(trainingDataSet)
		var vocabList = new Array[String](vocabTally.size)
		var vocabIndex = 0
		for(word <- vocabTally) { //converts to simple array for faster iteration
			vocabList(vocabIndex) = word._1
			vocabIndex+=1
		}
		//vocabTally.foreach(word => println(word))
		val trainingFeatureVectors = helper.makeFeatureVector(trainingDataSet,vocabList)
		println(vocabList.size + "," + emailList.size + "," + trainingFeatureVectors.size)
		val trainingData = Perceptron.perceptronTrain(trainingFeatureVectors)
		val weights = trainingData._1
		val k = trainingData._2
		val iter = trainingData._3
		val validationTrainingVectors = helper.makeFeatureVector(validationDataSet,vocabList)
		val testError = Perceptron.perceptronTest(validationTrainingVectors,weights)
		println(f"Test Error: $testError%1.3f")
		val heavyWeights = getHeaviestWeights(weights,vocabList)
		val mostPositiveWeights = heavyWeights._1
		val mostNegativeWeights = heavyWeights._2
		mostPositiveWeights.foreach {x => println(x._1 + ":" + x._2)}
		println()
		mostNegativeWeights.foreach {x => println(x._1 + ":" + x._2)}
		val end = System.currentTimeMillis

		println("Running time: " + (end-start) + " millis")
	}

	def getHeaviestWeights(weights: Array[Double],vocabList: Array[String]) : (List[(String,Double)],List[(String,Double)]) = {
		var size = 15
		var mostPositveWeights = new Array[Double](size)
		var mostNegativeWeights = new Array[Double](size)
		var mostPositveWeightsIndex = new Array[Int](size)
		var mostNegativeWeightsIndex = new Array[Int](size)
		var mostPositive = List[(String,Double)]()
		var mostNegative = List[(String,Double)]()

		for (i <- 0 until mostNegativeWeights.length) {mostNegativeWeights(i) = 10}

		for(x <- 0 until weights.length) {
			var i = 0
			var j = 0
			while( i < mostPositveWeights.length && weights(x) > mostPositveWeights(i)) {
				if (i > 0) {
					mostPositveWeights(i-1) = mostPositveWeights(i)
					mostPositveWeightsIndex(i-1) = mostPositveWeightsIndex(i)
				} 
				mostPositveWeights(i) = weights(x)
				mostPositveWeightsIndex(i)=x
				i+=1
			}
			while(j < mostNegativeWeights.length && weights(x) < mostNegativeWeights(j)) {
				if (j > 0) {
					mostNegativeWeights(j-1) = mostNegativeWeights(j)
					mostNegativeWeightsIndex(j-1) = mostNegativeWeightsIndex(j)
				} 
				mostNegativeWeights(j) = weights(x)
				mostNegativeWeightsIndex(j)=x
				j+=1
			}
		} 
		for (j <- 0 until size) {
			val tmp1 = (vocabList(mostPositveWeightsIndex(j)), mostPositveWeights(j))
			val tmp2 = (vocabList(mostNegativeWeightsIndex(j)),mostNegativeWeights(j))
			mostPositive ::= tmp1
			mostNegative ::= tmp2
		}
		return (mostPositive,mostNegative)
	}
}