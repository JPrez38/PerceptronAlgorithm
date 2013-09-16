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
		val end = System.currentTimeMillis

		println("Running time: " + (end-start) + " millis")
	}

	//def getHeaviestWeights(weights: Array[Double],vocabList: Map[String,Int]) : Array[Double] = {
	//	var mostPositveWeights = new Array[Double](15)
	//	var mostNegativeWeights = new Array[Double](15)

	//}
}