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
		val emailList = helper.getEmailList(lines)
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
		val featureVectors = helper.makeFeatureVector(trainingDataSet,vocabList)
		//println(featureVectors.get(validationDataSet(5)))
		//featureVectors.foreach(vect => println(vect))
		println(vocabList.size + "," + emailList.size + "," + featureVectors.size)
		//featureVectors(3)._1.foreach(i => print(i))
		val yourmom = Perceptron.perceptronTrain(featureVectors)
		val end = System.currentTimeMillis

		println("Running time: " + (end-start) + " millis")
	}
}