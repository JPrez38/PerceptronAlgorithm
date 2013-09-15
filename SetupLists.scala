package perceptron

import math._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object Support {
	def getEmailList(inputFile: String) : List[(Char,String)] = {
		val start = System.currentTimeMillis
		var emails = List[(Char,String)]() 
		var newIndex = 0
		var currentIndex = 0
		def constructEmail(beginIndex: Int, endIndex: Int, input: String) : (Char,String) = {
			val emailText = input.slice(beginIndex,endIndex)
			return (emailText.head,emailText.substring(1,emailText.length))
		}
		def setIndex(nextIndex: Int, defaultValue: Int) : Int = {
			return (if (nextIndex > -1) nextIndex else defaultValue)
		}
		while({
			newIndex = math.min(setIndex(inputFile.indexOf("0",currentIndex+1),inputFile.length+1),
			setIndex(inputFile.indexOf("1",currentIndex+1),inputFile.length+1));
			newIndex != inputFile.length+1}) {

			val email = constructEmail(currentIndex,newIndex,inputFile)
			emails ::= email
			currentIndex=newIndex
		}
		val email = constructEmail(currentIndex,inputFile.length,inputFile)
		emails ::= email
		val end = System.currentTimeMillis
		println("getEmaillist time:" + (end-start))
		return emails.reverse
	}

	def buildVocabulary(emailList: List[(Char,String)]) : Map[String,Int] = {
		val start = System.currentTimeMillis
		var vocabulary = Map[String,Int]()
		for (email <- emailList) {
			val words = email._2.split(" ")
			var wordList = Set[String]()
			words.foreach(word => wordList += word)
			for (word <- wordList) {
				var count = vocabulary.getOrElse(word,0)
				count += 1
				vocabulary += word -> count
			}
		}
		vocabulary.remove("")
		val end = System.currentTimeMillis
		println("buildVocabulary time:" + (end-start))
		return vocabulary.retain((k,v) => v >= 30)
	} 



	def makeFeatureVector(emailList: List[(Char,String)], vocabList: Array[String]) : List[(List[Int],Int)] = {
		val start = System.currentTimeMillis
		println(emailList.size + "heello")
		var emailFeatureVector = List[(List[Int],Int)]()
		var i = 0
		for (email <- emailList) {
			var featureVector = List[Int](vocabList.size)
			var vocabIndex = 0
			val words = email._2.split(" ")
			var wordList = HashMap[String,Int]()
			words.foreach(word => wordList += word -> 0)
			for (vocabWord <- vocabList) {
				if(wordList.getOrElse(vocabWord,None) != None) featureVector ::= 1 else featureVector ::= 0
				vocabIndex+=1
			}
			val vector = (featureVector,email._1)
			emailFeatureVector ::= vector
		}
		
		val end = System.currentTimeMillis
		println("makeFeatureVector time:" + (end-start))
		return emailFeatureVector
	}
}