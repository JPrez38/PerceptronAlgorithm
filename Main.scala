import math._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

def main(args: Array[String]) {
	val start = System.currentTimeMillis
	import scala.io.Source 
	val trainingSize = 4000
	val source = Source.fromFile("ps1_data/spam_train.txt")
	val lines = source.getLines mkString ""
	val emailList = getEmailList(lines)
	val tmp = emailList.splitAt(trainingSize)
	val trainingDataSet = tmp._1
	val validationDataSet = tmp._2
	//trainingDataSet.foreach(email => println(email))
	val vocabTally = buildVocabulary(trainingDataSet)
	var vocabList = new Array[String](vocabTally.size)
	var vocabIndex = 0
	for(word <- vocabTally) { //converts to simple array for faster iteration
		vocabList(vocabIndex) = word._1
		vocabIndex+=1
	}
	//vocabTally.foreach(word => println(word))
	val featureVectors = makeFeatureVector(validationDataSet,vocabList)
	//println(featureVectors.get(validationDataSet(5)))
	//featureVectors.foreach(vect => println(vect))
	//println(vocabList.size + "," + emailList.size)
	val end = System.currentTimeMillis

	println("Running time: " + (end-start) + " millis")

}

def getEmailList(inputFile: String) : List[(Char,String)] = {
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
	return emails.reverse
}

def buildVocabulary(emailList: List[(Char,String)]) : Map[String,Int] = {
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
	return vocabulary.retain((k,v) => v >= 30)
} 



def makeFeatureVector(emailList: List[(Char,String)], vocabList: Array[String]) : Map[(Char,String),List[Int]] = {
	var emailFeatureVector = Map[(Char,String),List[Int]]()
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
		//featureVector.foreach(vect => println(vect))
		emailFeatureVector += email -> featureVector
		//emailFeatureVector.get(email).foreach(x => println(x))
	}
	return emailFeatureVector
}

val args = Array[String]()
main(args)