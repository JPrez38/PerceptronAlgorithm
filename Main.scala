import math._
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
	val vocab = buildVocabulary(trainingDataSet)
	//println(vocab.size + "," + emailList.size)
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

def buildVocabulary(emailList: List[(Char,String)]) : HashMap[String,Int] = {
	var vocabulary = new HashMap[String,Int]()
	for (email <- emailList) {
		val words = email._2.split(" ")
		var wordList = Set[String]()
		for (word <- words) {
			wordList = wordList + word
		}
		for (word <- wordList) {
			var count = vocabulary.getOrElse(word,0)
			count += 1
			vocabulary += word -> count
		}
	}
	vocabulary.remove("")
	return vocabulary.retain((k,v) => v > 30)
} 

val args = Array[String]()
main(args)