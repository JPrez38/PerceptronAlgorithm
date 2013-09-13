import math._
import scala.collection.mutable.HashMap

def main(args: Array[String]) {
	val start = System.currentTimeMillis
	import scala.io.Source 
	val trainingSize = 4000
	var emailCount = 0
	var currentEmail = ""
	val source = Source.fromFile("ps1_data/spam_train.txt")
	val lines = source.getLines mkString "\n"
	val emailList = getEmailList(lines)
	val tmp = emailList.splitAt(trainingSize)
	val trainingDataSet = tmp._1
	val validationDataSet = tmp._2
	val vocab = buildVocabulary(trainingDataSet)
	for( word <- vocab) {
		println(word.toString)
	}
	val end = System.currentTimeMillis

	println("Running time: " + (end-start) + " millis")


}

def getEmailList(inputFile: String) : List[(Char,String)] = {
	var emails = List[(Char,String)]() 
	var newIndex = 0
	var newZeroIndex =0
	var newOneIndex = 0
	var currentIndex = 0
	while({
		newZeroIndex = inputFile.indexOf("0",currentIndex+1);
		newOneIndex = inputFile.indexOf("1",currentIndex+1);
		newZeroIndex = if (newZeroIndex > -1) newZeroIndex else inputFile.length+1;
		newOneIndex = if (newOneIndex > -1) newOneIndex else inputFile.length+1;
		newIndex = math.min(newZeroIndex,newOneIndex);
		newIndex != inputFile.length+1}) {

		val emailText = inputFile.slice(currentIndex,newIndex)
		val email = (emailText.head,emailText.substring(1,emailText.length))
		emails ::= email
		currentIndex=newIndex
	}
	val emailText = inputFile.slice(currentIndex,inputFile.length)
	val email = (emailText.head,emailText.substring(1,emailText.length))
	emails ::= email
	emails = emails.reverse
	return emails
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

	return vocabulary
} 

def show(x: Option[Int]) = x match {
      case Some(s) => s
      case None => -1
   }


val args = Array[String]()
main(args)