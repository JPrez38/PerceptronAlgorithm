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
	println(emailList(54))
	val tmp = emailList.splitAt(trainingSize)
	val trainingDataSet = tmp._1
	val validationDataSet = tmp._2
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

def buildVocabulary(emailList: List[(Char,String)]) : HashMap[Int,String] = {
	val map = new HashMap[Int,String]()
	return map
} 


val args = Array[String]()
main(args)