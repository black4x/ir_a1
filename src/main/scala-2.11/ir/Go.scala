package ir

import ch.ethz.dal.tinyir.io.{ReutersRCVStream, ZipDirStream}
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.util.StopWatch
import ir.classifires.{NaiveBayes, SVM}
import ir.textclass.Scoring.Scoring

import scala.collection.mutable.ListBuffer

object Go extends App {

  // TODO read baseDir from args
  val baseDir = "/home/ajuodelis/eth/ir/data_real"

  val codesPath = baseDir + "/codes"
  val trainPath = baseDir + "/train"
  val testPath = baseDir + "/test"
  val validationPath = baseDir + "/validation"

  val watch = new StopWatch()
  watch.start

  val codesStream = new ZipDirStream(codesPath).stream
  val codesMap = IRUtils.getCodeValueMap(codesStream)
  val trainStream = new ReutersRCVStream(trainPath).stream.take(50000)//!!! if change - have to delete cache
  val validationReuters = new ReutersRCVStream(validationPath)
  val validationStream = validationReuters.stream
  val testStream = new ReutersRCVStream(testPath).stream//.take(10000)

  val codeSet =  IRUtils.readAllRealCodes(trainStream)
  val allDocsVectorsTrain = IRUtils.readAllDocsVectors(trainStream)
  val allDocVecotorsValidation = IRUtils.readAllDocsVectors(validationStream)


  // merging all vocab to one set
  val vocab = IRUtils.getSetOfDistinctTokens(allDocsVectorsTrain)
  val vocabSize = vocab.size

  println("vocab size= " + vocabSize)
  println("all words size = " + IRUtils.totalSumCoordinates(allDocsVectorsTrain))
  println("test files zise = " + validationStream.length)

  watch.stop
  println("init complete " + watch.stopped)
  println("------------")

  watch.start

  // todo: run this for Validation and Test docs. Only for Test Docs you need to print a file with the results.
  // todo: For Validation docs, get the result via prediction method and then send it to the F1 Score method in IRUtils
  val naiveBayes = new NaiveBayes(vocabSize, vocab, allDocsVectorsTrain, codeSet, trainStream, validationStream)



  watch.stop
  println("done " + watch.stopped)



  // Start of SVM
  var resultsSVM = Map[String, ListBuffer[String]]()
  val lambda = 0.01
  val steps = 10000

  // Start of processing VALIDATION Docs
  // Loop over all Codes, and for each code predict documents for that code. Then proceed to next code, etc.
  for ((code, text) <- codesMap) {

    val trainDocsFiltered=trainStream.filter(_.codes(code))
    val allTrainy=trainDocsFiltered.map(doc => doc.name ->1).toMap
    // Create SVM Classifier object for current code (allTrainy are all the docs of the current code)
    val svmClassifier = new SVM(allDocsVectorsTrain,allTrainy,lambda,steps)

    // Now predict the docs that match this current code
    // allDocVectorsValidation is of type Map[String, Map[String, Int]] -> Doc Name + Map of distinct tokens + coutn
    for (validationDoc <- allDocVecotorsValidation) {

      val svm_result = svmClassifier.prediction(validationDoc._2) //submit one Doc Vector = Distinct Token + Count of one doc

      // add code/label to result set if number is positive
      // resultsSVM will contain for each Doc name a list of codes found
      if (svm_result > 0.0){
        if (resultsSVM.contains(validationDoc._1)) resultsSVM.getOrElse(validationDoc._1, ListBuffer[String]()) += code
        else resultsSVM += (validationDoc._1 -> ListBuffer[String](code))
      }
    }

    // Call calcualte F1 Score for the validation documents
    val score = new Scoring(validationReuters, resultsSVM)
    val f1ScoreSVM = score.calculateF1()
    println("The F1 Score for the SVM Classifier is: " + f1ScoreSVM)

    // Start of processing Test Docs

    

  }





  //SVM: Geht ca 175.72737288 sec.
  // allTrainy = list of doc ID of code and 1





  //  9216727 ~ 9 Mill
  //  178069 ~ 180 K
  //  without stop words and with poster stemmer
  //  6304748 ~ 6,5 Mill
  //  152413 ~ 150 K

}
