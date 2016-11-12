package ir

import ch.ethz.dal.tinyir.io.{ReutersRCVStream, ZipDirStream}
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.util.StopWatch
import ir.classifires.{NaiveBayes, SVM, LogisticRegressionClassifier}
import ir.textclass.Scoring.Scoring
import ir.IRUtils.DocVector
import ir.IRUtils._

import scala.collection.mutable.ListBuffer

object Go extends App {

  // Set parameters
  var runMode = "vali" // "test"
  var classifierType = "nb"
  var baseDir = "/home/ajuodelis/eth/ir/data_real"

  if (!args.isEmpty) {
    baseDir = args(0)
    classifierType = args(1)
    runMode = args(2)
  }

  val codesPath = baseDir + "/codes"
  val trainPath = baseDir + "/train"
  val testPath = baseDir + "/test"
  val validationPath = baseDir + "/validation"

  val watch = new StopWatch()
  watch.start

  val codesStream = new ZipDirStream(codesPath).stream
  //val codesMap = IRUtils.getCodeValueMap(codesStream)
  val trainStream = new ReutersRCVStream(trainPath).stream.take(50000)//!!! if change - have to delete cache
  val validationReuters = new ReutersRCVStream(validationPath)
  val validationStream = validationReuters.stream
  val testStream = new ReutersRCVStream(testPath).stream//.take(10000)

  val codeSet =  IRUtils.readAllRealCodes(trainStream)
  val allDocsVectorsTrain = IRUtils.readAllDocsVectors(trainStream)

  // Depending on the Run Mode, use either Test or Validation Docs for the prediction
  var allDocVectorsToPredict = Map[String, DocVector]()
  if (runMode == "vali") {
    allDocVectorsToPredict = IRUtils.readAllDocsVectors(validationStream)
  }
  else{
    allDocVectorsToPredict = IRUtils.readAllDocsVectors(testStream)
  }


  // merging all vocab to one set
  val vocab = IRUtils.getSetOfDistinctTokens(allDocsVectorsTrain)
  val vocabSize = vocab.size

  println("vocab size= " + vocabSize)
  println("all words size = " + IRUtils.totalSumCoordinates(allDocsVectorsTrain))
  println("test files size = " + validationStream.length)

  watch.stop
  println("init complete " + watch.stopped)
  println("------------")


  // Start of Naive Bayes (if specified)
  if (classifierType == "nb") {
    watch.start

    // todo: run this for Validation or Test docs. Only for Test Docs you need to print a file with the results.
    // todo: For Validation docs, get the result via prediction method and then send it to the F1 Score method in IRUtils
    val naiveBayes = new NaiveBayes(vocabSize, vocab, allDocsVectorsTrain, codeSet, trainStream, validationStream)

    watch.stop
    println("done " + watch.stopped)

  }



  // Start of linear SVM (if specified)
  if (classifierType == "lsvm") {

    var resultsSVM = Map[String, ListBuffer[String]]()
    val lambda = 0.01
    val steps = 10000

    // Loop over all Codes, and for each code predict documents for that code. Then proceed to next code, etc.
    for (code <- codeSet) {

      val trainDocsFiltered = trainStream.filter(_.codes(code))
      val allTrainy = trainDocsFiltered.map(doc => doc.name -> 1).toMap
      // Create SVM Classifier object for current code (allTrainy are all the docs of the current code)
      val svmClassifier = new SVM(allDocsVectorsTrain, allTrainy, lambda, steps)

      // Now predict the docs that match this current code
      // allDocVectorsToPredict is of type Map[String, Map[String, Int]] -> Doc Name + Map of distinct tokens + coutn
      for (docVectorToPredict <- allDocVectorsToPredict) {

        val svm_result = svmClassifier.prediction(docVectorToPredict._2) //submit one Doc Vector = Distinct Token + Count of one doc

        // add code/label to result set if number is positive
        // resultsSVM will contain for each Doc name a list of codes found
        if (svm_result > 0.0) {
          if (resultsSVM.contains(docVectorToPredict._1)) resultsSVM.getOrElse(docVectorToPredict._1, ListBuffer[String]()) += code
          else resultsSVM += (docVectorToPredict._1 -> ListBuffer[String](code))
        }
      }
    } // end of code loop

    // Call calcualte F1 Score in case Run Mode is "Validation"
    if (runMode == "vali") {
      val score = new Scoring(validationReuters, resultsSVM)
      val f1ScoreSVM = score.calculateF1()
      println("The F1 Score for the SVM Classifier is: " + f1ScoreSVM)
    }

    // Write results to file in case Run Mode is "Test"
    if (runMode == "test") {
      IRUtils.saveResultMap(resultsSVM, "ir-project-2016-1-28-lsvm.txt")
    }

  }


  // Start of Logistic Regression (if specified)
  if (classifierType == "lr") {
    var resultsLogReg = Map[String, ListBuffer[String]]()
    val alphap = 1.0
    val alpham = 1.0

    // Loop over all Codes, and for each code predict documents for that code. Then proceed to next code, etc.
    for (code <- codeSet) {

      val trainDocsFiltered = trainStream.filter(_.codes(code))
      val allTrainy = trainDocsFiltered.map(doc => doc.name -> 1).toMap

      val lRClassifier= new LogisticRegressionClassifier(allDocsVectorsTrain, allTrainy, alphap, alpham, 100)

      // Now predict the docs that match this current code
      // allDocVectorsToPredict is of type Map[String, Map[String, Int]] -> Doc Name + Map of distinct tokens + coutn
      for (docVectorToPredict <- allDocVectorsToPredict) {

        val lr_result = lRClassifier.prediction(docVectorToPredict._2) //submit one Doc Vector = Distinct Token + Count of one doc

        // add code/label to result set if number is positive
        // resultsLogReg will contain for each Doc name a list of codes found
        if (lr_result > 0.0) {
          if (resultsLogReg.contains(docVectorToPredict._1)) resultsLogReg.getOrElse(docVectorToPredict._1, ListBuffer[String]()) += code
          else resultsLogReg += (docVectorToPredict._1 -> ListBuffer[String](code))
        }
      }
    } // end of code loop

    // Call calcualte F1 Score in case Run Mode is "Validation"
    if (runMode == "vali") {
      val score = new Scoring(validationReuters, resultsLogReg)
      val f1ScoreLogReg = score.calculateF1()
      println("The F1 Score for the Logistic Regression Classifier is: " + f1ScoreLogReg)
    }

    // Write results to file in case Run Mode is "Test"
    if (runMode == "test") {
      IRUtils.saveResultMap(resultsLogReg, "ir-project-2016-1-28-lr.txt")
    }

  }



}
