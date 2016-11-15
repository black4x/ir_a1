package ir

import ch.ethz.dal.tinyir.io.{ReutersRCVStream, ZipDirStream}
import ch.ethz.dal.tinyir.util.StopWatch
import ir.utils.{IRUtils, Scoring}
import ir.classifires.{LogisticRegression, NaiveBayes, SVM}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Go extends App {

  // defining param constants
  val TEST_MODE = "test"
  val VALIDATION_MODE = "vali"

  val BAYES = "nb"
  val SVM = "lsvm"
  val LINEAR_REGRESSION = "lr"

  // Set default parameters
  var baseDir = "/home/ajuodelis/eth/ir/data_real"
  var classifierType = BAYES
  var runMode = VALIDATION_MODE

  val codesPath = baseDir + "/codes"
  val trainPath = baseDir + "/train"
  val predictPath =
    if (runMode == VALIDATION_MODE) baseDir + "/validation"
    else baseDir + "/test"

  if (!args.isEmpty) {
    baseDir = args(0)
    classifierType = args(1)
    runMode = args(2)
  }
  val watch = new StopWatch()
  val codesStream = new ZipDirStream(codesPath).stream
  val trainStream = new ReutersRCVStream(trainPath).stream
  val predictStream = new ReutersRCVStream(predictPath).stream
  watch.start

  println("initializing ... ")
  //reading all codes from training set
  val codeSet = trainStream.map(doc => doc.codes).reduce(_ ++ _)//IRUtils.readAllRealCodes(trainStream)
  // making map: Doc_Name -> Doc_Vector, by Doc_Vector means: Distinct_Token -> Frequency_Number
  val allDocsVectorsTrain = IRUtils.getAllDocsVectors(trainStream)
  //val allDocsVectorsTrain = IRUtils.readAllDocsVectors(trainStream)

  // merging all vocab to one set, in order to get distinct tokens
  val vocab = IRUtils.getSetOfDistinctTokens(allDocsVectorsTrain)
  val vocabSize = vocab.size

  println("vocab size= " + vocabSize)
  println("all words size = " + IRUtils.totalSumCoordinates(allDocsVectorsTrain))
  println("predict files size = " + predictStream.length)

  watch.stop
  println("init complete " + watch.stopped)
  println("------------")

  watch.start
  // Start of Naive Bayes
  var resultsMap = mutable.Map[String, ListBuffer[String]]()

  if (classifierType == BAYES) {
    println("very naïve Bayes ...")

    NaiveBayes.predict(vocabSize, vocab, allDocsVectorsTrain, codeSet, trainStream, predictStream)
    NaiveBayes.collectResults(resultsMap)
  }

  // Start of linear SVM (if specified)
  if (classifierType == SVM) {
    val allDocVectorsToPredict = IRUtils.getAllDocsVectors(predictStream)
    println("support vector machine  ...")

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
        if (svm_result > 0.0) IRUtils.addCodeToResultMap(resultsMap, docVectorToPredict._1, code)
      }
    } // end of code loop
  }


  // Start of Logistic Regression (if specified)
  if (classifierType == LINEAR_REGRESSION) {
    val allDocVectorsToPredict = IRUtils.getAllDocsVectors(predictStream)
    println("logistic regression ...")

    val alphap = 1.0
    val alpham = 1.0
    val steps = 10000

    // Loop over all Codes, and for each code predict documents for that code. Then proceed to next code, etc.
    for (code <- codeSet) {

      val trainDocsFiltered = trainStream.filter(_.codes(code))
      val allTrainy = trainDocsFiltered.map(doc => doc.name -> 1).toMap

      val lRClassifier = new LogisticRegression(allDocsVectorsTrain, allTrainy, alphap, alpham, steps)

      // Now predict the docs that match this current code
      // allDocVectorsToPredict is of type Map[String, Map[String, Int]] -> Doc Name + Map of distinct tokens + coutn
      for (docVectorToPredict <- allDocVectorsToPredict) {
        val lr_result = lRClassifier.prediction(docVectorToPredict._2) //submit one Doc Vector = Distinct Token + Count of one doc
        if (lr_result > 0.0) IRUtils.addCodeToResultMap(resultsMap, docVectorToPredict._1, code)
      }
    } // end of code loop
  }

  // Call calcualte F1 Score in case Run Mode is "Validation"
  if (runMode == VALIDATION_MODE) {
    val score = new Scoring(predictStream, resultsMap)
    val f1ScoreSVM = score.calculateF1()
    println("The F1 Score for the " + classifierType + " Classifier is: " + f1ScoreSVM)
  }
  // Write results to file in case Run Mode is "Test"
  else {
    IRUtils.saveResultMap(resultsMap, "ir-project-2016-1-28-" + classifierType + ".txt")
  }

  watch.stop
  println("Classifier execution done " + watch.stopped)

}
