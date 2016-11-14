package ir

import ch.ethz.dal.tinyir.io.{ReutersRCVStream, ZipDirStream}
import ch.ethz.dal.tinyir.util.StopWatch
import ir.classifires.{LogisticRegressionClassifier, NaiveBayes, SVM}
import ir.textclass.Scoring.Scoring

import scala.collection.mutable.ListBuffer

object Go extends App {

  // Set default parameters

  // vali, test
  var runMode = "vali"
  //lsvm, nb, lr
  var classifierType = "lr"

  var baseDir = "/home/ajuodelis/eth/ir/data_real"

  if (!args.isEmpty) {
    baseDir = args(0)
    classifierType = args(1)
    runMode = args(2)
  }

  val codesPath = baseDir + "/codes"
  val trainPath = baseDir + "/train"

  val predictPath =
    if (runMode == "vali") baseDir + "/validation"
    else baseDir + "/test"

  val watch = new StopWatch()
  watch.start

  println("initializing ... ")
  val codesStream = new ZipDirStream(codesPath).stream
  val trainStream = new ReutersRCVStream(trainPath).stream
  val predictStream = new ReutersRCVStream(predictPath).stream
  //reading all codes from training set
  val codeSet = trainStream.map(doc => doc.codes).reduce(_ ++ _)
  // making map: Doc_Name -> Doc_Vector, by Doc_Vector means: Distinct_Token -> Frequency_Number
  val allDocsVectorsTrain = IRUtils.getAllDocsVectors(trainStream)
  val allDocVectorsToPredict = IRUtils.getAllDocsVectors(predictStream)

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
  var resultsMap = Map[String, ListBuffer[String]]()

  if (classifierType == "nb") {
    println("very naïve Bayes ...")

    // todo: run this for Validation or Test docs. Only for Test Docs you need to print a file with the results.
    // todo: For Validation docs, get the result via prediction method and then send it to the F1 Score method in IRUtils
    val naiveBayes = new NaiveBayes(vocabSize, vocab, allDocsVectorsTrain, codeSet, trainStream, predictStream)

  }

  // Start of linear SVM (if specified)
  if (classifierType == "lsvm") {
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
        if (svm_result > 0.0) addCodeToResultMap(docVectorToPredict._1, code)
      }
    } // end of code loop
  }


  // Start of Logistic Regression (if specified)
  if (classifierType == "lr") {
    println("logistic regression ...")

    val alphap = 1.0
    val alpham = 1.0
    val steps = 10000

    // Loop over all Codes, and for each code predict documents for that code. Then proceed to next code, etc.
    for (code <- codeSet) {

      val trainDocsFiltered = trainStream.filter(_.codes(code))
      val allTrainy = trainDocsFiltered.map(doc => doc.name -> 1).toMap

      val lRClassifier = new LogisticRegressionClassifier(allDocsVectorsTrain, allTrainy, alphap, alpham, steps)

      // Now predict the docs that match this current code
      // allDocVectorsToPredict is of type Map[String, Map[String, Int]] -> Doc Name + Map of distinct tokens + coutn
      for (docVectorToPredict <- allDocVectorsToPredict) {
        val lr_result = lRClassifier.prediction(docVectorToPredict._2) //submit one Doc Vector = Distinct Token + Count of one doc
        if (lr_result > 0.0) addCodeToResultMap(docVectorToPredict._1, code)
      }
    } // end of code loop
  }

  // Call calcualte F1 Score in case Run Mode is "Validation"
  if (runMode == "vali") {
    val score = new Scoring(predictStream, resultsMap)
    val f1ScoreSVM = score.calculateF1()
    println("The F1 Score for the SVM Classifier is: " + f1ScoreSVM)
  }

  // Write results to file in case Run Mode is "Test"
  if (runMode == "test") {
    IRUtils.saveResultMap(resultsMap, "ir-project-2016-1-28-" + classifierType + ".txt")
  }

  watch.stop
  println("Classifier execution done " + watch.stopped)


  // add code/label to result set if number is positive
  // resultsLogReg will contain for each Doc name a list of codes found
  def addCodeToResultMap(docName: String, newCode: String): Unit = {
    if (resultsMap.contains(docName)) resultsMap.getOrElse(docName, ListBuffer[String]()) += newCode
    else resultsMap += (docName -> ListBuffer[String](newCode))
  }
}
