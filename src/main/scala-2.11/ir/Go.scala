package ir

import ch.ethz.dal.tinyir.io.{ReutersRCVStream, ZipDirStream}
import ch.ethz.dal.tinyir.util.StopWatch
import ir.classifires.NaiveBayes

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
  val testStream = new ReutersRCVStream(testPath).stream//.take(10000)
  val trainStream = new ReutersRCVStream(trainPath).stream.take(50000)//!!! if change - have to delete cash
  val validationStream = new ReutersRCVStream(validationPath)

  val codeSet =  IRUtils.readAllRealCodes(trainStream)
  val allDocsVectors = IRUtils.readAllDocsVectors(trainStream)
  //val codesMap = IRUtils.getCodeValueMap(codesStream)

  // merging all vocab to one set
  val vocab = IRUtils.getSetOfDistinctTokens(allDocsVectors)
  val vocabSize = vocab.size

  println("vocab size= " + vocabSize)
  println("all words size = " + IRUtils.totalSumCoordinates(allDocsVectors))
  println("test files zise = " + testStream.length)

  watch.stop
  println("init complete " + watch.stopped)
  println("------------")

  watch.start

  val naiveBayes = new NaiveBayes(vocabSize, vocab, allDocsVectors, codeSet, trainStream, testStream)

  watch.stop
  println("done " + watch.stopped)

  //  9216727 ~ 9 Mill
  //  178069 ~ 180 K
  //  without stop words and with poster stemmer
  //  6304748 ~ 6,5 Mill
  //  152413 ~ 150 K

}
