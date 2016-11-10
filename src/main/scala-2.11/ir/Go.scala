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


  val watch = new StopWatch()
  watch.start

  val codesStream = new ZipDirStream(codesPath).stream
  val trainStream = new ReutersRCVStream(trainPath).stream.take(50000)//!!! if change - have to delete cash

  val codeSet =  IRUtils.readAllRealCodes(trainStream)
  val allDocsVectors = IRUtils.readAllDocsVectors(trainStream)
  val codesMap = IRUtils.getCodeValueMap(codesStream)

  // merging all tokens to one set
  val allVocabSet = IRUtils.getSetOfDistinctTokens(allDocsVectors)

  println("vocab size= " + allVocabSet.size)
  println("all words size= " + IRUtils.totalSumCoordinates(allDocsVectors))

  watch.stop
  println("init " + watch.stopped)
  println("------------")

  watch.start

  val naiveBayes = new NaiveBayes(allDocsVectors, allVocabSet, codeSet, trainStream)

  watch.stop
  println("done " + watch.stopped)

  //  9216727 ~ 9 Mill
  //  178069 ~ 180 K
  //  without stop words and with poster stemmer
  //  6304748 ~ 6,5 Mill
  //  152413 ~ 150 K

}
