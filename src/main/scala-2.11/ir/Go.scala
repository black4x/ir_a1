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

  val codesStream = new ZipDirStream(codesPath).stream

  val codesMap = IRUtils.getCodeValueMap(codesStream)

  watch.start

  val trainStream = new ReutersRCVStream(trainPath).stream.take(5000)
  val allDocsVectors = IRUtils.getAllDocsVectors(trainStream)

  // merging all tokens to one set
  val allVocabSet = IRUtils.getSetOfDistinctTokens(allDocsVectors)

  println (allVocabSet.size)
  println(IRUtils.totalSumCoordinates(allDocsVectors))

  watch.stop
  println(watch.stopped)

  watch.start

  val naiveBayes = new NaiveBayes(allDocsVectors, allVocabSet, codesMap, trainStream)

  watch.stop
  println(watch.stopped)

  //  9216727 ~ 9 Mill
  //  178069 ~ 180 K
  //  without stop words and with poster stemmer
  //  6304748 ~ 6,5 Mill
  //  152413 ~ 150 K

}
