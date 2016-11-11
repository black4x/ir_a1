package ir

import ch.ethz.dal.tinyir.io.{ReutersRCVStream, ZipDirStream}
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.util.StopWatch
import ir.classifires.{NaiveBayes, SVM}

object Go extends App {

  // TODO read baseDir from args
  val baseDir = "/home/ajuodelis/eth/ir/data_real"

  val codesPath = baseDir + "/codes"
  val trainPath = baseDir + "/train"
  val testPath = baseDir + "/test"

  val watch = new StopWatch()
  watch.start

  val codesStream = new ZipDirStream(codesPath).stream
  val testStream = new ReutersRCVStream(testPath).stream//.take(10000)
  val trainStream = new ReutersRCVStream(trainPath).stream.take(50000)//!!! if change - have to delete cash


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



  def createVectorFromDoc(doc: XMLDocument): Map[String,Int] = StopWords.filterOutSW(Tokenizer.tokenize(doc.content)).groupBy(identity).mapValues(_.size)

  val lambda=0.01
  val steps=10000

  val trainDocsFiltered=trainStream.filter(_.codes("M13"))
  val allTrainy=trainDocsFiltered.map(doc => doc.name ->1).toMap

  //val allValVectors = validateStream.map(doc => doc.name -> createVectorFromDoc(doc)).toMap

  //SVM: Geht ca 175.72737288 sec.
  val svmClassifier= new SVM(allDocsVectors,allTrainy,lambda,steps)




  //  9216727 ~ 9 Mill
  //  178069 ~ 180 K
  //  without stop words and with poster stemmer
  //  6304748 ~ 6,5 Mill
  //  152413 ~ 150 K

}
