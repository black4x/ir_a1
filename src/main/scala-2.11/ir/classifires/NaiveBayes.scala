package ir.classifires


import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch
import ir.IRUtils
import ir.IRUtils.DocVector

<<<<<<< HEAD
class NaiveBayes(val vocabSize: Int, val vocab: Set[String],
                 val allDocsVectors: Map[String, DocVector],
                 codeSet: Set[String],
                 stream: Stream[XMLDocument]) {
=======

class NaiveBayes(val vocabSize: Int, val vocab: Set[String],
                 val allDocsVectors: Map[String, DocVector],
                 codeSet: Set[String],
                 trainStream: Stream[XMLDocument],
                 testStream: Stream[XMLDocument]
                ) {
>>>>>>> 1ab1bb908b3b95297b4a6b79a2e3a7805e0bc0e2

  // only for showing progress **** TODO remove later ?
  val codeSize = codeSet.size
  var i = 0
  val watch = new StopWatch()
  // *** END


<<<<<<< HEAD
  // for each code calc conditional probability
  val condProbPerCode = codeSet.map(code => {

    watch.start

    val (docsWithCode, docsWithoutCode) = stream.partition(_.codes(code))
=======
  //val stest = Set("I33020", "GCRIM", "THAIL")

  // for each code calc conditional probability
  val condProbPerCode = codeSet.foreach(code => {

    watch.start

    val (docsWithCode, docsWithoutCode) = trainStream.partition(_.codes(code))
>>>>>>> 1ab1bb908b3b95297b4a6b79a2e3a7805e0bc0e2

    val (codePriorWithCode, probMapWithCode) = calculateConditionalProbability(docsWithCode)

    val (codePriorWithoutCode, probMapWithoutCode) = calculateConditionalProbability(docsWithoutCode)

    watch.stop

<<<<<<< HEAD
    i = getProgress(i, codeSize)

    code -> ((codePriorWithCode, probMapWithCode), (codePriorWithoutCode, probMapWithoutCode))
  })

  def calculateConditionalProbability(streamWithCode: Stream[XMLDocument]) = {

    val docsSet = streamWithCode.map(doc => doc.name).toSet
=======
    i = getProgress(i, codeSize, code)

   // code -> ((codePriorWithCode, probMapWithCode), (codePriorWithoutCode, probMapWithoutCode))
  })

  def calculateConditionalProbability(oneClassStream: Stream[XMLDocument]) = {

    val docsSet = oneClassStream.map(doc => doc.name).toSet
>>>>>>> 1ab1bb908b3b95297b4a6b79a2e3a7805e0bc0e2
    val codePrior = docsSet.size.toDouble / vocabSize

    val vocabWithCode = IRUtils.mergeVocab(allDocsVectors.filterKeys(docsSet))
    println(vocabWithCode.size)

<<<<<<< HEAD
    val normalization = vocabWithCode.values.par.sum + vocabSize

    val condProbMap = vocab.map(token => token -> (vocabWithCode.getOrElse(token, 0) + 1) / normalization.toDouble)
=======
    val normalization = (vocabWithCode.values.par.sum + vocabSize).toDouble

    val condProbMap = vocab.map(token => token -> (vocabWithCode.getOrElse(token, 0) + 1) / normalization)
>>>>>>> 1ab1bb908b3b95297b4a6b79a2e3a7805e0bc0e2

    (codePrior, condProbMap)
  }

  def predict(): Unit = {

  }

<<<<<<< HEAD
  private def getProgress(index: Int, len: Int): Int = {
    println("%.0f".format(i.toDouble * 100 / len) + "% done " + watch.stopped)
=======
  private def getProgress(index: Int, len: Int, code:String): Int = {
    println("%.0f".format(i.toDouble * 100 / len) + "% done, label = " + code + " " + watch.stopped)
>>>>>>> 1ab1bb908b3b95297b4a6b79a2e3a7805e0bc0e2
    i + 1
  }

}
