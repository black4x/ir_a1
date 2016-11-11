package ir.classifires


import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch
import ir.IRUtils
import ir.IRUtils.DocVector

class NaiveBayes(val vocabSize: Int, val vocab: Set[String],
                 val allDocsVectors: Map[String, DocVector],
                 codeSet: Set[String],
                 stream: Stream[XMLDocument]) {

  // only for showing progress **** TODO remove later ?
  val codeSize = codeSet.size
  var i = 0
  val watch = new StopWatch()
  // *** END

  // for each code calc conditional probability
  val condProbPerCode = codeSet.map(code => {

    watch.start

    val (docsWithCode, docsWithoutCode) = stream.partition(_.codes(code))

    //val (codePriorWithCode, probMapWithCode) = calculateConditionalProbability(docsWithCode)

    //val (codePriorWithoutCode, probMapWithoutCode) =
    calculateConditionalProbability(docsWithoutCode)

    watch.stop

    i = getProgress(i, codeSize)

    //code._1 -> ((codePriorWithCode, probMapWithCo
    // de), (codePriorWithoutCode, probMapWithoutCode))
  })

  def calculateConditionalProbability(streamWithCode: Stream[XMLDocument]) = {
    val codePrior = streamWithCode.length.toDouble / vocabSize

    // mapping docsVectors with particular code
    val docsVectorsWithCode = streamWithCode.map(doc => allDocsVectors.getOrElse(doc.name, Map[String, Int]())).toList
    val normalization = IRUtils.totalCoordinateSum(docsVectorsWithCode) + vocabSize

    val probMap = vocab.map(token =>
      token -> IRUtils.coordinateSumByToken(token, docsVectorsWithCode).toDouble / normalization).toMap
    (codePrior, probMap)
  }

  def predict(): Unit = {

  }

  private def getProgress(index: Int, len: Int): Int = {
    println("%.0f".format(i.toDouble * 100 / len) + "% done " + watch.stopped)
    i + 1
  }

}
