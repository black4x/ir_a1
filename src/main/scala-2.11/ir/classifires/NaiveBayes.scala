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

//
  //
  //
  //val stest = Set("I33020", "GCRIM", "THAIL")

  // for each code calc conditional probability
  val condProbPerCode = codeSet.map(code => {

    watch.start

    val (docsWithCode, docsWithoutCode) = stream.partition(_.codes(code))

    val (codePriorWithCode, probMapWithCode) = calculateConditionalProbability(docsWithCode)

    val (codePriorWithoutCode, probMapWithoutCode) = calculateConditionalProbability(docsWithoutCode)

    watch.stop

    i = getProgress(i, codeSize, code)

    code -> ((codePriorWithCode, probMapWithCode), (codePriorWithoutCode, probMapWithoutCode))
  })

  def calculateConditionalProbability(streamWithCode: Stream[XMLDocument]) = {

    val docsSet = streamWithCode.map(doc => doc.name).toSet
    val codePrior = docsSet.size.toDouble / vocabSize

    val vocabWithCode = IRUtils.mergeVocab(allDocsVectors.filterKeys(docsSet))
    println(vocabWithCode.size)

    val normalization = vocabWithCode.values.par.sum + vocabSize

    val condProbMap = vocab.map(token => token -> (vocabWithCode.getOrElse(token, 0) + 1) / normalization.toDouble)

    (codePrior, condProbMap)
  }

  def predict(): Unit = {

  }

  private def getProgress(index: Int, len: Int, code:String): Int = {
    println("%.0f".format(i.toDouble * 100 / len) + "% done for label " + code + " " + watch.stopped)
    i + 1
  }

}
