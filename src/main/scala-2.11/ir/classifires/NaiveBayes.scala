package ir.classifires


import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch
import ir.IRUtils
import ir.IRUtils.DocVector

class NaiveBayes(val allVocabVectors: Map[String, DocVector], val allVocabSet: Set[String],
                 val codesSet: Set[String], val stream: Stream[XMLDocument]) {

  // distinct vocab size
  val vocabSize = allVocabSet.size

  val codeSize = codesSet.size

  // only for showing progress
  var i = 0
  val watch = new StopWatch()

  // for each code calc conditional probability
  val condProbPerCode = codesSet.map(code => {

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

  def calculateConditionalProbability(stream: Stream[XMLDocument]) = {
    val codePrior = stream.length / codeSize
    // getting only needed document vectors from given stream (stream contains only docs for a particular code)

    val l = stream.map(doc => allVocabVectors.getOrElse(doc.name, Map[String, Int]())).toList
    val docsVectors = IRUtils.mergeAllDocs(l)

    val normalization = IRUtils.sumDocVector(docsVectors) + vocabSize

    //val vocabFrequencyMap = tokens.groupBy(identity).mapValues(l => (l.length + 1).toDouble)

    val probMap = docsVectors.mapValues(sum => (sum + 1).toDouble / normalization)

    (codePrior, probMap)

  }

  def predict(): Unit = {

  }

  private def getProgress(index: Int, len: Int): Int = {
    println("%.0f".format(i.toDouble * 100 / len) + "% done " + watch.stopped)
    i + 1
  }

}
