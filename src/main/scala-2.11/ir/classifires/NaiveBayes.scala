package ir.classifires


import ch.ethz.dal.tinyir.processing.XMLDocument
import ir.IRUtils.DocVector

class NaiveBayes(val allVocabVectors: Map[String, DocVector], val allVocabSet: Set[String],
                 val codesMap: Map[String, String], val stream: Stream[XMLDocument]) {

  // distinct vocab size
  val vocabSize = allVocabSet.size

  // only for showing progress
  var i = 0
  val codeSize = codesMap.size

  // for each code calc conditional probability
  val condProbPerCode = codesMap.map(code => {
    val (docsWithCode, docsWithoutCode) = stream.partition(_.codes(code._1))

    val (codePriorWithCode, probMapWithCode) = calculateConditionalProbability(docsWithCode)
    val (codePriorWithoutCode, probMapWithoutCode) = calculateConditionalProbability(docsWithoutCode)

    i = getProgress(i, codeSize)

    code._1 -> ((codePriorWithCode, probMapWithCode), (codePriorWithoutCode, probMapWithoutCode))
  })

  def calculateConditionalProbability(stream: Stream[XMLDocument]): (Int, Map[String, Double]) ={
    val codePrior = stream.length / codeSize
    val tokens = stream.flatMap(_.tokens)
    val normalization = tokens.length.toDouble + vocabSize
    val vocabFrequencyMap = tokens.groupBy(identity).mapValues(l => (l.length + 1).toDouble)
    val probMap = vocabFrequencyMap.keys.map(word => word -> (vocabFrequencyMap.getOrElse(word, 1.0) / normalization)).toMap
    (codePrior, probMap)
  }

  def predict (): Unit ={

  }

  private def getProgress(index: Int, len: Int): Int ={
    println("%.0f".format (i.toDouble*100/len) + "% done")
    i + 1
  }

}
