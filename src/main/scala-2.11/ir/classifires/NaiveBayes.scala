package ir.classifires


import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch
import ir.utils.IRUtils
import ir.utils.IRUtils.DocVector

import scala.collection.mutable.ListBuffer


class NaiveBayes(val vocabSize: Int, val vocab: Set[String],
                 val allDocsVectors: Map[String, DocVector],
                 codeSet: Set[String],
                 trainStream: Stream[XMLDocument],
                 testStream: Stream[XMLDocument]) {

  var result = Map[String, ListBuffer[String]]()
  // for each code calc conditional probability
  codeSet.foreach(code => {


    val (docsWithCode, docsWithoutCode) = trainStream.partition(_.codes(code))

     val (priorPos, condProbPos) = calculateConditionalProbability(docsWithCode)
     val (priorNeg, condProbNeg) = calculateConditionalProbability(docsWithoutCode)

    testStream.foreach(testDoc => {
      val probWithCode = calcCondProb(testDoc, condProbPos) + priorPos
      val probWithoutCode = calcCondProb(testDoc, condProbNeg) +priorNeg
      if (probWithCode > probWithoutCode) {
        if (result.contains(testDoc.name)) result.getOrElse(testDoc.name, ListBuffer[String]()) += code
        else result += (testDoc.name -> ListBuffer[String](code))
      }
    })

  })

  def calculateConditionalProbability(oneClassStream: Stream[XMLDocument]): (Double, Map[String, Double]) = {

    val docsSet = oneClassStream.map(doc => doc.name).toSet
    val codePriorLog = scala.math.log(docsSet.size.toDouble / vocabSize)

    val vocabWithCode = IRUtils.mergeVocab(allDocsVectors.filterKeys(docsSet))
    println(vocabWithCode.size)

    val normalization = (vocabWithCode.values.par.sum + vocabSize).toDouble

    val condProbLogMap = vocab.map(token => token -> scala.math.log((vocabWithCode.getOrElse(token, 0) + 1) / normalization)).toMap

    (codePriorLog, condProbLogMap)
  }

  def predict() = result

  private def calcCondProb(doc: XMLDocument, condProb: Map[String, Double]): Double =
    doc.tokens.map(token => condProb.getOrElse(token, 0.0)).sum

}
