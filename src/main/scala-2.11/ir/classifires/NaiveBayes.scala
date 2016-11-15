package ir.classifires


import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch
import ir.utils.IRUtils
import ir.utils.IRUtils.DocVector

import scala.collection.mutable.ListBuffer


class NaiveBayes(val vocabSize: Int, val vocab: Set[String],
                 val allDocsVectors: Map[String, DocVector],
                 codeSet: Set[String],
                 trainStream: Stream[XMLDocument]) {

  // only for showing progress **** TODO remove later ?
  val codeSize = codeSet.size
  var i = 0
  val watch = new StopWatch()
  // *** END

  // for each code calc conditional probability
  val condProbPerCode = codeSet.map(code => {

    watch.start

    val (docsWithCode, docsWithoutCode) = trainStream.partition(_.codes(code))

    val res = code -> (calculateConditionalProbability(docsWithCode), calculateConditionalProbability(docsWithoutCode))

    watch.stop
    i = getProgress(i, codeSize, code)

    res
  }).toMap

  def calculateConditionalProbability(oneClassStream: Stream[XMLDocument]): (Double, Map[String, Double]) = {

    val docsSet = oneClassStream.map(doc => doc.name).toSet
    val codePriorLog = scala.math.log(docsSet.size.toDouble / vocabSize)

    val vocabWithCode = IRUtils.mergeVocab(allDocsVectors.filterKeys(docsSet))
    println(vocabWithCode.size)

    val normalization = (vocabWithCode.values.par.sum + vocabSize).toDouble

    val condProbLogMap = vocab.map(token => token -> scala.math.log((vocabWithCode.getOrElse(token, 0) + 1) / normalization)).toMap

    (codePriorLog, condProbLogMap)
  }

  def predict(testStream: Stream[XMLDocument]) = {

    var result = Map[String, ListBuffer[String]]()

    codeSet.foreach(code => {

      val condProbTuple = condProbPerCode.get(code)
      condProbTuple match {
        case Some(_) => {
          val (withCode, withoutCode) = condProbTuple.get
          testStream.foreach(testDoc => {
            val probWithCode = calcCondProb(testDoc, withCode._2) + withCode._1
            val probWithoutCode = calcCondProb(testDoc, withoutCode._2) + withoutCode._1
            if (probWithCode > probWithoutCode) {
              if (result.contains(testDoc.name)) result.getOrElse(testDoc.name, ListBuffer[String]()) += code
              else result += (testDoc.name -> ListBuffer[String](code))
            }
          })
        }
      }
    })
    result
  }


  private def getProgress(index: Int, len: Int, code: String): Int = {
    println("%.0f".format(i.toDouble * 100 / len) + "% done, label = " + code + " " + watch.stopped)
    i + 1
  }

  private def calcCondProb(doc: XMLDocument, condProb: Map[String, Double]): Double =
    doc.tokens.map(token => condProb.getOrElse(token, 0.0)).sum

}
