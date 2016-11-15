package ir.classifires


import java.io.{File, PrintWriter}

import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch
import ir.utils.IRUtils
import ir.utils.IRUtils.DocVector

import scala.collection.mutable.ListBuffer
import scala.io.Source


object NaiveBayes {

  def predict(vocabSize: Int, vocab: Set[String],
              allDocsVectors: Map[String, DocVector],
              codeSet: Set[String],
              trainStream: Stream[XMLDocument],
              testStream: Stream[XMLDocument]) {

    // only for showing progress **** TODO remove later ?
    val codeSize = codeSet.size
    var i = 0
    val watch = new StopWatch()
    // *** END

    val out = new PrintWriter(new File("nb-temp.txt"))

    // for each code calc conditional probability
    codeSet.foreach(code => {

      watch.start

      val (docsCodePos, docsCodeNeg) = trainStream.partition(_.codes(code))

      val (posPrior, posCondProbMap) = calculateConditionalProbability(vocabSize, vocab, allDocsVectors, docsCodePos)
      val (negPrior, negCondProbMap) = calculateConditionalProbability(vocabSize, vocab, allDocsVectors, docsCodeNeg)

      out.write(code + "\t")
      testStream.foreach(testDoc => {
        val probWithCode = calcCondProb(testDoc, posCondProbMap) + posPrior
        val probWithoutCode = calcCondProb(testDoc, negCondProbMap) + negPrior
        if (probWithCode > probWithoutCode) {
          out.write(testDoc.name + "\t")
        }
      })
      out.write("\n")

      watch.stop
      println("%.0f".format(i.toDouble * 100 / codeSize) + "% done, label = " + code + " " + watch.stopped)
      i = i + 1
    })

    out.close()
  }

  def collectResults(resultsMap: scala.collection.mutable.Map[String, ListBuffer[String]]): Unit = {
    Source.fromFile("nb-temp.txt").getLines().foreach(line => {
      val all = line.split("\t")
      val code = all.head
      all.tail.foreach(docName => {
        IRUtils.addCodeToResultMap(resultsMap, docName, code)
      })
    })
    resultsMap
  }

  private def calculateConditionalProbability(vocabSize: Int, vocab: Set[String],
                                              allDocsVectors: Map[String, DocVector],
                                              oneClassStream: Stream[XMLDocument]): (Double, Map[String, Double]) = {

    val docsSet = oneClassStream.map(doc => doc.name).toSet
    val codePriorLog = scala.math.log(docsSet.size.toDouble / vocabSize)

    val vocabWithCode = IRUtils.mergeVocab(allDocsVectors.filterKeys(docsSet))
    println(vocabWithCode.size)

    val normalization = (vocabWithCode.values.par.sum + vocabSize).toDouble

    val condProbLogMap = vocab.map(token => token -> scala.math.log((vocabWithCode.getOrElse(token, 0) + 1) / normalization)).toMap

    (codePriorLog, condProbLogMap)
  }


  private def calcCondProb(doc: XMLDocument, condProb: Map[String, Double]): Double =
    doc.tokens.map(token => condProb.getOrElse(token, 0.0)).sum

}
