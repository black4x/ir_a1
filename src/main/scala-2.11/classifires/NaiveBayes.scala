package classifires

import ch.ethz.dal.tinyir.io.ReutersRCVStream

class NaiveBayes(val codesMap: Map[String, String], val trainPath: String) extends {
  // parsing all training docs
  val stream = new ReutersRCVStream(trainPath).stream

  // extracting distinct vocab size
  val vocabSize = stream.flatMap(_.tokens).distinct.length

  // only for showing progress
  var i = 0
  val codeSize = codesMap.size

  // for each code calc conditional probability
  val condProbPerCode = codesMap.map(code => {
    val docsWithLabel = stream.filter(_.codes(code._1))
    val docsWithLabelMinus = stream.filterNot(_.codes(code._1))
    val codePrior = docsWithLabel.length / codeSize
    val codePriorMinus = docsWithLabelMinus.length / codeSize
    val tokensWithLabel = docsWithLabel.flatMap(_.tokens)
    val tokensWithLabelMinus = docsWithLabelMinus.flatMap(_.tokens)
    val normalization = tokensWithLabel.length.toDouble + vocabSize
    val normalizationMinus = tokensWithLabelMinus.length.toDouble + vocabSize
    // keep sparseness, no normalization here
    val vocabFrequencyMapWithLabel = tokensWithLabel.groupBy(identity).mapValues(l => (l.length + 1).toDouble)
    val vocabFrequencyMapWithLabelMinus = tokensWithLabelMinus.groupBy(identity).mapValues(l => (l.length + 1).toDouble)

    val probMap = vocabFrequencyMapWithLabel.keys
      .map(word => word -> (vocabFrequencyMapWithLabel.getOrElse(word, 1.0) / normalization)).toMap

    val probMapMinus = vocabFrequencyMapWithLabelMinus.keys
      .map(word => word -> (vocabFrequencyMapWithLabelMinus.getOrElse(word, 1.0) / normalizationMinus)).toMap


    i = getProgress(i, codeSize);

    code._1 -> ((codePrior, probMap), (codePriorMinus, probMapMinus))
  })

  def predic (): Unit ={

  }


  def getProgress(index: Int, len: Int): Int ={
    println("%.0f".format (i.toDouble*100/len))
    i + 1
  }

}
