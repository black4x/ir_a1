package classifires

import ch.ethz.dal.tinyir.io.ReutersRCVStream

class NaiveBayes(val codesMap: Map[String, String], val trainPath: String) extends {
  // parsing all training docs
  val stream = new ReutersRCVStream(trainPath).stream

  // extracting vocabulary
  val vocab = stream.flatMap(_.tokens).distinct

  val vocabSize = vocab.length

  // only for showing progress
  var i = 0
  val l = codesMap.size

  // for each code calc conditional probability
  val condProbPerCode = codesMap.map(code => {
    val docsWithLabel = stream.filter(_.codes(code._1))
    //val labelPrior = docsWithLabel.length /
    val tokensWithLabel = docsWithLabel.flatMap(_.tokens)
    val normalization = tokensWithLabel.length.toDouble + vocabSize
    // keep sparseness, no normalization here
    val vocabFrequencyMapWithLabel = tokensWithLabel.groupBy(identity).mapValues(l => (l.length + 1).toDouble)

    val probMap = vocabFrequencyMapWithLabel.keys
      .map(word => word -> (vocabFrequencyMapWithLabel.getOrElse(word, 1.0) / normalization)).toMap

    i = getProgress(i, l);

    code._1 -> probMap
  })

  println(condProbPerCode)

  def getProgress(index: Int, len: Int): Int ={
    println("%.0f".format (i.toDouble*100/len))
    i + 1
  }

}
