package classifires

import ch.ethz.dal.tinyir.io.ReutersRCVStream

class NaiveBayes(val codes: Map[String, String], val trainPath: String) extends {
  // parsing all training docs
  val stream = new ReutersRCVStream(trainPath).stream

  // extracting vocabulary
  val vocab = stream.flatMap(_.tokens).distinct

  val vocabSize = vocab.length
  // TODO for each Label! now is only category
  val docsWithLabel = stream.filter(_.codes(codes.last._1))
  //val labelPrior = docsWithLabel.length /
  val tokensWithLabel = docsWithLabel.flatMap(_.tokens)
  val normalization = tokensWithLabel.length.toDouble + vocabSize
  // keep sparseness, no normalization here
  val vocabFrequencyMapWithLabel = tokensWithLabel.groupBy(identity).mapValues(l => (l.length + 1).toDouble)



  val probMap = vocabFrequencyMapWithLabel.keys
    .map(word => word -> (vocabFrequencyMapWithLabel.getOrElse(word, 1.0) / normalization)).toMap


}
