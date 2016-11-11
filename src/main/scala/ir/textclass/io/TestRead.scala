package ir.textclass.io

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.util.StopWatch

object TestRead extends App {

  // Type for vocabulary: Map[ distinct_word -> occurrences_number]
  type Vocab = Map[String, Int]

  // adding operations to Vocab Type
  implicit class VocabOperations (vocab: Vocab) {
    // summing all values in Map
    def sumOccurrences() = vocab.foldLeft(0)(_ + _._2)
  }

  def mergeMap(ms: List[Vocab])(f: (Int, Int) => Int): Vocab =
    (Map[String, Int]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
      a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
    }

  def createVocabFromDoc(doc: XMLDocument): Vocab = StopWords.filter(Tokenizer.tokenize(doc.content)).groupBy(identity).mapValues(_.size)

  val reuters = new ReutersRCVStream("/home/ajuodelis/eth/ir/data_mini/train")
  println("# of files in zip = " + reuters.length)

  val watch = new StopWatch()
  watch.start

  val allDocsVocabs = reuters.stream.map(doc => doc.name -> createVocabFromDoc(doc)).toMap

  val totalTokens = allDocsVocabs.foldLeft(0)(_ + _._2.sumOccurrences)
  val allVocab = mergeMap(allDocsVocabs.values.toList)((v1, v2) => v1 + v2)

  watch.stop
  println(watch.stopped)

  // real:
  //  9216727 ~ 10 Mill
  //  178069 ~ 200 K

  // for mini
  //# all tokens = 1244
  //# distinct tokens = 602

  println("# of docs im map = " + allDocsVocabs.size)
  println("# all tokens = " + totalTokens)
  println("# distinct tokens = " + allVocab.size)

}