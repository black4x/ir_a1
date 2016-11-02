package ir.textclass.io

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.util.StopWatch

object TestRead extends App {

  // Type for vocabulary: Map[ distinct_word -> occurrences_number]
  type Vocab = Map[String, Int]

  // adding operations to Vocab Type
  implicit class VocabOperations (x: Vocab) {
    // summing all values in Map
    def sumOccurrences() = x.foldLeft(0)(_ + _._2)
  }

  def createVocabFromDoc(doc: XMLDocument): Vocab = Tokenizer.tokenize(doc.content).groupBy(identity).mapValues(_.size)

  val reuters = new ReutersRCVStream("/home/ajuodelis/eth/ir/data_mini/train")
  println("# of files in zip = " + reuters.length)

  val watch = new StopWatch()
  watch.start

  val docVocabs = reuters.stream.map(doc => doc.name -> createVocabFromDoc(doc)).toMap

  val totalTokens = docVocabs.foldLeft(0)(_ + _._2.sumOccurrences)

  watch.stop
  // real:
  //  9216727
  //  178069


  //val vocabSize = reuters.stream.flatMap(_.tokens).distinct.length
  //val count_tokens = reuters.stream.flatMap(_.tokens).length
  println(watch.stopped)

  //  println(tokensMap.last)

  //println("# of docs im map = " + tokensMap.size)
  //println("# all tokens = " + allTokensNumber)
  //println("# distinct tokens = " + distinctMap.size)

  // for mini
  //# all tokens = 1244
  //# distinct tokens = 602

  //val ms = List(Map("hello" -> 1.1, "world" -> 2.2), Map("goodbye" -> 3.3, "hello" -> 4.4))
  //val mm = mergeMap(ms)((v1, v2) => v1 + v2)
}

