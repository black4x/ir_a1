package ir.textclass.io

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.util.StopWatch

object TestRead extends App {


  val reuters = new ReutersRCVStream("/home/ajuodelis/eth/ir/data_mini/train")
  println("# of files in zip = " + reuters.length)

  val watch = new StopWatch()
  watch.start
  val tokensMap = reuters.stream.map(doc => {
    doc.name -> Tokenizer.tokenize(doc.content).groupBy(identity).mapValues(_.size)
  }).toMap

  val allTokensNumber = tokensMap.foldLeft(0)(_ + _._2.foldLeft(0)(_ + _._2))

  watch.stop
// real:
//  9216727
//  178069


  //val vocabSize = reuters.stream.flatMap(_.tokens).distinct.length
  //val count_tokens = reuters.stream.flatMap(_.tokens).length
  //println("took time = " + watch.stopped)

  println(tokensMap.last)

//  println("# of docs im map = " + tokensMap.size)
//  println("# all tokens = " + allTokensNumber)
//  //println("# distinct tokens = " + distinctTokensNumber)

}