package ir.textclass

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.util.StopWatch
import ir.textclass.Classifiers.{NaiveBayesClassifier, TextClassifier}

object TextCategorizationMain {

  def main(args: Array[String]): Unit = {



//    if (args.isEmpty) {
//      val path_train: String = "/Users/Ralph/Development/ETH/Information Retrieval/Project 1/zipTrain"
//      //val path_test: String =
//    }
//    else {
//      val path_train = args(0)
//      val Path_test = args(1)
//    }

    val myStopWatch = new StopWatch()

    val reuters = new ReutersRCVStream("/home/ajuodelis/eth/ir/project1/train")
    val test = new ReutersRCVStream("/home/ajuodelis/eth/ir/project1/test")
    val bayes = new NaiveBayesClassifier(reuters)
    bayes.prediction(test)



  }

}
