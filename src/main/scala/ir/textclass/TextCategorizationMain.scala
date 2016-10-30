package ir.textclass

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.util.StopWatch
import ir.textclass.Classifiers.NaiveBayesClassifier


object TextCategorizationMain {

  def main(args: Array[String]): Unit = {


    // TODO read samples path from agrs
    //    if (args.isEmpty) {
    //      val path_train: String = "/Users/Ralph/Development/ETH/Information Retrieval/Project 1/zipTrain"
    //      //val path_test: String =
    //    }
    //    else {
    //      val path_train = args(0)
    //      val Path_test = args(1)
    //    }

    // TODO read all dictinct labes
    // but for now just an array


    val myStopWatch = new StopWatch()
    myStopWatch.start


    val reuters = new ReutersRCVStream("/home/ajuodelis/eth/ir/project1/train")
    //val test = new ReutersRCVStream("/home/ajuodelis/eth/ir/project1/test")
    val trainDoc = reuters.stream;

    val vocabSize = trainDoc.flatMap(_.tokens).distinct.length
    val n = trainDoc.length

    val labels = List("M13", "C21")

    var model = Map[String, NaiveBayesClassifier]()

    labels.foreach(label => {
      model += (label -> new NaiveBayesClassifier(trainDoc, label, vocabSize, n))
    })

    print(model)

    // todo: read test docs and call prediction method

    //    if( cpos >= cneg){
    //      //vr_labels_found += vr_label
    //      println("label found: " + vr_label)
    //    }
    //    // add all labels found for the current document to the map
    //    vr_result +=  (vr_doc_id -> vr_labels_found)
    //
    //    vr_result foreach {case (key, value) => {
    //      print (key + " ")
    //      value.foreach(label => print(label + " "))
    //    }
    //    }
    //    println(" ")
    //
    //

    myStopWatch.stop
    println("Runtime: " + myStopWatch.stopped)

  }

}
