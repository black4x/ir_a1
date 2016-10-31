package ir.textclass

import ch.ethz.dal.tinyir.io.{DirStream, ReutersRCVStream}
import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.util.StopWatch
import ir.textclass.Classifiers.NaiveBayesClassifier

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


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


    val reuters_train = new ReutersRCVStream("/home/ajuodelis/eth/ir/project1/train")
    //val test = new ReutersRCVStream("/home/ajuodelis/eth/ir/project1/test")

    // Comment Ralph:
    // Cannot store the stream in trainDoc because the next statement for VocaSize will crash!!!
    //val trainDoc = reuters.stream; // DO NOT DO THIS

    // Comment Ralph: This statement takes 50 seconds to run but way better than using trainDoc.flatMap...
    val vocabSize = reuters_train.stream.flatMap(_.tokens).distinct.length
    val n = reuters_train.length.toDouble
    // So we only have to count the positive tokens and can use this number to calculate the negative tokens
    val count_tokens = reuters_train.stream.flatMap(_.tokens).length.toDouble

    println("Vacabulary Size: " + vocabSize)
    println("Nr of Docs: " + n)


    val labels = List("M13", "C21")

    var model = Map[String, NaiveBayesClassifier]()

    labels.foreach(label => {
      model += (label -> new NaiveBayesClassifier(reuters_train, label, vocabSize, n, count_tokens))
    })

    println(model)

    myStopWatch.stop
    println("Training done : " + myStopWatch.stopped)

    println("Start of Test")
    myStopWatch.start

    // TODO: For tests only!: without Zip file in order to run it for just a small number of files
    // TODO: Replace later with the Test Doc Zip Folder. If you test it change the path to your folder!
    val path_validate : String = "/Users/Ralph/Development/ETH/Information Retrieval/Project 1/validateSmall"
    val stream_validate = new DirStream (path_validate, ".xml")
    println("Number of files in directory = " + stream_validate.length)

    var result = mutable.Map[String, ListBuffer[String]]()  // Doc ID + List of Labels
    var labels_found = ListBuffer[String]()

    // For each test document call the prediction method previously trained for each label
    for (doc_validate <- stream_validate.stream.map(inputStream => new ReutersRCVParse(inputStream))) {

      println("classifying document: " + doc_validate.title)
      model.foreach(label => {

        if (label._2.prediction(doc_validate) == true){
          labels_found += label._1 // first column of Map is the label/code
          println("label found: " + label._1)
        }

      })

      // Add current document to the result with all labels predicted
      result +=  (doc_validate.title -> labels_found)
      labels_found.remove(0, labels_found.length) //initialize list before next loop
    }

    // Todo: write into file
    // Print results
    result foreach {case (key, value) => {
      print (key + " ")
      value.foreach(label => print(label + " "))
    }
      println(" ") // seprate labels per document by space
    }

    myStopWatch.stop
    println("Test done: " + myStopWatch.stopped)



  }

}
