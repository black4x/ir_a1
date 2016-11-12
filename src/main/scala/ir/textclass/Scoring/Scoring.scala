package ir.textclass.Scoring

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument

import scala.collection.Map
import scala.collection.mutable.ListBuffer

/**
  * Created by Ralph on 06/11/16.
  */
class Scoring(val reuters_validate:ReutersRCVStream, val result_classifier: Map[String, ListBuffer[String]] ) {


  def calculateF1(): Double = {

    // Example:
    // Prediction assigned labels A,B,C to Doc 1. Doc 1 however contains the actual labels A,D,E,F
    // So Precision is 1/3 and Recall is 1/4. Only label A was correctly determined by the prediction.

    //Precision per Document = "Number of labels predicted that are correct" divided by "Total number of labels predicted"
    //Recall per Document = "Number of labels found that are correct" divided by "Total Number of labels in the validation doc"

    val nr_of_docs = reuters_validate.stream.size.toDouble
    val F1_total = reuters_validate.stream.map(doc => calculateF1PerDoc(doc)).reduce(_ + _)  / nr_of_docs
    return F1_total

  }

  private def calculateF1PerDoc(vali_doc: XMLDocument): Double = {

    val labels_correct = vali_doc.codes
    println("codes of validation doc " + labels_correct + " " + labels_correct.size)

    // Just in case we check if there is a result for validation document
    if(result_classifier.exists(_._1 == vali_doc.name) == false){
      println("No result found for Doc: " + vali_doc.name)
      return 0.0
    }

    val labels_predicted = result_classifier(vali_doc.name)
    val nr_labels_predicted = labels_predicted.size.toDouble

    if (nr_labels_predicted == 0) return 0.0

    // Count how many predicted labels are actually in the validation document
    var nr_labels_classified_correct = 0.0
    labels_predicted.foreach(label_predicted => {
      if (labels_correct.exists(label => label == label_predicted)) {
        nr_labels_classified_correct += 1
      }
    })

    println("correct prediction: " + nr_labels_classified_correct)
    println("total labels in doc: " + labels_correct.size)

    val precision = nr_labels_classified_correct / nr_labels_predicted
    val recall = nr_labels_classified_correct / labels_correct.size

    println("per and recall " + precision + " " + recall)
    if(precision + recall == 0){ return 0.0 }
    else {
      return (2 * precision * recall) / (precision + recall)
    }

  }


}

// to test it use this code in a main method
/*var result2 = Map[String, List[String]]()  // Doc ID + List of Labels
var labels_found2 = ListBuffer[String]()

labels_found2 += "UK"
labels_found2 += "E121"
labels_found2 += "M11"
result2 +=  ("2380" -> labels_found2.toList)

// F1 Scoring for validation docs
println("start of F1 Scoring")
val score = new Scoring(stream_validate, result2)
val f1Score = score.calculateF1()
println("The F1 Score is: " + f1Score)
*/
