package ir.textclass.Scoring

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.Map

/**
  * Created by Ralph on 06/11/16.
  */
class Scoring(val reuters_validate:ReutersRCVStream, val result_classifier: Map[String, List[String]] ) {


  def calculateF1(): Double = {

    // Example:
    // Prediction assigned labels A,B,C to Doc 1. Doc 1 however contains the actual labels A,D,E,F
    // So Percision is 1/3 and Recall is 1/4. Only label A was correctly determined by the prediction.

    //Percision per Document = "Number of labels predicted that are correct" divided by "Total number of labels predicted"
    //Recall per Document = "Number of labels found that are correct" divided by "Total Number of labels in the validation doc"

    val nr_of_docs = reuters_validate.stream.size.toDouble
    val F1_total = reuters_validate.stream.map(doc => calculateF1PerDoc(doc)).reduce(_ + _)  / nr_of_docs
    return F1_total

  }

  private def calculateF1PerDoc(vali_doc: XMLDocument): Double = {

    val labels_correct = vali_doc.codes
    println("codes of validation doc " + labels_correct + " " + labels_correct.size)
    println("F1 for doc" + vali_doc.name)
    val labels_predicted = result_classifier(vali_doc.name)
    println("labels predicted" + labels_predicted.size)
    val nr_labels_predicted = labels_predicted.size
    if (nr_labels_predicted == 0) {
      println("no labels" + nr_labels_predicted)
      return 0.0
    }
    else {

      // Count how many predicted labels are actually in the validation document
      var nr_labels_classified_correct = 0
      labels_predicted.foreach(label_predicted => {
        if (labels_correct.exists(label => label == label_predicted)) {
          nr_labels_classified_correct += 1
        }
      })

      println("correct prediction: " + nr_labels_classified_correct)

      val percision = nr_labels_classified_correct / nr_labels_predicted
      val recall = nr_labels_classified_correct / labels_correct.size

      println("per and recall " + percision + " " + recall)
      if(percision + recall == 0){
        return 0.0
      }
      else {
        return (2 * percision * recall) / (percision + recall)
      }
    }

  }



  }

