package ir.textclass.Classifiers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class TextClassifier() {

  def classify: mutable.Map[String, ListBuffer[String]]


  def readCodes {

  }

  def printResults {

  }


}
