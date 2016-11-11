package ir.classifires

import ch.ethz.dal.tinyir.util.StopWatch
import ir.Classifier

import scala.collection.Map


//y must be 1 or -1
class SVM(allTrainVectors: Map[String, Map[String, Int]], ally: Map[String, Int], lambda: Double = 1, steps: Int = 1000, debug: Boolean = false) extends Classifier {

  var random = scala.util.Random
  var theta = Map[String, Double]()
  val allDocKeys = allTrainVectors.keys.toList
  val allDocKeysLength = allDocKeys.length
  val myStopWatch = new StopWatch()

  if (debug) {
    myStopWatch.start
    println("Here SVM, lambda:%f, steps=%d".format(lambda, steps))
  }

  for (t <- 1 to steps) {

    val randomKey = allDocKeys(random.nextInt(allDocKeysLength))
    val docVector = allTrainVectors(randomKey)
    val y = ally.getOrElse(randomKey, -1)

    theta = updateStep(docVector, y, lambda, t)
    if (debug & t % 1000 == 0) {
      print("SVM t:%d,Error %f ".format(t, errorfunction))
      println(theta.toSeq.sortWith(_._2 > _._2).take(100))

    }
  }

  def margin(x: Double) = {
    var res = 0.0
    if (x - 1 > 0) {
      res = 0
    } else {
      res = 1.0 - x
    }
    res
  }

  def errorfunction() = {
    val er = allTrainVectors.map(x => margin(ally.getOrElse(x._1, -1) * scalarProduct(theta, x._2))).sum + lambda * scalarProduct1(theta, theta)
    er
  }

  if (debug) {
    myStopWatch.stop
    println("SVM: Time to create the:%s".format(myStopWatch.stopped))
    println("SVM: Size of Theta:%d".format(theta.size))
  }

  override def prediction(vector: Map[String, Int]): Double = {
    val s = scalarProduct(theta, vector)
    s
  }

  def updateStep(contentVector: Map[String, Int], y: Int, lambda: Double, step: Int): Map[String, Double] = {

    val margin = 1.0 - y.toDouble * scalarProduct(theta, contentVector)
    var thetaShrink = scalarTimesVector((1 - 1.0 / step.toDouble), theta.toMap)

    if (margin > 0) {
      thetaShrink = vectorPlusVector(thetaShrink, scalarTimesVector1((1.0 / (lambda * step)) * y, contentVector))
    }
    thetaShrink
  }
}
