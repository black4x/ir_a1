package ir.classifires

import ch.ethz.dal.tinyir.util.StopWatch

import scala.collection.Map


/* Creates a support vector machine classifier. Input a set of training data (x,y), where y must be 1 or -1
 * Output is a classifier, on which the prediction method can be called to predict the class of a new data point.
 * Default lambda performed best on the validation set. Default steps is a compromise between time and convergence
 *
 * If the debugging option is on,additional output is generated. Eg total errorfunction and the weight vector theta
 * every 1000 iterations.
 */
class SVM(val allTrainVectors: Map[String, Map[String, Int]], ally: Map[String, Int], lambda: Double = 1, steps: Int = 10000, debug: Boolean = false) extends Classifier {

  val allDocKeys = allTrainVectors.keys.toList
  val allDocKeysLength = allDocKeys.length
  val myStopWatch = new StopWatch()
  var random = scala.util.Random
  var theta = Map[String, Double]()

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

  if (debug) {
    myStopWatch.stop
    println("SVM: Time to create the:%s".format(myStopWatch.stopped))
    println("SVM: Size of Theta:%d".format(theta.size))
  }

  /*The errorfunction function  calculates the total error of the SupportVectorMachine.
   * This function is only used in the debugging mode and allows to observe the convergence of the
   * iteration.
   */
  def errorfunction() = {
    val er = allTrainVectors.map(x => margin(ally.getOrElse(x._1, -1) * scalarProduct(theta, x._2))).sum + lambda * scalarProduct1(theta, theta)
    er
  }

  /* margin calculates (1-x)+  */
  def margin(x: Double) = {
    var res = 0.0
    if (x - 1 > 0) {
      res = 0
    } else {
      res = 1.0 - x
    }
    res
  }

  /* prediction method on the classifier object. Input a vector. Output a number, where positive numbers
   * indicate that the input vector belongs to class 1, negative number to class -1
   */
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