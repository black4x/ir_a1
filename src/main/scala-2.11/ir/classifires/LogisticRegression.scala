package ir.classifires


import ch.ethz.dal.tinyir.util.StopWatch

import scala.collection.Map


/* Creates a logistic regression classifier. Input a set of training data (x,y), where y must be 1 or -1
 * Output is a classifier, on which the prediction method can be called to predict the class of a new data point.
 * alphap and alpha m can be used to give more (or less) weight to the gradient calculation. If alphap is the ratio
 * of the positive class and alpha minus the ratio of the negative class, then the gradient of the positive training data
 * is weighted by alpham and vice versa. This effectively mimiks balanced data.
 *
 * If the debugging option is on,additional output is generated. Eg negative logliklelihood and the weight vector theta
 * every 1000 iterations. This allows to observe the convergence of the target function and of the weighted vector
 */
class LogisticRegression(allTrainVectors: Map[String, Map[String, Int]], ally: Map[String, Int], alphap: Double, alpham: Double, steps: Int = 1000, debug: Boolean = false) extends Classifier {
  val allDocKeys = allTrainVectors.keys.toList
  val allDocKeysLength = allDocKeys.length
  val myStopWatch = new StopWatch()
  var random = scala.util.Random
  var theta = collection.mutable.Map[String, Double]()
  var previousnegativll = 1000000.0

  if (debug) {
    myStopWatch.start
    println("Here LogisticRegressionClassifier1, alphap:%f, alpham=%f".format(alphap, alpham))
  }

  for (step <- 1 to steps) {

    val randomKey = allDocKeys(random.nextInt(allDocKeysLength))
    val docVector = allTrainVectors(randomKey)
    val y = ally.getOrElse(randomKey, -1)
    val dt = dtheta(docVector, y, 1 / math.sqrt(step)) //(1/math.sqrt(t),

    vectorPlusVector1(theta, dt)


    /* This is the code for the bold driver approach to the learning rate
     * Here the new learning rate is set to 1.1*old rate or 0.7*old rate depending on
     * if the negative Loglikelihood has decreased or increased. The negativ LogLikelihood of the data is
     * returned by the function negativll.
     * It turned out, that a learning rate of 1/sqrt(step) worked equally well.
     * Both approaches were better then a learning rate of 1/step.
     *
    if (step%200==0){
      val currentnegativll=negativll(allTrainVectors,ally)
      if (currentnegativll<=previousnegativll) {
        c=1.1*c
      } else {
        c=0.7*c
      }
      previousnegativll=currentnegativll
    }
    */
    if (debug & step % 1000 == 0) {
      //print("LogisticRegressionClassifier1 t:%d".format(t))
      print("t:%d negll:%.6f sump:%.6f".format(step, negativll(allTrainVectors, ally), sump(allTrainVectors, ally)))
      println(theta.toSeq.sortWith(_._2 > _._2).take(100))
      println("LogisticRegressionClassifier1: Size of Theta:%d".format(theta.size))
    }
  }

  if (debug) {
    myStopWatch.stop
    println("LogisticRegressionClassifier1: Time to create the:%s".format(myStopWatch.stopped))

  }

  //The function below calculates the negativ LogLikelihood of the data. Used vor observing the convergence
  def negativll(allTrainVectors: Map[String, Map[String, Int]], ally: Map[String, Int]) = {

    val s1 = ally.map(x => (x._1, math.log(logistic(allTrainVectors(x._1), theta))))
    val unwanted = ally.keys.toSet
    val k = allTrainVectors.keys.filterNot(unwanted)
    val s2 = k.map(x => (x, math.log(1 - logistic(allTrainVectors(x), theta)))).toMap
    val negll = -s1.values.sum - s2.values.sum
    negll
  }

  def sump(allTrainVectors: Map[String, Map[String, Int]], ally: Map[String, Int]) = {
    val s1 = ally.map(x => (x._1, logistic(allTrainVectors(x._1), theta)))

    val unwanted = ally.keys.toSet
    val k = allTrainVectors.keys.filterNot(unwanted)
    val s2 = k.map(x => (x, 1 - logistic(allTrainVectors(x), theta))).toMap

    val negll = -s1.values.sum - s2.values.sum
    negll

  }

  def dtheta(v: Map[String, Int], y: Int, learningrate: Double): Map[String, Double] = {
    var p = 0.0
    var l = 0.0

    p = logistic(v, theta)
    if (y == 1) {
      l = learningrate * (1 - p) * alpham
    } else {
      l = -learningrate * p * alphap
    }
    scalarTimesVector1(l, v)
  }

  def logistic(v: Map[String, Int], th: Map[String, Double]): Double = 1.0 / (1 + math.exp(-scalarProduct(th, v)))

  override def prediction(vector: Map[String, Int]): Double = {
    val s = logistic(vector, theta)
    s - 0.5
  }


}