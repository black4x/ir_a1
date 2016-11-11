package ir.classifires


import ch.ethz.dal.tinyir.util.StopWatch
import ir.Classifier

import scala.collection.Map


class LogisticRegressionClassifier1(allTrainVectors: Map[String, Map[String, Int]], ally: Map[String, Int], alphap: Double, alpham: Double, steps: Int = 1000, debug: Boolean = false) extends Classifier {
  var random = scala.util.Random

  var theta = collection.mutable.Map[String, Double]()
  //var theta=Map[String,Double]()
  val allDocKeys = allTrainVectors.keys.toList
  val allDocKeysLength = allDocKeys.length
  val myStopWatch = new StopWatch()
  var previousnegativll = 1000000.0
  //var c=0.1

  if (debug) {
    myStopWatch.start
    println("Here LogisticRegressionClassifier1, alphap:%f, alpham=%f".format(alphap, alpham))
  }

  for (step <- 1 to steps) {

    val randomKey = allDocKeys(random.nextInt(allDocKeysLength))
    val docVector = allTrainVectors(randomKey)
    val y = ally.getOrElse(randomKey, -1)
    val dt = dtheta(docVector, y, 1 / math.sqrt(step)) //(1/math.sqrt(t),

    //theta=vectorPlusVector(theta,dt)
    vectorPlusVector1(theta, dt)

    /*
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

  override def prediction(vector: Map[String, Int]): Double = {
    val s = logistic(vector, theta)
    s - 0.5
  }

  def logistic(v: Map[String, Int], th: Map[String, Double]): Double = 1.0 / (1 + math.exp(-scalarProduct(th, v)))


}
