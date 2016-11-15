package ir.classifires

import scala.collection.Map


abstract class Classifier {
  def prediction(vector: Map[String, Int]): Double


  def scalarTimesVector(a: Double, b: Map[String, Double]): Map[String, Double] = {
    b.mapValues($1 => a * $1)
  }

  def scalarTimesVector1(a: Double, b: Map[String, Int]): Map[String, Double] = {
    b.mapValues($1 => a * $1)
  }

  def scalarProduct(a: Map[String, Double], b: Map[String, Int]): Double = {
    b.map(x => x._2 * a.getOrElse(x._1, 0.0)).sum
  }

  def scalarProduct1(a: Map[String, Double], b: Map[String, Double]): Double = {
    b.map(x => x._2 * a.getOrElse(x._1, 0.0)).sum
  }

  def vectorPlusVector(a: Map[String, Double], b: Map[String, Double]): Map[String, Double] = {
    var res = scala.collection.mutable.Map[String, Double]() ++= a
    b.foreach(x => res.put(x._1, x._2 + a.getOrElse(x._1, 0.0)))
    res
  }

  def vectorPlusVector1(a: collection.mutable.Map[String, Double], b: Map[String, Double]): Unit = {
    b.foreach(x => a.put(x._1, x._2 + a.getOrElse(x._1, 0.0)))

  }
}
