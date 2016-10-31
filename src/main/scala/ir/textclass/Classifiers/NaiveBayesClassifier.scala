package ir.textclass.Classifiers

import breeze.linalg.max
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}
import ch.ethz.dal.tinyir.util.StopWatch

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NaiveBayesClassifier(val reuters_train:ReutersRCVStream, val code:String, val vocabSize:Double, val n:Double, val count_tokens:Double)  {

  val myStopWatch = new StopWatch()

  // Comment Ralph: I changed this as the previous version crashed!
  /*Calculate p(c) for the two classes and store it in a map*/
  var pc = Map[String, Double]()
  val prior_pos = reuters_train.stream.filter(_.codes(code)).length.toDouble / n
  val prior_neg = 1 - prior_pos
  pc = pc + ("cpos" -> prior_pos)
  pc = pc + ("cneg" -> prior_neg)
  println(pc)


  /* Calculate and store the p(w|c) for the two classes and store the values in pwcpos and pwcnegwith alpha=1 smoothing
     pwcpos map contains only tokens of the poitive documents */

  val tks_pos = reuters_train.stream.filter(_.codes(code)).flatMap(_.tokens) // this is fast
  val tks_neg = reuters_train.stream.filter(!_.codes(code)).flatMap(_.tokens) // this is fast

  // Denominator
  myStopWatch.start
  val count_tokens_pos = tks_pos.length.toDouble
  val sumlengthdpos: Double = count_tokens_pos + vocabSize
  val sumlengthdneg: Double = (count_tokens - count_tokens_pos  ) + vocabSize
  println("denominator pos: " + sumlengthdpos + " denominator neg: " + sumlengthdneg )
  myStopWatch.stop
  println("Denominator calc done: " + myStopWatch.stopped)


  // Nominator (TODO: activated pwcneg once performance fixed
  myStopWatch.start
  val pwcpos = tks_pos.groupBy(identity).mapValues(l=>(l.length+1))
  //val pwcneg = tks_neg.groupBy(identity).mapValues(l=>(l.length+1)) // this crashes memory
  myStopWatch.stop
  println("Nominator calc done: " + myStopWatch.stopped)

  println("the size of pwcpos is :" + pwcpos.size)



  /*Returns the two class probabilities. The bigger one should be picked if only interested in the class*/
  def prediction(doc_to_classify: XMLDocument): Boolean = {

    var cpos = scala.math.log(pc("cpos"))
    var cneg = scala.math.log(pc("cneg"))

    // Calculate positive probability of document being in class.
    var cdpos: Double = cpos + Tokenizer.tokenize(doc_to_classify.content).map(tkn => scala.math.log(pwcpos.getOrElse(tkn, 1) / sumlengthdpos)).sum

    // Calcualte negative probability
    // todo: For the negative probabily it is calcualted on-the-fly and not read from the training data
    // todo: the reasons is that with so many negative tokens it could not be stored without memory overflow.
    var cdneg: Double = Tokenizer.tokenize(doc_to_classify.content).map(tkn => train_negative_onthefly(tkn)).reduce(_ + _)
    // var cdneg: Double = cneg + Tokenizer.tokenize(doc_to_classify.content).map(tkn => scala.math.log(pwcneg.getOrElse(tkn, 1) / sumlengthdneg)).sum

    println("cdpos: " + cdpos + " cdneg: " + cdneg)
    if (cdpos >= cdneg) true else false

  }

  def train_negative_onthefly(tkn: String): Double = {
    math.log(( tks_neg.count(_ == tkn) + 1 ) / sumlengthdneg)
  }


}




//var length: Long = 0
//var tokens: List[String] = List()

//var npos: Integer = 0;
//val xmldocs = trainstream.stream
//val xmldocspos = reuters_train.stream.filter(_.codes(code))
//val xmldocsneg = reuters_train.stream.filter(!_.codes(code))

//println ("nr of docs in class: "+ xmldocspos.length)
//println ("nr of docs not in class: "+ xmldocsneg.length)

/* RALPH: THIS CRASHES MEMORY, so commented out for now
  //denominator
  val sumlengthdpos = xmldocspos.flatMap(_.tokens).length.toDouble + vocabSize
  val sumlengthdneg = xmldocsneg.flatMap(_.tokens).length.toDouble + vocabSize
  // nominator
  val pwcpos=xmldocspos.flatMap(_.tokens).groupBy(identity).mapValues(_.size + 1)
  val pwcneg = xmldocsneg.flatMap(_.tokens).groupBy(identity).mapValues(_.size + 1)

  println("the size of pwcpos is :" +pwcpos.size)
*/
// println(pwcpos)
// println(pwcneg.values.sum)