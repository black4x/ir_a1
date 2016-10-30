package ir.textclass.Classifiers

import breeze.linalg.max
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NaiveBayesClassifier(val reuters_train:ReutersRCVStream, val code:String, val vocabSize:Long, val n:Long)  {

  //var length: Long = 0
  //var tokens: List[String] = List()

  //var npos: Integer = 0;
  //val xmldocs = trainstream.stream
  val xmldocspos = reuters_train.stream.filter(_.codes(code))
  val xmldocsneg = reuters_train.stream.filter(!_.codes(code))

  println ("nr of docs in class: "+ xmldocspos.length)
  println ("nr of docs not in class: "+ xmldocsneg.length)

  /*Calculate p(c) for the two classes and store it in a map (perhaps there are better structures)*/
  //var pc = Map("cpos" -> 1.0, "cneg" -> 0.0)
  var pc = Map[String, Double]()
  var npos = xmldocspos.length
  pc = pc + ("cpos" -> npos.toDouble / n)
  pc = pc + ("cneg" -> (1 - npos.toDouble / n))
  //print(pc)

  /* Calculate and store the p(w|c) for the two classes and store the values in pwcpos and pwcnegwith alpha=1 smoothing
  * pwcpos map contains only tokens of the poitivedocuments */

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

  /*Returns the two class probabilities. The bigger one should be picked if only interested in the class*/
  def prediction(xmlDoc: XMLDocument): ((String, Double), (String, Double)) = {


    var cpos = scala.math.log(pc("cpos"))
    var cneg = scala.math.log(pc("cneg"))
    //print(xmlDoc.content)
    /*Calculate likelihood of d for pos */

    //cpos = Tokenizer.tokenize(xmlDoc.content).map(tkn => scala.math.log(pwcpos.getOrElse(tkn, 1) / sumlengthdpos)).sum
    //cpos = Tokenizer.tokenize(xmlDoc.content).map(tkn => scala.math.log(pwcneg.getOrElse(tkn, 1) / sumlengthdneg)).sum

    //(("cpos", cpos), ("cneg", cneg))
    (("cpos", 0.9), ("cneg", 0.1)) //todo: this is just a dummy to be removed later
    //max(cpos, cneg)



  }

}