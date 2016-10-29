package ir.textclass.Classifiers

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NaiveBayesClassifier(val trainstream: ReutersRCVStream, val code: String = "C21") extends TextClassifier {

  var length: Long = 0
  var tokens: List[String] = List()
  var n: Integer = 0;
  var npos: Integer = 0;
  val xmldocs = trainstream.stream
  val xmldocspos = xmldocs.filter(_.codes(code))
  val xmldocsneg = xmldocs.filter(!_.codes(code))

  /*Calculate p(c) for the two classes and store it in a map (perhaps there are better structures)*/
  var pc = Map("cpos" -> 1.0, "cneg" -> 0.0)
  n = trainstream.length
  npos = xmldocspos.length
  pc = pc + ("cpos" -> npos.toFloat / n)
  pc = pc + ("cneg" -> (1 - npos.toFloat / n))
  print(pc)

  /* Calculate and store the p(w|c) for the two classes and store the values in pwcpos and pwcnegwith alpha=1 smoothing
  * pwcpos map contains only tokens of the poitivedocuments
  *
   */
  val vocab = xmldocs.flatMap(_.tokens).distinct
  val vocabSize = xmldocs.flatMap(_.tokens).distinct.length

  //denominator
  val sumlengthdpos = xmldocspos.flatMap(_.tokens).length.toDouble + vocabSize
  val sumlengthdneg = xmldocsneg.flatMap(_.tokens).length.toDouble + vocabSize

  // nominator
  val pwcpos=xmldocspos.flatMap(_.tokens).groupBy(identity).mapValues(_.size + 1)
  val pwcneg = xmldocsneg.flatMap(_.tokens).groupBy(identity).mapValues(_.size + 1)


  println(pwcpos)
  println(pwcneg.map(a => a._2).sum)

  /*Returns the two class probabilities. The bigger one should be picked if only interested in the class*/
  def prediction(xmlDoc: XMLDocument): ((String, Double), (String, Double)) = {


    var cpos = scala.math.log(pc("cpos"))
    var cneg = scala.math.log(pc("cneg"))
    print(xmlDoc.content)
    /*Calculate likelihood of d for pos */

    cpos = Tokenizer.tokenize(xmlDoc.content).map(tkn => scala.math.log(pwcpos.getOrElse(tkn, 1) / sumlengthdpos)).sum
    cpos = Tokenizer.tokenize(xmlDoc.content).map(tkn => scala.math.log(pwcneg.getOrElse(tkn, 1) / sumlengthdneg)).sum

    (("cpos", cpos), ("cneg", cneg))


//    if( cpos >= cneg){
//      //vr_labels_found += vr_label
//      println("label found: " + vr_label)
//    }
//    // add all labels found for the current document to the map
//    vr_result +=  (vr_doc_id -> vr_labels_found)
//
//    vr_result foreach {case (key, value) => {
//      print (key + " ")
//      value.foreach(label => print(label + " "))
//    }
//    }
//    println(" ")
//
//

  }


  override def classify: mutable.Map[String, ListBuffer[String]] = {

    //todo copy coding for naive classifier
    val path: String = "/Users/Ralph/Development/ETH/Information Retrieval/Project 1/zipTrain"
    val reuters = new ReutersRCVStream(path)


    mutable.Map[String, ListBuffer[String]]()

  }

}