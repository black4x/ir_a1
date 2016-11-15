package ir.utils

import java.io.{File, InputStream}

import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.mutable.ListBuffer
import scala.io.Source._

object IRUtils {
  type DocVector = Map[String, Int]

  def totalCoordinateSum(docsVectorsList: List[DocVector]): Int =
    docsVectorsList.map(docVector => docVector.values.reduce(_ + _)).reduce(_ + _)

  def coordinateSumByToken(token: String, docsVectorsList: List[DocVector]): Int =
    docsVectorsList.map(docVector => getCoordinateWithLaplase(docVector.get(token))).reduce(_ + _)

  def getCoordinateWithLaplase(coordinate: Option[Int]): Int = coordinate match {
    case Some(i) => i + 1
    case None => 1
  }

  def getAllDocsVectors(stream: Stream[XMLDocument]): Map[String, DocVector] =
    stream.map(doc => doc.name -> getDocVector(doc)).toMap

  def getDocVector(doc: XMLDocument): DocVector =
    StopWords.filterOutSW(Tokenizer.tokenize(doc.content)) // filtering stop words out
      .map(token => PorterStemmer.stem(token)) // making all form of words the same
      .groupBy(identity) // grouping by distinct tokens
      .mapValues(_.size) // counting size

  def totalSumCoordinates(docsVector: Map[String, DocVector]): Int =
    docsVector.values.map(docVector => docVector.values.sum).sum

  def sumCoordinatesByToken(docsVector: Map[String, DocVector], token: String): Int =
    docsVector.values.map(docVector => docVector.getOrElse(token, 0)).reduce(_ + _)

  def getSetOfDistinctTokens(docsVectors: Map[String, DocVector]): Set[String] =
    docsVectors.values.map(docVector => docVector.keySet).reduce(_ ++ _)

  def getSetOfDistinctTokens(docsVectorsList: List[DocVector]): Set[String] =
    docsVectorsList.map(docVector => docVector.keySet).reduce(_ ++ _)

  def mergeVocab(docsVectors: Map[String, DocVector]): DocVector =
    docsVectors.values.toList.par.aggregate(Map[String, Int]())(merge, merge)

  def merge(docVector1: DocVector, docVector2: DocVector): DocVector =
    docVector1 ++ docVector2.map { case (token, coord) => token -> (coord + docVector1.getOrElse(token, 0)) }

  def saveResultMap(result: Map[String, ListBuffer[String]], filename: String) = {
    // Write results
    import java.io._
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    var result_per_doc = new String
    result foreach { case (key, value) => {
      result_per_doc = key + " " + value.mkString(" ") + "\n"
      bw.write(result_per_doc)
    }
    }
    bw.close()
  }

  def readResultFile(file: File): Map[String, List[String]] =
    scala.io.Source.fromFile(file).getLines().map(line => {
      val tokents = line.split(" ")
      val name = tokents.head
      val codes = tokents.tail.toList
      (name, codes)
    }).toMap

  /**
    * Creates Map with Code Definitions from zip files from given stream
    *
    * @param stream Inout stream with zip
    * @return Map: Code -> Definition
    */
  def getCodeValueMap(stream: Stream[InputStream]): Map[String, String] =
  stream.map(fileInputStream =>
    // convert each file to a map: (code -> text)
    fromInputStream(fileInputStream).getLines()
      .filterNot(_ startsWith ";")
      .map(_ split "\t")
      // checking if value pair is pair: code, value
      .collect { case Array(code, value) => (code, value) }
      .toMap
    // making one map with all codes
  ) reduce (_ ++ _)
}
