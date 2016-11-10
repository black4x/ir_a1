package ir

import java.io.InputStream

import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import com.github.aztek.porterstemmer.PorterStemmer

import scala.io.Source._

object IRUtils {

  type DocVector = Map[String, Int]

  implicit class DocVectorOperations(vector: DocVector) {
    // summing all values in Map
    def sumCoordinates(): Int = vector.foldLeft(0)(_ + _._2)
  }

  def sumWithLaplase(curr: Double, coord: Option[Int]): Double = coord match {
    case Some(i) => curr + i.toDouble + 1.0
    case None => curr + 1.0
  }

  def getDocVector(doc: XMLDocument): DocVector = StopWords.filterOutSW(Tokenizer.tokenize(doc.content))
    .map(token => PorterStemmer.stem(token))
    .groupBy(identity).mapValues(_.size)

  def getAllDocsVectors(stream: Stream[XMLDocument]): Map[String, DocVector] =
    stream.map(doc => doc.name -> getDocVector(doc)).toMap

  def totalSumCoordinates(docsVector: Map[String, DocVector]): Int =
    docsVector.foldLeft(0)(_ + _._2.sumCoordinates)

  def sumByToken(token: String, docsVector: Map[String, DocVector]): Double =
    docsVector.foldLeft(0.0)((currentSum, item) => sumWithLaplase(currentSum, item._2.get(token)))

  def getSetOfDistinctTokens(docsVectors: Map[String, DocVector]) =
    docsVectors.values.foldLeft(Set[String]())((curr, docVector) => curr ++ docVector.keySet)

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
