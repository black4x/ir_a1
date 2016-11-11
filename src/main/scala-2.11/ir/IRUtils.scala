package ir

import java.io.{File, FileInputStream, InputStream, PrintWriter}

import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import com.github.aztek.porterstemmer.PorterStemmer
import com.lambdaworks.jacks.JacksMapper

import scala.io.Source._

object IRUtils {
  type DocVector = Map[String, Int]

  def totalCoordinateSum(docsVectorsList: List[DocVector]): Int =
    docsVectorsList.map(docVector => docVector.values.reduce((_) + _)).reduce((_) + _)

  def coordinateSumByToken(token: String, docsVectorsList: List[DocVector]): Int =
    docsVectorsList.map(docVector => getCoordinateWithLaplase(docVector.get(token))).reduce((_) + _)

  def getCoordinateWithLaplase(coordinate: Option[Int]): Int = coordinate match {
    case Some(i) => i + 1
    case None => 1
  }

  def getDocVector(doc: XMLDocument): DocVector = StopWords.filterOutSW(Tokenizer.tokenize(doc.content))
    .map(token => PorterStemmer.stem(token)) // saves almost nothing, but takes a lot of time => useless?
    .groupBy(identity).mapValues(_.size)

  def getAllDocsVectors(stream: Stream[XMLDocument]): Map[String, DocVector] =
    stream.map(doc => doc.name -> getDocVector(doc)).toMap

  def totalSumCoordinates(docsVector: Map[String, DocVector]): Int =
    docsVector.values.map(docVector => docVector.values.reduce((_) + _)).reduce((_) + _)

  //  def sumByToken(token: String, docsVector: Map[String, DocVector]): Double =
  //    docsVector.foldLeft(0.0)((currentSum, item) => sumWithLaplase(currentSum, item._2.get(token)))

  def getSetOfDistinctTokens(docsVectors: Map[String, DocVector]): Set[String] =
    docsVectors.values.map(docVector => docVector.keySet).reduce(_ ++ _)

  def getSetOfDistinctTokens(docsVectorsList: List[DocVector]): Set[String] =
    docsVectorsList.map(docVector => docVector.keySet).reduce(_ ++ _)

  def readAllDocsVectors(trainStream: Stream[XMLDocument]): Map[String, DocVector] = {
    // trying to read from cash file : [docName -> DocVector]
    val file = new File("train")
    if (file.exists()) {
      val instance = JacksMapper.readValue[Map[String, DocVector]](new FileInputStream(file))
      println("train size = " + instance.size)
      instance
    } else {
      // if not exists then creating map: [docName -> DocVector]
      val allDocsVectors = getAllDocsVectors(trainStream)
      // saving to file
      JacksMapper.writeValue(new PrintWriter(new File("train")), allDocsVectors)
      allDocsVectors
    }
  }

  def readAllRealCodes(trainStream: Stream[XMLDocument]): Set[String] = {
    // trying to read from cash file
    val file = new File("codes")
    if (file.exists()) {
      val instance = JacksMapper.readValue[Set[String]](new FileInputStream(file))
      println("codes size = " + instance.size)
      instance
    } else {
      // if not exists then creating set
      val codesSet = trainStream.map(doc => doc.codes).reduce(_ ++ _)
      // saving to file
      JacksMapper.writeValue(new PrintWriter(new File("codes")), codesSet)
      codesSet
    }
  }

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
