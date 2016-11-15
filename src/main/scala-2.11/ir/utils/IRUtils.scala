package ir.utils

import java.io.{File, FileInputStream, InputStream, PrintWriter}

import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import com.github.aztek.porterstemmer.PorterStemmer
import com.lambdaworks.jacks.JacksMapper

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

  def saveResultMap(result: scala.collection.mutable.Map[String, ListBuffer[String]], filename: String) = {
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

  def readResultFile(file: File): Map[String, List[String]] =
    scala.io.Source.fromFile(file).getLines().map(line => {
      val tokents = line.split(" ")
      val name = tokents.head
      val codes = tokents.tail.toList
      (name, codes)
    }).toMap

  /**
    * add code/label to result set if number is positive
    *
    * @param resultsMap
    * @param docName
    * @param newCode
    */
  def addCodeToResultMap(resultsMap: scala.collection.mutable.Map[String, ListBuffer[String]], docName: String, newCode: String): Unit = {
    if (resultsMap.contains(docName)) resultsMap.getOrElse(docName, ListBuffer[String]()) += newCode
    else resultsMap += (docName -> ListBuffer[String](newCode))
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
