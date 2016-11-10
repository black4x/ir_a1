package ir

import java.io.{File, FileInputStream, InputStream, PrintWriter}

import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import com.github.aztek.porterstemmer.PorterStemmer
import com.lambdaworks.jacks.JacksMapper

import scala.io.Source._

object IRUtils {

  type DocVector = Map[String, Int]

  implicit class DocVectorOperations(vector: DocVector) {
    // summing all values in Map
    def sumCoordinates(): Int = vector.foldLeft(0)(_ + _._2)
  }

  def sumDocVector(docVector: DocVector) = docVector.sumCoordinates()

  def getWithLaplase(coord: Option[Int]): Double = coord match {
    case Some(i) => i.toDouble + 1.0
    case None => 1.0
  }

  def getDocVector(doc: XMLDocument): DocVector = StopWords.filterOutSW(Tokenizer.tokenize(doc.content))
    .map(token => PorterStemmer.stem(token)) // almost useless!
    .groupBy(identity).mapValues(_.size)

  def getAllDocsVectors(stream: Stream[XMLDocument]): Map[String, DocVector] =
    stream.map(doc => doc.name -> getDocVector(doc)).toMap

  def totalSumCoordinates(docsVector: Map[String, DocVector]): Int =
    docsVector.foldLeft(0)(_ + _._2.sumCoordinates)

  //def sumByToken(token: String, docsVector: Map[String, DocVector]): Double =
  //  docsVector.foldLeft(0.0)((currentSum, item) => sumWithLaplase(currentSum, item._2.get(token)))

  def getSetOfDistinctTokens(docsVectors: Map[String, DocVector]) =
    docsVectors.values.foldLeft(Set[String]())((curr, docVector) => curr ++ docVector.keySet)


  def mergeMap(ms: List[DocVector])(f: (Int, Int) => Int): DocVector =
    (Map[String, Int]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
      a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
    }

  def mergeAllDocs(allDocs: List[DocVector]): DocVector = mergeMap(allDocs)((v1, v2) => v1 + v2)

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
      val codesSet = trainStream.map(doc=> doc.codes).reduce(_ ++ _)
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
