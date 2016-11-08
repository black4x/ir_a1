import ch.ethz.dal.tinyir.io.ZipDirStream

import scala.io.Source._

object Utils {

  /**
    * Creates Map with Code Definitions from zip files in given directory
    * @param path to Dir with zip
    * @return Map: Code -> Definition
    */
  def getCodeValueMap(path: String): Map[String, String] =
    new ZipDirStream(path).stream.map(fileInputStream =>
      // convert each file to a map: (code -> text)
      fromInputStream(fileInputStream).getLines()
        .filterNot(_ startsWith ";" )
        .map(_ split "\t")
        // checking if value pair is pair: code, value
        .collect { case Array(code, value) => (code, value) }
        .toMap
      // making one map with all codes
    ) reduce (_ ++ _)
}
