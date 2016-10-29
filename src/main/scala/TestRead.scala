import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.Tokenizer

class TestRead {

  var length : Long = 0
  var tokens : Long = 0

  val path : String = "/Users/Ralph/Development/ETH/Information Retrieval/Project 1/zips"
  val reuters = new ReutersRCVStream(path)
  println("Number of files in zips = " + reuters.length)
  for (doc <- reuters.stream) {
    length += doc.content.length
    tokens += Tokenizer.tokenize(doc.content).length
  }
  println("Total number of characters = " + length)
  println("Total number of tokens     = " + tokens)

}
