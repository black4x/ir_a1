import ch.ethz.dal.tinyir.util.StopWatch
import classifires.NaiveBayes

object Go extends App {
  val watch = new StopWatch()
  val path = "/home/ajuodelis/eth/ir/data_real";

  val codesMap = Utils.getCodeValueMap(path + "/codes")

  watch.start

  val naiveBayes = new NaiveBayes(codesMap, path +"/train")

  watch.stop
  println(watch.stopped)

}