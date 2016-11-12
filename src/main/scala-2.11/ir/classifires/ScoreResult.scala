package ir.classifires

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ir.IRUtils

object ScoreResult extends App{
  val baseDir = "/home/ajuodelis/eth/ir/data_real"

  val validationPath = baseDir + "/validation"


  IRUtils.printScore(new ReutersRCVStream(validationPath))
}
