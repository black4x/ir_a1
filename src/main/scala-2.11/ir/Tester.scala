package ir

object Tester {
  def testClassifier(myClassifier: Classifier, allTestVectors: Map[String, Map[String, Int]], allTesty: Map[String, Int]) = {
    var tp = 0
    var tn = 0
    var fp = 0
    var fn = 0
    val size = allTestVectors.size
    val allTestKeys = allTestVectors.keys.toList
    for (docKey <- allTestKeys) {
      val pred = myClassifier.prediction(allTestVectors(docKey))
      val y = allTesty.getOrElse(docKey, -1)
      if (pred > 0.0) {
        if (y == 1) {
          tp += 1
        }
        else {
          fp += 1
        }
      }
      else {
        if (y == 1) {
          fn += 1
        }
        else {
          tn += 1
        }
      }
    }
    println("Accuracy:%f precision:%f Recall:%f".format((tp + tn) / (tp + tn + fp + fn.toDouble),
      tp / (tp + fp.toDouble),
      tp / (tp + fn.toDouble)))
  }
}
