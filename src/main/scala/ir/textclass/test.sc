val map1 = Map("a" -> 1, "b" -> 2, "c" -> 3)
val map2 = Map("a" -> 1, "b" -> 2, "c" -> 3)

val in = Array(map1, map2)


def merge(m1: Map[String, Int], m2: Map[String, Int]): Map[String, Int] =
  m1 ++ m2.map { case (k, v) => k -> (v + m1.getOrElse(k, 0)) }

in.par.aggregate(Map[String, Int]())(merge, merge)

scala.math.log(1)