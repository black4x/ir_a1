val arr = Array('a', 'a', 'b', 'b')

arr.groupBy(identity).foreach(l=>println(l._2(0)))