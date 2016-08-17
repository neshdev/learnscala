import objsets.{Empty, Tweet}

val set1 = new Empty
val set2 = set1.incl(new Tweet("a", "a body", 20))
val set3 = set2.incl(new Tweet("b", "b body", 20))
val c = new Tweet("c", "c body", 7)
val d = new Tweet("d", "d body", 9)
val set4c = set3.incl(c)
val set4d = set3.incl(d)
val set5 = set4c.incl(d)



set5.mostRetweeted

set5.remove(set5.mostRetweeted).foreach(x=> println(x.user))

val set5removed1 = set5.remove(set5.mostRetweeted)

set5removed1.remove(set5removed1.mostRetweeted).foreach(x=> println(x.user))

val set5removed2 = set5removed1.remove(set5removed1.mostRetweeted)

set5removed2.remove(set5removed2.mostRetweeted).foreach(x=> println(x.user))

val set5removed3 = set5removed2.remove(set5removed2.mostRetweeted)

set5removed3.remove(set5removed3.mostRetweeted).foreach(x=> println(x.user))

val set5removed4 = set5removed3.remove(set5removed3.mostRetweeted)

set5removed4.isEmpty
set5removed4.tweet


val rem = set5.remove(set5.mostRetweeted)
rem.remove(set5.mostRetweeted).foreach(x=> println(x.user))

set5.foreach(x => println(x.user))

set5.descendingByRetweet.foreach(x=> println(x.user))

set5.descendingByRetweet.foreach(x=> println(x.user))

set5.descendingByRetweet.foreach(x=> println(x.user))