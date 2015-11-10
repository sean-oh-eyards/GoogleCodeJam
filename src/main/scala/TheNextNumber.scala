object TheNextNumber extends App 
{

  def nextNumber(number: String): String = 
  {

    @annotation.tailrec
    def findSuffix(number: String, acc: Seq[Char]): (String, Seq[Char]) =
      if (number.isEmpty) 
        ("0", acc)
      else if (number.last < acc.head) 
        (number, acc)
      else 
        findSuffix(number.init, number.last +: acc)

    val (suffix, remain) = findSuffix(number.init, number.last :: Nil)
    val digit = remain.filter(_ > suffix.last).min

    suffix.init + digit ++ (suffix.last +: remain diff digit +: Nil).sorted.mkString
  }

  def doIt(lineIn: Iterator[String])(outfun: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) 
      outfun(s"Case #$i: ${nextNumber(lineIn.next())}")

  val writer = new java.io.PrintWriter("b.large.out")
  
  try 
  {
    doIt(io.Source.fromFile("B-large-practice.in").getLines) { s => writer.println(s);writer.flush() } 
  } 
  finally 
  {
    writer.flush(); writer.close()
  }
}
