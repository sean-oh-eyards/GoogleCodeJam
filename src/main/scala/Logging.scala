object Logging extends App 
{

  def minLogging(ref: (Int, Int), trees: Seq[(Int, Int)]): Int = 
  {
    require(!trees.contains(ref))

    def count(s: Seq[(Int, Int)], p: ((Int, Int)) => Boolean): (Int, Int) = 
    {
      val trues = s count p
      (trues, s.size - trues)
    }
    val (infinites, finites) = trees.map { case (x, y) => (x - ref._1, y - ref._2) }.partition(_._1 == 0)
    val (iniLeft, iniRight) = count(finites, _._1 > 0)
    val (iniFront, iniBack) = count(infinites, _._2 < 0)

    object tiltOrder extends Ordering[(Int, Int)] 
    {
      def compare(a: (Int, Int), b: (Int, Int)): Int = a._2.toLong * b._1 compare b._2.toLong * a._1
    }

    (collection.SortedMap.empty[(Int, Int), (Int, Int)](tiltOrder) /: finites) 
    {
      case (sortedMap, (dx, dy)) =>
        def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
        val g = gcd(dx.abs, dy.abs)
        val tilt = (dx.abs / g, dy * dx.signum / g)
        val (front, back) = sortedMap.getOrElse(tilt, (0, 0))
        sortedMap + (tilt -> (if (dx > 0) (front + 1, back) else (front, back + 1)))
    }.foldLeft(iniLeft min iniRight, iniLeft + iniBack, iniRight + iniFront) {
      case ((min, left, right), (tilt, (front, back))) =>
        val (left1, right1) = (left - front, right - back)
        (min min left1 min right1, left1 + back, right1 + front)
    }._1
  }

  def minLogging(trees: IndexedSeq[(Int, Int)]): Seq[Int] = 
    for 
    { 
      i <- 0 until trees.size 
      (front, end) = trees splitAt i
    } yield minLogging(trees(i), front ++ end.tail)

  def doIt(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) 
    {
      val trees = Vector.fill(lineIn.next().toInt) {
        val Array(x, y) = lineIn.next() split ' ' map (_.toInt)
        (x, y)
      }
      lineOut(s"Case #$i:")

      for (n <- minLogging(trees)) lineOut(n.toString)
    }

  val filename = "Logging.C-large-practice"
  val writer = new java.io.PrintWriter(filename + ".out")
  try 
  {
    doIt(io.Source.fromFile(filename + ".in").getLines) { s =>
      writer.println(s); writer.flush()
    }
  } 
  finally 
  {
    writer.flush(); writer.close()
  }
}