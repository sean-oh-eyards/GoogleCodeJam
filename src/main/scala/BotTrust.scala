import collection.immutable.SortedSet

object BotTrust extends App 
{

  type ButtonPushSeq = List[(String, Int)]
  
  // (time, bot name, position) tuple
  type ArrivalSet = SortedSet[(Int, String, Int)]

  def minTime(orders: ButtonPushSeq): Int = 
  {
    def updatedArrivals(departureTime: Int, botPos: Int, orders: ButtonPushSeq) (
        arrivals: ArrivalSet, bot: String): ArrivalSet = 
    {
      val next = orders find (_._1 == bot) map (_._2)
      if (next.isEmpty) 
        arrivals
      else {
        val arrivalTime = (departureTime + (next.get - botPos).abs, bot, next.get)
        arrivals + arrivalTime
      }
    }

    @annotation.tailrec
    def simulate(time: Int, orders: ButtonPushSeq, botPos: Map[String, Int])(
        arrivals: ArrivalSet): Int =
      if (orders.isEmpty) 
        time
      else 
      {
        val (bot, button) = orders.head
        val (time1, orders1, botPos1, arrivals1) = 
          if (botPos(bot) == button)
            (time + 1, orders.tail, botPos,
              updatedArrivals(time + 1, button, orders.tail)(arrivals, bot))
          else 
          {
            val (arrivalTime, arrivedBot, button) = arrivals.head
            (arrivalTime max time, orders, botPos + (arrivedBot -> button), arrivals.tail)
          }
        simulate(time1, orders1, botPos1)(arrivals1)
      }

    simulate(0, orders, Map("O" -> 1, "B" -> 1)) {
      (SortedSet.empty[(Int, String, Int)] /: Seq("O", "B")) { updatedArrivals(0, 1, orders) }
    }
  }

  def doIt(lineIn: Iterator[String])(outFun: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) 
    {
      val order = lineIn.next().split(' ').tail.grouped(2).map {
        case Array(bot, button) => (bot, button.toInt)
      }.toList
      outFun(s"Case #$i: ${minTime(order)}")
    }

  val writer = new java.io.PrintWriter("BotTrust.a.large.out")
  try 
  {
    doIt(io.Source.fromFile("BotTrust.A-large-practice.in").getLines) { s =>
      writer.println(s)
      writer.flush()
    }
  } 
  finally 
  {
    writer.flush(); writer.close()
  }

}