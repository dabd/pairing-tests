package fruitmachine

object FM {

  sealed trait Colour

  case object Black extends Colour
  case object White extends Colour
  case object Green extends Colour
  case object Yellow extends Colour

  type Money = BigDecimal
  type Slots = (Colour, Colour, Colour, Colour)

  case class FruitMachine(prize: Money, randomSlotGenerator: () => Slots)

  case class Player(bankroll: Money,
                    freePlays: BigDecimal = 0,
                    prizeWon: BigDecimal = 0)

  val costOfPlay: BigDecimal = 10

  def randomColour: Colour = {
    val colours = List(Black, White, Green, Yellow)
    colours(scala.util.Random.nextInt(3))
  }

  def randomSlots: Slots =
    (randomColour, randomColour, randomColour, randomColour)

  def play(fruitMachine: FruitMachine,
           player: Player): (FruitMachine, Player) = {
    fruitMachine.randomSlotGenerator() match {
      case slots if isJackpot(slots) =>
        payOut(fruitMachine.prize, fruitMachine, player)
      case slots if allDifferentColours(slots) =>
        payOut(fruitMachine.prize / 2, fruitMachine, player)
      case slots if hasAdjacentColours(slots) =>
        payOut(5 * costOfPlay, fruitMachine, player)
      case _ => payOut(0, fruitMachine, player)
    }
  }

  def payOut(prizeWon: Money,
             fruitMachine: FruitMachine,
             player: Player): (FruitMachine, Player) = {
    if (prizeWon > fruitMachine.prize)
      (FruitMachine(0, fruitMachine.randomSlotGenerator),
       Player(player.bankroll - costOfPlay + fruitMachine.prize,
              player.freePlays + prizeWon - fruitMachine.prize,
              prizeWon))
    else
      (FruitMachine(fruitMachine.prize - prizeWon,
                    fruitMachine.randomSlotGenerator),
       Player(player.bankroll - costOfPlay + prizeWon,
              player.freePlays,
              prizeWon))
  }

  def tuple4ToList[T](t: (T, T, T, T)): List[T] = List(t._1, t._2, t._3, t._4)

  def isJackpot(slots: Slots): Boolean = {
    val slotsList = tuple4ToList(slots)
    slotsList.forall(_ == slotsList.head)
  }

  def allDifferentColours(slots: Slots): Boolean = {
    val slotsList = tuple4ToList(slots)
    slotsList.distinct == slotsList
  }

  def hasAdjacentColours(slots: Slots): Boolean = {
    val slotsList = tuple4ToList(slots)
    val (_, countAdjacent) = slotsList.tail.foldLeft((slotsList.head, 1)) {
      case ((prevSlot, count), currSlot) =>
        if (currSlot == prevSlot) (currSlot, count + 1)
        else (currSlot, count)
    }
    countAdjacent >= 2 && countAdjacent < 4
  }

}
