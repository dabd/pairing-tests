package fruitmachine

import cats.data.State

object FM {

  sealed trait Colour

  case object Black extends Colour
  case object White extends Colour
  case object Green extends Colour
  case object Yellow extends Colour

  type Slots = (Colour, Colour, Colour, Colour)

  case class FruitMachine(prize: BigDecimal, randomSlotGenerator: () => Slots)

  case class Player(bankroll: BigDecimal,
                    freePlays: BigDecimal = 0,
                    prizeWon: BigDecimal = 0)

  type FMState = State[(FruitMachine, Player), Unit]

  val costOfPlay: BigDecimal = 10

  def randomColour: Colour = {
    val colours = List(Black, White, Green, Yellow)
    colours(scala.util.Random.nextInt(3))
  }

  def randomSlots: Slots =
    (randomColour, randomColour, randomColour, randomColour)

  def play: FMState =
    State {
      case (fruitMachine, player) =>
        fruitMachine.randomSlotGenerator() match {
          case slots if isJackpot(slots) =>
            (payOut(fruitMachine, player.copy(prizeWon = fruitMachine.prize)),
             ())
          case slots if hasDifferentColours(slots) =>
            (payOut(fruitMachine,
                    player.copy(prizeWon = fruitMachine.prize / 2)),
             ())
          case slots if hasAdjacentColours(slots) =>
            (payOut(fruitMachine, player.copy(prizeWon = 5 * costOfPlay)), ())
          case _ => (payOut(fruitMachine, player.copy(prizeWon = 0)), ())
        }
    }

  def payOut(fruitMachine: FruitMachine,
             player: Player): (FruitMachine, Player) =
    if (player.prizeWon > fruitMachine.prize)
      payoutFreePlays(fruitMachine, player)
    else payoutPrize(fruitMachine, player)

  def payoutFreePlays(fruitMachine: FruitMachine,
                      player: Player): (FruitMachine, Player) =
    (FruitMachine(0, fruitMachine.randomSlotGenerator),
     Player(player.bankroll - costOfPlay + fruitMachine.prize,
            player.freePlays + player.prizeWon - fruitMachine.prize,
            player.prizeWon))

  def payoutPrize(fruitMachine: FruitMachine,
                  player: Player): (FruitMachine, Player) =
    (FruitMachine(fruitMachine.prize - player.prizeWon,
                  fruitMachine.randomSlotGenerator),
     Player(player.bankroll - costOfPlay + player.prizeWon,
            player.freePlays,
            player.prizeWon))

  def tuple4ToList[T](t: (T, T, T, T)): List[T] = List(t._1, t._2, t._3, t._4)

  def isJackpot(slots: Slots): Boolean = {
    val slotsList = tuple4ToList(slots)
    slotsList.forall(_ == slotsList.head)
  }

  def hasDifferentColours(slots: Slots): Boolean = {
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
