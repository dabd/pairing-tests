package fruitmachine

import fruitmachine.FM._
import org.scalacheck._

class FMTest extends CommonSpec {

  val genJackpot: Gen[Slots] =
    for {
      s <- Gen.oneOf(Black, White, Green, Yellow)
    } yield (s, s, s, s)

  val genSlotsDifferentColours: Gen[Slots] =
    (for {
      s1 <- Gen.oneOf(Black, White, Green, Yellow)
      s2 <- Gen.oneOf(Black, White, Green, Yellow)
      s3 <- Gen.oneOf(Black, White, Green, Yellow)
      s4 <- Gen.oneOf(Black, White, Green, Yellow)
    } yield (s1, s2, s3, s4)) retryUntil (allDifferentColours(_))

  val genSlotsAdjacentColours: Gen[Slots] =
    (for {
      s1 <- Gen.oneOf(Black, White, Green, Yellow)
      s2 <- Gen.oneOf(Black, White, Green, Yellow)
      s3 <- Gen.oneOf(Black, White, Green, Yellow)
      s4 <- Gen.oneOf(Black, White, Green, Yellow)
    } yield (s1, s2, s3, s4)) retryUntil (hasAdjacentColours(_))

  val genSlotsWithPrize: Gen[Slots] =
    Gen.oneOf(genJackpot, genSlotsDifferentColours, genSlotsAdjacentColours)

  val genSlotsWithoutPrize: Gen[Slots] =
    (for {
      s1 <- Gen.oneOf(Black, White, Green, Yellow)
      s2 <- Gen.oneOf(Black, White, Green, Yellow)
      s3 <- Gen.oneOf(Black, White, Green, Yellow)
      s4 <- Gen.oneOf(Black, White, Green, Yellow)
    } yield
      (s1, s2, s3, s4)) retryUntil (slots =>
                                      !isJackpot(slots) && !allDifferentColours(
                                        slots) && !hasAdjacentColours(slots))

  val genSlots: Gen[Slots] =
    Gen.frequency((8, genSlotsWithoutPrize), (2, genSlotsWithPrize))

  def genFruitMachine(
      slotGen: Gen[Slots],
      insufficientPrizeMoney: Boolean = false): Gen[FruitMachine] =
    for {
      prize <- {
        if (insufficientPrizeMoney) Gen.choose(0, costOfPlay.toInt)
        else Gen.choose(5 * costOfPlay.toInt, 10000)
      }
      randomSlots <- slotGen
      randomSlotGenerator <- Gen.const(() => randomSlots)
    } yield FruitMachine(prize, randomSlotGenerator)

  val genPlayer: Gen[Player] =
    for {
      bankroll <- Gen.choose(0, 10000)
      freePlays <- Gen.choose(0, 10)
    } yield Player(bankroll, freePlays)

  def genPlay(slotGen: Gen[Slots], insufficientPrizeMoney: Boolean = false) =
    for {
      fm <- genFruitMachine(slotGen, insufficientPrizeMoney)
      p <- genPlayer
    } yield (fm, p)

  "payOut" should {
    """credit a player a number of plays equal to the difference between the prize won and the available prize if it contains
      |insufficient money to award the prize.
      |The player should win all the money in the machine minus the cost of a single play.
      |The fruit machine should contain no money afterwards.
    """.stripMargin in {
      forAll(genFruitMachine(genSlotsAdjacentColours,
                             insufficientPrizeMoney = true),
             genPlayer) {
        case (fm, p) =>
          val prizeWon = costOfPlay * 5
          val (fm2, p2) = payOut(prizeWon, fm, p)
          p2.bankroll shouldBe p.bankroll - costOfPlay + fm.prize
          p2.freePlays shouldBe p.freePlays + p2.prizeWon - fm.prize
          p2.prizeWon shouldBe prizeWon
          fm2.prize shouldBe 0
          fm2.randomSlotGenerator shouldBe fm.randomSlotGenerator
      }
    }

    "award the player the prize won if it contains sufficient credit" in {
      forAll(for {
        fm <- genFruitMachine(genSlots)
        p <- genPlayer
        prizeWon <- Gen.choose(0, fm.prize.toInt)
      } yield (fm, p, prizeWon)) {
        case (fm, p, prizeWon) =>
          val (fm2, p2) = payOut(prizeWon, fm, p)
          p2.bankroll shouldBe p.bankroll - costOfPlay + prizeWon
          p2.freePlays shouldBe p.freePlays
          p2.prizeWon shouldBe prizeWon
          fm2.prize shouldBe fm.prize - prizeWon
          fm2.randomSlotGenerator shouldBe fm.randomSlotGenerator
      }
    }
  }

  "hasAdjacentColours" should {
    "return true if there are 2 or more adjacent coloured slots but not all of them are the same colour" in {
      forAll(genSlots) { slots =>
        val slotList = tuple4ToList(slots)
        hasAdjacentColours(slots) shouldBe
          slotList.sliding(2).toList.exists { case List(s1, s2) => s1 == s2 } && !slotList
            .forall(_ == slotList.head)
      }
    }
  }

  "isJackpot" should {
    "return true if all slots are of the same colour" in {
      forAll(genSlots) { slots =>
        isJackpot(slots) shouldBe tuple4ToList(slots)
          .sliding(2)
          .toList
          .forall { case List(s1, s2) => s1 == s2 }
      }
    }
  }

  "allDifferentColours" should {
    "return true if all slots have different colours" in {
      forAll(genSlots) { slots =>
        allDifferentColours(slots) shouldBe tuple4ToList(slots).toSet.size == slots.productArity
      }
    }
  }

  "player" should {

    "win a jackpot if the fruit machine displays all slots of the same colour" in {
      forAll(genPlay(genJackpot)) {
        case (fm, p) =>
          val (fm2, p2) = play(fm, p)
          (fm2, p2) shouldBe payOut(fm.prize, fm, p)
      }
    }

    "win half of the prize money if the slots display different colours" in {
      forAll(genPlay(genSlotsDifferentColours)) {
        case (fm, p) =>
          val (fm2, p2) = play(fm, p)
          (fm2, p2) shouldBe payOut(fm.prize / 2, fm, p)
      }
    }

    "win 5 times the cost of a single play if the slots display two or more adjacent colours but not a jackpot" in {
      forAll(genPlay(genSlotsAdjacentColours)) {
        case (fm, p) =>
          val (fm2, p2) = play(fm, p)
          (fm2, p2) shouldBe payOut(costOfPlay * 5, fm, p)
          p2.freePlays shouldBe p.freePlays

      }
    }

    """be credited a number of free plays equal to the difference between the full prize money and the available money
        |if the machine does not have enough money to pay a prize""".stripMargin in {
      forAll(genPlay(genSlotsAdjacentColours, insufficientPrizeMoney = true)) {
        case (fm, p) =>
          val (fm2, p2) = play(fm, p)
          (fm2, p2) shouldBe payOut(costOfPlay * 5, fm, p)
          p2.freePlays shouldBe p.freePlays + p2.prizeWon - fm.prize
      }
    }

    "lose the cost of a play if no prize is awarded" in {
      forAll(genPlay(genSlotsWithoutPrize)) {
        case (fm, p) =>
          val (fm2, p2) = play(fm, p)
          (fm2, p2) shouldBe payOut(0, fm, p)
      }
    }
  }

}
