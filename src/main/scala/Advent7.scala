import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent7:

  def main(args: Array[String]): Unit = {

    val rawLines = Files.readString(Path.of("./inputs/input7.txt")).linesIterator.toSeq

    val strengths: Map[Char, Int] = Map(
      'A' -> 14,
      'K' -> 13,
      'Q' -> 12,
      'J' -> 1, // Joker
      'T' -> 10
    ).withDefault(_.toString.toInt)

    val bids = rawLines.map(line => {
      val parts = line.split(" ")
      Bid(parts.head.map(strengths).toVector, parts.last.toInt)
    })

    val rankedBids = bids
        .sortBy(b => (b.handType.value, b.hand(0), b.hand(1), b.hand(2), b.hand(3), b.hand(4)))
        .zipWithIndex.map((b, i) => RankedBid(b, i + 1))

    println(rankedBids.map(_.winnings).sum) // 247899149

  }

  case class RankedBid(bid: Bid, rank: Int) {
    val winnings: Int = bid.bid * rank
  }

  case class Bid(hand: Vector[Int], bid: Int) {

    val handType: HandType = {
      val (jokers, normalHand) = hand.partition(_ == 1)
      val sorted = normalHand.groupBy(identity).values.map(_.size).toSeq.filter(_ > 1).sorted.reverse
      val jokerCount = jokers.size
      (sorted, jokerCount) match {
        case (Seq(5), _) => HandType.FiveOfAKind
        case (Seq(4), 1) => HandType.FiveOfAKind
        case (Seq(3), 2) => HandType.FiveOfAKind
        case (Seq(2), 3) => HandType.FiveOfAKind
        case (Nil, 4) => HandType.FiveOfAKind
        case (Nil, 5) => HandType.FiveOfAKind
        case (Seq(4), 0) => HandType.FourOfAKind
        case (Seq(3), 1) => HandType.FourOfAKind
        case (Seq(2), 2) => HandType.FourOfAKind
        case (Nil, 3) => HandType.FourOfAKind
        case (Seq(3, 2), 0) => HandType.FullHouse
        case (Seq(2, 2), 1) => HandType.FullHouse
        case (Seq(3), 0) => HandType.ThreeOfAKind
        case (Seq(2), 1) => HandType.ThreeOfAKind
        case (Nil, 2) => HandType.ThreeOfAKind
        case (Seq(2, 2), 0) => HandType.TwoPairs
        case (Seq(2), 0) => HandType.OnePair
        case (Nil, 1) => HandType.OnePair
        case (Nil, 0) => HandType.HighCard
        case _ => throw new IllegalArgumentException(sorted.toString)
      }
    }

  }

  enum HandType(val value: Int):
    case HighCard extends HandType(1)
    case OnePair extends HandType(2)
    case TwoPairs extends HandType(3)
    case ThreeOfAKind extends HandType(4)
    case FullHouse extends HandType(5)
    case FourOfAKind extends HandType(6)
    case FiveOfAKind extends HandType(7)
