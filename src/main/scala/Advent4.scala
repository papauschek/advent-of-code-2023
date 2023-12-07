import java.nio.file.{Files, Path}

object Advent4:

  def main(args: Array[String]): Unit = {

    val rawCards = Files.readString(Path.of("./inputs/input4.txt")).linesIterator.toSeq

    def parseCard(rawCard: String, index: Int): Card = {
      val (_, content) = rawCard.splitAt(rawCard.indexOf(":") + 1)
      val (rawWinners, rawDeck) = content.splitAt(content.indexOf("|"))
      val winners = rawWinners.split(' ').filter(_.nonEmpty).map(_.toInt).toSet
      val deck = rawDeck.drop(1).split(' ').filter(_.nonEmpty).map(_.toInt).toSeq
      Card(index, winners, deck)
    }

    val allCards = rawCards.zipWithIndex.map(parseCard).toVector

    println(allCards.map(_.score).sum) // Part 1 = 23750

    def countCards(card: Card): Long = {
      var cardCount = 1L
      (1 to card.winCount).foreach {
        number =>
          val nextCard = allCards(card.index + number)
          cardCount += countCards(nextCard)
      }
      cardCount
    }

    println(allCards.map(countCards).sum) // Part 2 = 13261850

  }

  case class Card(index: Int, winners: Set[Int], deck: Seq[Int]) {

    val winCount: Int = deck.count(winners)
    val score: Long = 1L << (winCount - 1)

  }

