

package org.poker
// Assumes 10 properly formed cards and input.  Minimal error handling
// Assumes the validity of input hands, i.e card description, counts, duplicates etc
// player 1 win is designated [-1], player 2 [1], split pot [0]

object Poker {
    def main(args: Array[String]) {

        val results = io.Source.stdin.getLines.map(l => processInput(l)).toList.groupBy(identity).mapValues(_.size).toList

        for(r <- results) {

          val p = if(r._1 == -1) "Player 1" else if(r._1 == 1) "Player 2" else "Split"
          println(s"$p: ${r._2} hands")

        }


    }

    // entry function to process a hand and return the result
    def processInput(hand: String): Int = {

      val cards = hand.split("\\s+")

      // prepare the raw input array.  length required to slice a 10-face card
      val cardsT = cards.map(x => {
        val l = x.length
        (mapFace(x.slice(0,l-1)), x.slice(l-1,l))
      })

      // need to have 10 elements or out of bounds
      val cards1 = cardsT.slice(0,5)
      val cards2 = cardsT.slice(5,10)

      val outcome = compareHands(cards1, cards2)
      outcome

    }

    // p1 win returns -1, p2 win returns 1, split pot returns 0
    // compare strengths and then high card if required
    def compareHands(h1: Array[(Int, String)], h2: Array[(Int, String)]): Int = {
      val (faces1, suits1) = reformHand(h1)
      val (faces2, suits2) = reformHand(h2)

      val strength1 = labelHand(faces1, suits1)
      val strength2 = labelHand(faces2, suits2)

      if(strength1 > strength2) - 1 else if (strength2 > strength1) 1 else {
          // same strength check high cards
          highCardCheck(faces1, faces2)

      }

    }

    // list assumed already be sorted
    // lists assumed same length
    // p1 win returns -1, p2 win returns 1, split pot returns 0
    def highCardCheck(h1: List[(Int, Int)], h2: List[(Int, Int)]): Int = {

      @annotation.tailrec
      def highCard(l1: List[(Int, Int)], l2: List[(Int, Int)]): Int = {

        (l1, l2) match {
          case (Nil, Nil) => 0 // split pot
          case (h1::t1, h2::t2) if h1._1 > h2._1 => -1
          case (h1::t1, h2::t2) if h1._1 < h2._1 => 1
          case (h1::t1, h2::t2) => highCard(t1, t2) // check next card

        }

      }

      highCard(h1, h2)

    }

    // determines the strength of the hand
    // strongest 10 royal flush to 1 being high card
    // unrecognised or no input returns 0
    def labelHand(faceCount: List[(Int, Int)], suits: List[(String, Int)]): Int = {

      faceCount  match {

        case Nil => 0
        case (a, 1)::(b, 1)::(c, 1)::(d, 1)::(e, 1)::Nil =>
          val suited = suits.size == 1
          val straight = (a-e) == 4
          val royal = a == 14

          (suited, straight, royal) match {

            case (true, true, true) => 10 // royal flush
            case (true, true, false) => 9 // straight flush
            case (true, false, _) => 6 // flush
            case (false, true, _) => 5 //straight
            case _ => 1 // high hard
          }

        case (a, 2) :: (b, 1)::(c, 1)::(d, 1)::Nil => 2 //pair
        case (a, 2) :: (b, 2)::(c, 1)::Nil => 3 //2 pair
        case (a, 3) :: (b, 1)::(c, 1)::Nil => 4 //trips
        case (a, 3) :: (b, 2)::Nil => 7 // full house
        case (a, 4) :: (b, 1)::Nil => 8 // quads

        case _ => 0

      }

    }

    // group counts of both card values and suits and sorts the values cards
    def reformHand(raw: Array[(Int, String)]): (List[(Int, Int)], List[(String, Int)])  = {

      val faces = raw.map(x => x._1).groupBy(identity).mapValues(_.size).toList.sortBy({case (k,v) => (-v,-k)})
      val suits = raw.map(x => x._2).groupBy(identity).mapValues(_.size).toList

      (faces, suits)
    }

    // translate face cards to numbers
    // incorrect input is a 0
    // A is always high
    def mapFace(s: String): Int = {

      s match {
        case "A" => 14
        case "K" => 13
        case "Q" => 12
        case "J" => 11
        case "T" => 10
        case f => util.Try(f.toInt).toOption.getOrElse(0)
      }

    }



}
