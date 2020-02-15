package objsets

import scala.util.Try

object TweetSetTransformations {
  // TODO: this method should be inside a builder or something like that.
  // TODO: rethink this architecture: Maybe do not need to expose the leftElem.
  def toTweetList(tweetSet: TweetSet): TweetList = {
    // TODO: merge the following two functions
    def appendBranch(value: NonEmpty, tweetSetL: List[TweetSet])(getElemF: NonEmpty => TweetSet): List[TweetSet] = {
      try {
        // May raise exception
        getElemF(value).element
        getElemF(value) :: tweetSetL
      } catch {
        case e: Exception => {
          tweetSetL
        }
      }
    }

    def appendBranchLeft(value: NonEmpty, tweetSetL: List[TweetSet]): List[TweetSet] = {
      appendBranch(value, tweetSetL)((nonEmpty: NonEmpty) => nonEmpty.leftElem)
    }

    def appendBranchRight(value: NonEmpty, tweetSetL: List[TweetSet]): List[TweetSet] = {
      appendBranch(value, tweetSetL)((nonEmpty: NonEmpty) => nonEmpty.rightElem)
    }

    def loop(tweetSetL: List[TweetSet], acc: TweetList): TweetList = {
      if (tweetSetL.isEmpty) {
        return acc
      }

      // TODO: do not use pattern matching.
      tweetSetL.head match {
        case _: Empty => return acc
        case value: NonEmpty => {
          val listAppendedLeft = appendBranchLeft(value, tweetSetL.tail)
          val listAppendedBranches = appendBranchRight(value, listAppendedLeft)

          return loop(listAppendedBranches, acc.append(tweetSetL.head.element))
        }
      }
    }

    loop(List(tweetSet), Nil)
  }
}
