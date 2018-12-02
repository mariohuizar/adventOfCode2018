import scala.annotation.tailrec

object DayOne extends App {
  def sumAllInputs(rawInput: String) =
    toIntSequence(rawInput).sum

  def findFirstRepeatedFrequency(
      rawInput: String
  ): Int = {
    val inputAsInts = toIntSequence(rawInput)

    @tailrec def innerLoop(input: List[Int], frequenciesSoFar: List[Int]): Int =
      input match {
        case currentElement :: xs =>
          frequenciesSoFar match {
            case lastFrequency :: _ =>
              if (frequenciesSoFar.contains(currentElement + lastFrequency)) {
                currentElement + lastFrequency
              } else {
                innerLoop(
                  xs,
                  (currentElement + lastFrequency) :: frequenciesSoFar
                )
              }
            case Nil =>
              innerLoop(xs, currentElement :: frequenciesSoFar)
          }

        case Nil => innerLoop(inputAsInts.toList, frequenciesSoFar)
      }

    innerLoop(inputAsInts.toList, Nil)
  }

  private val toIntSequence = (rawInput: String) =>
    rawInput.stripMargin.split('\n').toSeq.map(_.toInt)

  print(s"""Results:
       |Part One -> ${sumAllInputs(DayOneInput.input)}
       |Part Two -> ${findFirstRepeatedFrequency(DayOneInput.input)}
     """.stripMargin)
}
