package chapter06.functionalstate

object CandyMachine:

  object Me:
    enum Input:
      case Coin, Turn

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    // I inverted candies & coins in result (but I would have defined a case class instead of in true modelisation)
    // Solution will be inverted compared to book
    // We do not need State.modify as in the Solution, as we have our private simulateMachine returns a State
    // State.traverse would have been nice, to have State.traverse(inputs)(simulateMachine) instead of the Sequence
    // The solution has a much nicer state transition description compare to the multiple methods here, as we have to deal with State all the way
    object Machine:
      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
        _ <- State.sequence(inputs.map(simulateMachine))
        s <- State.get
      } yield (s.candies, s.coins)

      private def simulateMachine(input: Input): State[Machine, (Int, Int)] = State {
        case m @ Machine(_, 0, coins) => ((0, coins), m)
        case m =>
          input match
            case Input.Coin => insertCoin().run(m)
            case Input.Turn => turnKnob().run(m)
      }

      private def insertCoin(): State[Machine, (Int, Int)] = State {
        case m @ Machine(true, candies, coins) =>
          val newCoins = coins + 1
          ((candies, newCoins), Machine(false, candies, newCoins))
        case m @ Machine(false, candies, coins) => ((candies, coins), m)
      }

      private def turnKnob(): State[Machine, (Int, Int)] = State {
        case m @ Machine(true, candies, coins) => ((candies, coins), m)
        case m @ Machine(false, candies, coins) =>
          val newCandies = candies - 1
          ((newCandies, coins), Machine(true, newCandies, coins))
      }

  object Solution:
    enum Input:
      case Coin, Turn

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    object Machine:

      // Thanks to new traverse method, we can easily transform the machine
      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
        for {
          _ <- State.traverse(inputs)(i => State.modify(update(i)))
          s <- State.get
        } yield (s.candies, s.coins)

      // This actually combine all rules in simulateMachine in Me, with dealing with State
      // The method is curried in a val to be easier to pass to State.modify
      private val update = (i: Input) =>
        (s: Machine) =>
          (i, s) match
            case (_, Machine(_, 0, _))                        => s
            case (Input.Coin, Machine(false, _, _))           => s
            case (Input.Turn, Machine(true, _, _))            => s
            case (Input.Coin, Machine(true, candies, coins))  => Machine(false, candies, coins + 1)
            case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
