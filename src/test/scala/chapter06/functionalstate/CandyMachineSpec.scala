package chapter06.functionalstate

import chapter06.functionalstate.CandyMachine.Me.Input.{Coin, Turn}
import chapter06.functionalstate.CandyMachine.Me.Machine
import munit.FunSuite

class CandyMachineSpec extends FunSuite {
  test("simulateMachine should allow buying candies") {
    val m = Machine(true, 5, 10)
    val ((candies, coins), updatedMachine) =
      Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(m)
    assertEquals(candies, 1)
    assertEquals(coins, 14)
    assertEquals(updatedMachine, Machine(true, 1, 14))
  }

  test("simulateMachine should not change machine when no inputs") {
    val m = Machine(true, 5, 10)
    val ((candies, coins), updatedMachine) = Machine.simulateMachine(List.empty).run(m)
    assertEquals(candies, 5)
    assertEquals(coins, 10)
    assertEquals(updatedMachine, m)
  }

  test("simulateMachine should keep only one coin when multiple coins are inserted following one another") {
    val m = Machine(true, 5, 10)
    val ((candies, coins), updatedMachine) = Machine.simulateMachine(List(Coin, Coin)).run(m)
    assertEquals(candies, 5)
    assertEquals(coins, 11)
    assertEquals(updatedMachine, Machine(false, 5, 11))
  }

  test("simulateMachine should give only one candy when turning knob multiple times avec a Coin") {
    val m = Machine(true, 5, 10)
    val ((candies, coins), updatedMachine) = Machine.simulateMachine(List(Coin, Turn, Turn)).run(m)
    assertEquals(candies, 4)
    assertEquals(coins, 11)
    assertEquals(updatedMachine, Machine(true, 4, 11))
  }

  test("simulateMachine should not give candy when machine is locked") {
    val m = Machine(true, 5, 10)
    val ((candies, coins), updatedMachine) = Machine.simulateMachine(List(Turn)).run(m)
    assertEquals(candies, 5)
    assertEquals(coins, 10)
    assertEquals(updatedMachine, m)
  }

  test("simulateMachine should not keep coins when machine has no candy") {
    val m = Machine(true, 0, 10)
    val ((candies, coins), updatedMachine) = Machine.simulateMachine(List(Coin, Coin)).run(m)
    assertEquals(candies, 0)
    assertEquals(coins, 10)
    assertEquals(updatedMachine, m)
  }
}
