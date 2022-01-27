package edu.knoldus

import org.scalatest.flatspec.AnyFlatSpec

class StackImplementationTest extends AnyFlatSpec {

  val nonEmptyStack = {
    new StackImplementation[Int](List(7, 11, 1, 3))
  }
  val emptyStack = {
    new StackImplementation[Nothing](List())
  }


  "A Condition" should "give false if stack is not Empty" in {
    assert(nonEmptyStack.isEmpty == false)
  }
  "A Condition" should "Push an element in the stack" in {
    assert(List(79, 7, 11, 1, 3) == nonEmptyStack.push(79))
  }
  "A Condition" should "give top element from the stack" in {
    nonEmptyStack.push(9)
    nonEmptyStack.push(5)
    assert(nonEmptyStack.top == 5)
  }
  "A Condition" should "Pop an element from the stack" in {
    nonEmptyStack.push(2)
    nonEmptyStack.push(13)
    assert(nonEmptyStack.pop == 13)
  }

  "A Condition" should "give true if stack is Empty" in {
    assert(emptyStack.isEmpty == true)
  }
  "A condition" should "throw an exception when we pop an element from an empty list" in {
    val error = "Stack is already empty"
    val thrown = intercept[Exception] {
      emptyStack.pop
    }
    assert(thrown.getMessage == error)
  }
  "A condition" should "throw an exception when we try to find top element from an empty list" in {
    val error = "Stack is already empty"
    val thrown = intercept[Exception] {
      emptyStack.top
    }
    assert(thrown.getMessage == error)
  }
}