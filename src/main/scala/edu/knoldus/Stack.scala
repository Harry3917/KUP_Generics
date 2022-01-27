package edu.knoldus

trait Stack[+A] {
  def push[B >: A](element: B): Stack[B]

  def pop[B >: A]: Stack[B]

  def top: A

  def isEmpty: Boolean

  def printElements: String

}