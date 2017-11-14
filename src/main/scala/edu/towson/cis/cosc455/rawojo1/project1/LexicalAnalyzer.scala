package edu.towson.cis.cosc455.rawojo1.project1


//
// Richard Awojoodu
// COSC455
// Project 1 : LexicalAnalyzer
//

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup(string: String) : Boolean

}
