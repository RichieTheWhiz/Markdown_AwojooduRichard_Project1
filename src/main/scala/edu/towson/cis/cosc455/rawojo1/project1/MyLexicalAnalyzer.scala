package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : MyLexicalAnalyzer
//


import scala.collection.mutable.ArrayBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer{
  val lexems: List[String] = List("\\BEGIN", "\\END", "\\TITLE[", "]", "#", "\\PARB","\\PARE", "**", "*", "+", "\\", "[", "(", ")", "![", "\\DEF[", "=", "\\USE[")
  var pos: Int = -1
  var c: Char = ' '
  var currentToken: String = "" //the potential string which will be a token that is fully checked against the grammar
  var isText : Boolean = false //checks whether or not tokens are text
  var check : Boolean = false


  def lookup(): Boolean = {
    var flag = false
    if (lexems.contains(currentToken.toUpperCase())) {
      flag = true
    }
    flag
  }

  override def addChar(): Unit = {
    currentToken = currentToken + c
  }

  override def getNextToken(): Unit = {
    Compiler.Parser.isText = false
    if(isGap()){
      c = getChar()
      while(isGap()){
        c = getChar()
      }
    }
    if(lexems.contains(c.toString)){
      Compiler.currentToken = c.toString
      if(c.equals('=') || c.equals(']'))
        c = getChar()
    }else if(notText()){
      addChar()
      c = getChar()
      while(!close()){
        addChar()
        c= getChar()
      }
      if(lexems.contains(c.toString))
        addChar()
      if(lookup()){
        Compiler.currentToken = currentToken
        currentToken = ""
        c = getChar()
      }else{
        println(currentToken)
        println("Lexical Error - Unrecognized Special Character")
        System.exit(1)
      }
    }else if(text()){
      while(text()){
        Compiler.Parser.isText = true
        addChar()
        c = getChar()
      }
      Compiler.currentToken = currentToken
      currentToken = ""
    }
  }


  override def getChar(): Char = {
    pos += 1
    Compiler.fileContents.charAt(pos)
  }

  def text() : Boolean = {
    isText = false
    for(ch <- 'A' to 'Z'){
      if(c.equals(ch))
        isText = true
    }
    for(ch <- 'a' to 'z'){
      if(c.equals(ch))
        isText = true
    }
    for(ch <- '0' to '9'){
      if(c.equals(ch))
        isText = true
    }
    if(c.equals(',') || c.equals('.') || c.equals('?') || c.equals('_')
      || c.equals('/') || c.equals('\"') || c.equals(' ')){
      isText = true
    }
    isText
  }


  def setCurrent(currentToken: String): Unit = {  // Sets current token in Compiler.scala
    Compiler.currentToken = currentToken
  }

  // Method for retrieving terminal tokens
  def isGap(): Boolean = {
    if(c.equals('\r') || c.equals('\n') || c.equals(' ') || c.equals('\t')){
      true
    }else{
      false
    }
  }

  def close(): Boolean ={
    c match{
      case '\r' | '\n' | '[' | '\\' | ']' | '*' | ')' | '(' => true
      case _ => false
    }
  }

  def notText(): Boolean ={
    c match{
      case '\\' | '*' | '#' | '+' | '[' | '!' => true
      case _ => false
    }
  }

}
