package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : MyLexicalAnalyzer
//


import scala.collection.mutable.ArrayBuffer


class MyLexicalAnalyzer extends LexicalAnalyzer{

  var sou: List[Char] = Nil
  private var currentToken: String = ""
  private var nextChar: Char = ' '
  private var startChar: Char = ' '
  private var pos: Int = 0
  private var addingText: Boolean = false

  private val tag: List[Char] = List(
    '\\', '#', '*', '[' , '!', ']', '+', '(', ')', '='
  )

  private val lexems: List[String] = List(CONSTANTS.DOCB, CONSTANTS.DOCE, CONSTANTS.TITLEB, CONSTANTS.BRACKETE, CONSTANTS.HEADING, CONSTANTS.PARAB, CONSTANTS.PARAE, CONSTANTS.BOLD, CONSTANTS.LISTITEM, CONSTANTS.NEWLINE, CONSTANTS.LINKB,
    CONSTANTS.ADDRESSB, CONSTANTS.ADDRESSE, CONSTANTS.IMAGEB, CONSTANTS.DEFB, CONSTANTS.EQSIGN, CONSTANTS.USEB
  )

  //Takes the file content and places it in a List for future reference
  def start(test_file: String): Unit =
  {
    sou = test_file.toList
  }

  //Checks to see if the source file given is empty yet
  def isEmpty(): Boolean =
  {
    if (sou.isEmpty)
      true
    else
      false
  }

  //Appropriates the presence of Text in the File
  def isText(currentToken: String): Boolean = {
     currentToken match {
       case CONSTANTS.TEXT =>  true
       case _ => false
     }
  }


  //Appropriates the presence of spaces in the File
  def isSpace(c: Char): Boolean =
  {
    c == ' ' | c == '\n' | c == '\t' | c == '\r'
  }


  //Appropriates the presence of absence in the File
  def getNonBlank(): Unit =
  {
    while (isSpace(nextChar))
      getChar()
  }


  //Adding characters to the current token string for the sake of building a full recognizable token.
  override def addChar(): Unit =
  {
    currentToken += nextChar
  }


  override def getChar() : Char =
  {
    if (!sou.isEmpty)
    {
      nextChar = sou.head
      sou = sou.tail
      nextChar
    }
    else
    {
      println("Lexical Error: " + nextChar + " caused an issue.")
      System.exit(1)
      'R'
    }
  }


  //The next token is grabbed so that the next step can be seen
  override def getNextToken() : Unit =
  {
    currentToken = ""
    getNonBlank()

    bunch(nextChar)
  }


    //Appropriates the presence of a valid lexem in the File
  override def lookup(token : String): Boolean = {
    if (lexems.contains(token))
    {
      true
    }
    else
    {
      println("Lexical error: " + token + " is not a valid token.")
      System.exit(1)
      false
    }
  }

  //Text that is fitting the syntax , allows us to get more letters for text
  def goodText(): Unit =
  {
    getChar()
     //Richard was here

    while (!tag.contains(nextChar))
    {
      addChar()
      getChar()
    }
  }

  // Bunch is the presence of any special character such as [ , ] , or #
  def bunch(chr : Char) =
  {
    chr match {
      case '\\' =>
      {
        addChar()
        while (!isSpace(nextChar) && nextChar != '[') {
          if (currentToken.equalsIgnoreCase(CONSTANTS.DOCE))
            Compiler.checkFin = true

          getChar()

          if (!isSpace(nextChar)) {
            addChar()
          }
        }
        if (lookup(currentToken.toUpperCase())) {
          Compiler.currentToken = currentToken.toString
        }
        else
          println("Lexical Error: " + Compiler.currentToken + " was not a legitimate token.")

        if (nextChar == '[')
          getChar()

      }
      case '#' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
        getChar()
      }
      case '*' => {
        addChar()
        getChar()
        if (nextChar == '*') {
          addChar()
          lookup(currentToken)
          Compiler.currentToken = currentToken.toString
          getChar()
        }
        else {
          lookup(currentToken)
          Compiler.currentToken = currentToken.toString
        }
      }
      case '+' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
        getChar()
      }
      case '[' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
        getChar()
      }
      case '=' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
        getChar()
      }
      case '!' => {
        addChar()
        getChar()
        if (nextChar == '[')
        {
          addChar()
          getChar()
        }

        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
      }
      case '(' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
        getChar()
      }
      case ')' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken.toString
        getChar()
      }
      case ']' => {
        addChar()
        lookup(currentToken)
        Compiler.currentToken = currentToken
        getChar()
      }

    }
  }
}
