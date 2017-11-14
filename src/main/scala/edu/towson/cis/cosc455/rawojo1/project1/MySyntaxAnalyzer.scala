package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : MySyntaxAnalyzer
//
//

import edu.towson.cis.cosc455.rawojo1.project1.CONSTANTS._

import scala.collection.mutable.Stack

/*The Syntax Analyzer goes through the grammar and explains the structure that it is supposed to take on so that
* so that we do not get errors of Lexem misusage. The structure can be changed but the deliverable made them clear!*/

class MySyntaxAnalyzer extends SyntaxAnalyzer{
  var found: Boolean = false;

  var parse = List("")

   //Called by Compiler , Uses ArrayList
  def start(): List[String] =
  {
    parse = parse.reverse.drop(1)
    parse
  }

  //droping one
  def chachaSlide() : List[String] =
  {
    parse = parse.drop(1)
    parse
  }

  override def gittex(): Unit =
  {
    docb()
    variableDefine()
    title()
    body()
    doce()

    if (Compiler.Scanner.sou.length > 2)
    {
      println("Syntax Error: there's something after the '\\END' token and there should not be.")
      System.exit(1)
    }
  }


  def docb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\BEGIN'")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB))
    {
      defb()
      reqtext()
      eqsign()
      reqtext()
      brackete()
      variableDefine()
    }
  }


  override def title(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB))
    {
      parse = Compiler.currentToken :: parse

      Compiler.Scanner.getNextToken()
      reqtext()
      brackete()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting'\\TITLE[' tag")
      System.exit(1)
    }
  }

 //Ri¢hie was here :)
  override def body(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else if (Compiler.currentToken == CONSTANTS.USEB || Compiler.currentToken == CONSTANTS.HEADING ||
      Compiler.currentToken == CONSTANTS.BOLD || Compiler.currentToken == CONSTANTS.LISTITEM || Compiler.currentToken == CONSTANTS.IMAGEB ||
      Compiler.currentToken == CONSTANTS.LINKB || Compiler.currentToken == CONSTANTS.NEWLINE || Compiler.isText)
    {
      innerText()
      body()
    }
  }


  def doce(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE))
    {
      Compiler.checkFin = true
      parse = Compiler.currentToken :: parse
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\END'")
      System.exit(1)
    }
  }

  def defb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\DEF['")
      System.exit(1)
    }
  }


  def parab(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\parab'")
      System.exit(1)
    }
  }


  def linkb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '['")
      System.exit(1)
    }
  }


  def addressb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '('")
      System.exit(1)
    }
  }


  def imageb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '!['")
      System.exit(1)
    }
  }


  def useb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\USE['")
      System.exit(1)
    }
  }


  def headingb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '#'")
      System.exit(1)
    }
  }


  def listitemb(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '+'")
      System.exit(1)
    }
  }


  def boldh(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '*'")
      System.exit(1)
    }
  }

  override def paragraph(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB))
    {
      parab()
      variableDefine()
      innerText()
      pare()
    }
  }

  override def innerItem(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
    {
      bold()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
    {
      link()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      variableUse()
    }
    else if (Compiler.isText)
    {
      reqtext()
    }
  }


  override def innerText(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING))
    {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
    {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM))
    {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
    {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
    {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))
    {
      newline()
      innerText()
    }
    else if (Compiler.isText)
    {
      reqtext()
      innerText()
    }
  }


  override def link(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
    {
      linkb()
      reqtext()
      brackete()
      addressb()
      reqtext()
      addresse()
    }
  }



  override def bold(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
    {
      boldh()
      reqtext()
      boldh()
    }
  }


  override def newline(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\\\'")
      System.exit(1)
    }
  }


  override def image(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
    {
      imageb()
      reqtext()
      brackete()
      addressb()
      reqtext()
      addresse()
    }
  }


  override def variableUse(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      useb()
      reqtext()
      brackete()
    }
  }


  override def heading(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING))
    {
      headingb()
      reqtext()
    }
  }


  override def listItem(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM))
    {
      listitemb()
      innerItem()
      listItem()
    }
  }

  def reqtext(): Unit =
  {
    if (Compiler.isText)
    {
      parse = Compiler.currentToken :: parse
      Compiler.isText = false
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting REQTEXT")
      System.exit(1)
    }
  }


  def eqsign(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '='")
      System.exit(1)
    }
  }

  def brackete(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting ']'")
      System.exit(1)
    }
  }

  def addresse(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting ')'")
      System.exit(1)
    }
  }


  def pare(): Unit =
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE))
    {
      parse = Compiler.currentToken :: parse
      Compiler.Scanner.getNextToken()
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken + " was found when expecting '\\PARE'")
      System.exit(1)
    }
  }
}
