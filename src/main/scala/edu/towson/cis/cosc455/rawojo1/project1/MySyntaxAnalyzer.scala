package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : MySyntaxAnalyzer
//
//

import edu.towson.cis.cosc455.rawojo1.project1.CONSTANTS._

import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{
  var parse = Stack[String]()
  var isText: Boolean = false

  override def gittex(): Unit = {
    println(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      println(Compiler.currentToken)
      title()
      //body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){

      }
    }
    else {
      println("Error in Start state")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parse.push(CONSTANTS.PARAB)
      variableDefine()
      innerText()
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parse.push(CONSTANTS.PARAE)
        Compiler.Scanner.getNextToken()
      } else {
        println("Syntax Error - Missing paragraph ending")
        System.exit(1)
      }
    } else {
      println("Syntax Error - Missing paragraph beginning")
      System.exit(1)
    }
  }

  override def innerItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerItem()
    }else {
      //reqText()
      innerItem()
    }
  }

  override def innerText(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerText()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      heading()
      innerText()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerText()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      listItem()
      innerText()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      image()
      innerText()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerText()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      newline()
      innerText()
    }else{
      //text()
      innerText()
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      parse.push(CONSTANTS.LINKB)
      Compiler.Scanner.getNextToken()
      //reqText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parse.push(CONSTANTS.ADDRESSB)
          Compiler.Scanner.getNextToken()
          //reqText()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parse.push(CONSTANTS.ADDRESSE)
            Compiler.Scanner.getNextToken()
          }else{
            println("Syntax Error - Missing Address ending")
            System.exit(1)
          }
        }else{
          println("Syntax Error - Missing Address beginning")
          System.exit(1)
        }
      }else{
        println("Syntax Error - Missing ending Bracket in Link")
        System.exit(1)
      }
    }else{
      println("Syntax Error - Not a proper link beginning")
      System.exit(1)
    }
  }

  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(PARAB) && !isText) {
      paragraph()
      body()
    } else if (Compiler.currentToken.equalsIgnoreCase(NEWLINE) && !isText) {
      newline()
      body()
    } else if (Compiler.currentToken.equalsIgnoreCase(DOCE) && !isText) {
    } else {
      innerText()
      body()
    }
  }  // End of body()

  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parse.push(CONSTANTS.BOLD)
      Compiler.Scanner.getNextToken()
      //text()
    }else{
      println("Syntax Error - Missing Bold indicator")
      System.exit(1)
    }

  }

  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      parse.push(CONSTANTS.NEWLINE)
      Compiler.Scanner.getNextToken()
    }else{
      println("Syntax Error - Missing Newline")
      System.exit(1)
    }
  }

  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      parse.push(CONSTANTS.TITLEB)
      Compiler.Scanner.getNextToken()
      //text
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error - Missing end bracket for Title")
        System.exit(1)
      }
    }else {
      println("Syntax Error - Missing Title Beginning")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parse.push(CONSTANTS.DEFB)
      Compiler.Scanner.getNextToken()
      //reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        parse.push(CONSTANTS.EQSIGN)
        Compiler.Scanner.getNextToken()
        //reqText()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parse.push(CONSTANTS.BRACKETE)
          Compiler.Scanner.getNextToken()
        }else{
          println("Syntax Error - Missing ending Bracket in variable definition")
          System.exit(1)
        }
      }else{
        println("Syntax Error - Missing Equals sign for variable definition")
        System.exit(1)
      }
    }else{
      println("Syntax Error - Missing variable definition beginning declaration")
      System.exit(1)
    }
    variableDefine()
  }

  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      parse.push(CONSTANTS.IMAGEB)
      Compiler.Scanner.getNextToken()
      //reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
          parse.push(CONSTANTS.ADDRESSB)
          Compiler.Scanner.getNextToken()
          //reqText()
          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
            parse.push(CONSTANTS.ADDRESSE)
            Compiler.Scanner.getNextToken()
          }else{
            println("Syntax Error - Missing address ending in image declaration")
            System.exit(1)
          }
        }else{
          println("Syntax Error - Missing address beginning in image declaration")
          System.exit(1)
        }
      }else{
        println("Syntax Error - Missing bracket ending in image declaration")
        System.exit(1)
      }
    }else{
      println("Syntax Error - Missing image beginning declaration")
      System.exit(1)
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parse.push(CONSTANTS.USEB)
      Compiler.Scanner.getNextToken()
      //reqText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error - Missing end bracket in variable usage")
        System.exit(1)
      }
    }else{
      println("Syntax Error - Missing Use variable declaration")
      System.exit(1)
    }
  }


  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parse.push(CONSTANTS.HEADING)
      Compiler.Scanner.getNextToken()
      //reqText()
    } else {
      println("Syntax Error - Missing header character ")
      System.exit(1)
    }

  }

  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      parse.push(CONSTANTS.LISTITEM)
      Compiler.Scanner.getNextToken()
      innerItem()
      listItem()
    }else{
      println("Syntax Error - Missing + for List item")
      System.exit(1)
    }

  }
}
