package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : Compiler
//

import scala.io.Source

object Compiler {

  var currentToken : String = ""
  var fileContents : String = ""
  var fileName: String = ""
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer
  var end: Boolean = false


  def main(args : Array[String]) : Unit = {  // Main method for Compiler.scala

      checkfile(args)
      readfile(args(0))

      println("File starting...")
      println(fileContents)
      println()


      Scanner.getNextToken()
      Parser.gittex()
      SemanticAnalyzer.semantics()



  }  // End of main()

  def readfile(file : String) = {  // Changes file text to string
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkfile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: Wrong number of arguments......")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println("USAGE ERROR: Wrong extension type........")
      System.exit(1)
    }
  }
}  // End of Compiler.scala