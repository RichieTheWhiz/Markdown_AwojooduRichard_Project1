package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : Compiler
//

import scala.io.Source
import java.nio.file.{Paths, Files}

object Compiler {

  var currentToken : String = ""
  var fileContents : String = ""
  var checkFin     : Boolean = false
  var isText : Boolean = false
  var tokens = List("")

  val Scanner           = new MyLexicalAnalyzer
  val Parser            = new MySyntaxAnalyzer
  val SemanticAnalyzer  = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    if (!Files.exists(Paths.get(args(0)))) {
      println("Error: " + args(0) + " does not exist.")
      System.exit(1)
    }

    checkFile(args)
    readFile(args(0))

    //Dumps the content of the file into the Lexical Analyzer to begin process.
    Scanner.start(fileContents)

    while (!checkFin) {
      //The Lex. Analyzer will go through one by one to make sure that there is appropriate usage of the predetermined Lexems
      Scanner.getNextToken()
      //The Syn. Analyzer will go through the Lexems that passed and see if they are used in the correct structure that was defined.
      Parser.gittex()
    }
      Parser.start()
      //Parse Tree created is dumped into the Semantic Analyzer to now be converted into HTML code. and then be written!
      SemanticAnalyzer.startSem()
      SemanticAnalyzer.writeOut()


  }


  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }
  //File Checking for Compiler process to begin.
  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of arguments TRY AGAIN.")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension bro, the file should be a .gtx file to begin.")
      System.exit(1)
    }
  }


}  // End of Compiler.scala