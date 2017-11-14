package edu.towson.cis.cosc455.rawojo1.project1

//
// Richard Awojoodu
// COSC455
// Project 1 : MySemanticAnalyzer
//

import edu.towson.cis.cosc455.rawojo1.project1.CONSTANTS._

import scala.collection.mutable.Stack
import java.awt.Desktop
import java.io.{File, IOException}
import java.io._


class MySemanticAnalyzer {

  var counter         : Int = 0
  var markdown         = List("")
  var chosen          : String = ""
  var var_d           : String = ""
  var var_meaning     : String = ""
  var var2_d          : String = ""
  var var2_meaning    : String = ""
  var var3_d          : String = ""
  var var3_meaning    : String = ""
  var finale          : String = ""
  var file_name       : String = "MarkDown_AwojooduR_Project1.html"


  /*This will be the Compilers call to initialize the conversion process
  * Professors deliverable ask that we take the parse tree and convert it one by one into HTML code
  * */
  def startSem(): Unit = {
    chosen = Compiler.Parser.parse(0)
    toHTML(chosen)
  }


  def getnext(): Unit = {
    Compiler.Parser.chachaSlide()
    chosen = Compiler.Parser.parse(0)
  }

  /*Step by step the Gittex conversion happens as the predetermined CONSTANTS BNF is used to identify which html
    should be place in place of the Grammar.
  * */
  def toHTML(changeme : String) : Unit = {
    if (chosen.equalsIgnoreCase(CONSTANTS.DOCB)) {
      markdown = "<html>" :: markdown
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      getnext()
      markdown = "<head>" :: markdown
      markdown = "<title>" :: markdown
      markdown = chosen :: markdown
      markdown = "</title>" :: markdown
      markdown = "</head>" :: markdown
      getnext()
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.HEADING)) {
      getnext()
      markdown = "<h1>" :: markdown
      markdown = chosen :: markdown
      markdown = "</h1>" :: markdown
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.PARAB)) {
      markdown = "<p>" :: markdown
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.PARAE)) {
      markdown = "</p>" :: markdown
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.BOLD)) {
      getnext()
      markdown = "<b>" :: markdown
      markdown = chosen :: markdown
      markdown = "</b>" :: markdown
      getnext()
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      getnext()
      markdown = "<li>" :: markdown

      if (chosen.equalsIgnoreCase(CONSTANTS.USEB)) {
        getnext()
        var requested_var: String = chosen

        if (counter == 1) {
          if (requested_var == var_d) {
            markdown = var_meaning :: markdown
          }
          else {
            println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
            System.exit(1)
          }
        }
        else if (counter == 2) {
          if (requested_var == var2_d) {
            markdown = var2_meaning :: markdown
            counter = counter - 1
          }
          else {
            println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
            System.exit(1)
          }
        }
        else {
          if (requested_var == var_d) {
            markdown = var_meaning :: markdown
          }
          else if (requested_var == var2_d) {
            markdown = var2_meaning :: markdown
          }
          else if (requested_var == var3_d) {
            markdown = var3_meaning :: markdown
          }
          else {
            println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
            System.exit(1)
          }
        }
        getnext()
        getnext()
      }
      else {
        markdown = chosen :: markdown
        getnext()
      }

      if (chosen.equalsIgnoreCase(CONSTANTS.USEB)) {
        getnext()
        var requested_var: String = chosen

        if (counter == 1) {
          if (requested_var == var_d) {
            markdown = var_meaning :: markdown
          }
          else {
            println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
            System.exit(1)
          }
        }
        else if (counter == 2) {
          if (requested_var == var2_d) {
            markdown = var2_meaning :: markdown
            counter = counter - 1
          }
          else {
            println("Semantic error: the variable '" + requested_var + "' was never properly defined.")
            System.exit(1)
          }
        }
        else {
          if (requested_var == var_d) {
            markdown = var_meaning :: markdown
          }
          else if (requested_var == var2_d) {
            markdown = var2_meaning :: markdown
          }
          else if (requested_var == var3_d) {
            markdown = var3_meaning :: markdown
          }
          else {
            println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
            System.exit(1)
          }
        }
        getnext()
        getnext()
      }
      markdown = "</li>" :: markdown
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      markdown = "<br>" :: markdown
      getnext()
      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.LINKB)) {
      getnext()
      val gettem: String = chosen

      getnext()
      getnext()
      getnext()

      val linktag: String = "<a href = \"" + chosen + "\">"
      markdown = linktag :: markdown
      markdown = gettem :: markdown
      markdown = "</a> " :: markdown
      getnext() // gets ')'
      getnext() // gets next real token

      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      getnext()

      val gettem: String = chosen + "\">"
      getnext() // gets ']'
      getnext() // gets '('
      getnext() // gets link

      val linktag: String = "<img src =\"" + chosen + "\" alt=\""
      markdown = linktag :: markdown
      markdown = gettem :: markdown
      getnext() // gets ')'
      getnext() // gets next real token

      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.DEFB)) {
      counter = counter + 1
      getnext()

      if (counter == 1) {
        var_d = chosen
        var_d = var_d.dropRight(1)
        getnext() // gets '='
        getnext() // gets var_ds meaning
        var_meaning = chosen + " "
      }
      else if (counter == 2) {
        var2_d = chosen
        var2_d = var2_d.dropRight(1)
        getnext() // gets '='
        getnext() // gets var_ds meaning
        var2_meaning = chosen + " "
      }
      else if (counter == 3) {
        var3_d = chosen
        var3_d = var3_d.dropRight(1)
        getnext() // gets '='
        getnext() // gets var_ds meaning
        var3_meaning = chosen + " "
      }

      getnext() // gets ']'
      getnext() // gets next real token

      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.USEB)) {
      getnext()
      var requested_var: String = chosen

      if (counter == 1) {
        if (requested_var == var_d) {
          markdown = var_meaning :: markdown
        }
        else {
          println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
          System.exit(1)
        }
      }
      else if (counter == 2) {
        if (requested_var == var2_d) {
          markdown = var2_meaning :: markdown
          counter = counter - 1
        }
        else {
          println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
          System.exit(1)
        }
      }
      else {
        if (requested_var == var_d) {
          markdown = var_meaning :: markdown
        }
        else if (requested_var == var2_d) {
          markdown = var2_meaning :: markdown
        }
        else if (requested_var == var3_d) {
          markdown = var3_meaning :: markdown
        }
        else {
          println("Semantic error: the variable '" + requested_var + "' was never defined in CONSTANTS.")
          System.exit(1)
        }
      }

      getnext()
      getnext()

      toHTML(chosen)
    }
    else if (chosen.equalsIgnoreCase(CONSTANTS.DOCE)) {
      markdown = "</html>" :: markdown
    }
    else {
      markdown = chosen :: markdown
      getnext()
      toHTML(chosen)
    }
  }

  //Finally we get to print out, into a new file, the HTML code produced
  def writeOut(): Unit = {
    markdown = markdown.reverse.drop(1)
    finale = markdown.mkString

    val HTML_Document = new PrintWriter(new File(file_name))

    HTML_Document.write(finale)
    HTML_Document.close
  }


}  // End of MySemanticAnalyzer.scala

