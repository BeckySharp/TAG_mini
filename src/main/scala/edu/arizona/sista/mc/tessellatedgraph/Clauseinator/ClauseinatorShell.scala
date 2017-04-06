package edu.arizona.sista.mc.tessellatedgraph.Clauseinator

import edu.arizona.sista.mc.focusword.QuestionDecomposerAPI
import org.clulab.utils.StringUtils

import scala.util.control.Breaks._

/**
 * Created by peter on 1/28/15.
 */


object ClausinatorShell {


  /*
   * Shell
   */

  def shell() = {
    println("Starting shell...")
    println("Please enter a sentence to clauseinate. ")

    var done: Boolean = false
    breakable {
      while (true) {
        // Read in focus words from user
        print("Sentence> ")
        val sentenceIn = readLine()

        // Exit if one of the query is empty
        if ((sentenceIn == null) || (sentenceIn.length <= 1)) break()

        // Clauseinate (Generate schema)
        val CLSent = new CLInputSentence("docid", sentenceIn, "SHELL")
        val schemas = Clauseinator.extract( Array(CLSent) )

        // Display schema as text
        val filenamePNG = "shellout.png"
        CLUtils.exportSchemasPNG( schemaIndices = Array(0),
          linksBetweenSchema = Array.empty[(Int, Int)],
          highlightTLemmas = Array.empty[Int],
          secondaryHighlight = Array.empty[Int],
          allSchemas = schemas,
          lexicon = Clauseinator.lexicon,
          filename = filenamePNG )

        println ("")

      }
    }

    println ("Empty query -- exiting shell. ")

  }




  def main(args:Array[String]): Unit = {
    //val props = StringUtils.argsToProperties(args)
    val props = StringUtils.argsToProperties(Array("-props", "src/main/resources/concreteness.props"))

    // Initialize question decomposer API
    QuestionDecomposerAPI.initialize(props)

    // Enter clausinator shell
    shell()

  }


}
