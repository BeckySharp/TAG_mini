package edu.arizona.sista.mc.focusword


import org.slf4j.LoggerFactory
import edu.arizona.sista.mc.concretenessnorms.ConcretenessNorms
import org.clulab.processors.Document
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.struct.Counter
import org.clulab.utils.StringUtils

import collection.mutable.ArrayBuffer

import java.util.Properties
//import edu.arizona.sista.mc.tessellatedgraph.Tessellator.NewTessellator
//import FocusSentence._

/**
 * API for the Focus Word Extractor subsystem
 * User: peter
 * Date: 11/17/14
 */
class QuestionDecomposerAPI {
}

object QuestionDecomposerAPI {
  val logger = LoggerFactory.getLogger(classOf[QuestionDecomposerAPI])
  val version:String = "0.2 (2014-12-17)"
  val FILENAME_DEFAULTPROPS = "qd.properties"
  val processor = new CoreNLPProcessor()

  var concretenessDB:Counter[String] = null
  var initialized:Boolean = false

  /*
   * API
   */

  /**
   * API: Answers a multiple choice question submitted in plain text format
   * @param text The text of the question to decompose (e.g. "Why does the sky appear blue?")
   * @return Returns an array of FocusSentence storage classes (one for each sentence) that contain the annotation
   *         for these sentences, including focus word tags, concreteness norms, focus word ranks, in addition to
   *         the other annotation from processors (lemmatizing, syntactic dependency parsing, named entity recognition,
   *         etc).
   */
  def analyze(text:String):Array[FocusSentence] = {
    var qdSentences = new ArrayBuffer[FocusSentence]

    if (!initialized) throw new RuntimeException(" * ERROR: initialize() must be called before using the QuestionDecomposerAPI. ")

    val annotation = processor.annotate( text )
    analyze(annotation)
  }

  def analyze(annotation:Document):Array[FocusSentence] = {
    var qdSentences = new ArrayBuffer[FocusSentence]

    for (sent <- annotation.sentences) {
      val fwSent = new FocusSentence(sent, concretenessDB)
      //val fwSent = new FocusSentence(sent, concretenessDB, isAnswerSentence = false, mode = BASELINE_UNIFORM)
      qdSentences.append(fwSent)
    }

    qdSentences.toArray
  }

  /**
   * API: Initialize the UA Focus Word Extractor system.  This function should be called first, before using the analyze() method.
   * @param filenameProps The full path to a properties filename specifying parameters for the Focus Word Extractor system
   *                      (e.g. qd.properties).  If no filename is specified, then the default parameters are used.
   */
  def initialize(filenameProps:String = "") {
    if (filenameProps.length < 1) {
      println ("Initializing SISTA QuestionDecomposerAPI using default properties file...")
      val props = StringUtils.argsToProperties(Array ("-props", FILENAME_DEFAULTPROPS))
      initialize(props)
    } else {
      println ("Initializing SISTA QuestionDecomposerAPI using specified properties file (" + filenameProps + ")...")
      val props = StringUtils.argsToProperties(Array ("-props", filenameProps))
      initialize(props)
    }
  }

  def initialize(props:Properties) {
    val filenameConcretenessNorms = props.getProperty("concretenessnorms", "")
    concretenessDB = ConcretenessNorms.loadConcretenessNormsDatabase(filenameConcretenessNorms)
    initialized = true
  }


  /*
   * Command-line interface
   */
  def shell() {
    var ok = true

    logger.info("Starting shell...")
    println ("UA-QuestionDecomposer: The University of Arizona NY Regent's Exam Question Decomposer (Alpha v" + version + ")")
    println ("")
    println ("Please enter a plain text question to decompose. Use empty query to exit. ")
    println ("Example format: In New York State, the longest period of daylight occurs during which month?")
    while(ok) {
      print("QD> ")
      val qText = readLine()
      if ((qText != null) && (qText.length > 1)) {

        // Decompose question
        val qdSentences: Array[FocusSentence] = analyze(qText)

        // Display results
        displayFocusSentences(qdSentences)



      } else {
        ok = false
      }
    }
  }

  /*
   * Displays the focus word decomposition of an array of sentences.  This simply calls the internal toString() method
   * of FocusSentence, which displays the sentence decomposition as an easy-to-read text table.
   */
  def displayFocusSentences(in:Array[FocusSentence]) {
    for (i <- 0 until in.size) {
      println("Decomposed Sentence " + i + ": ")
      println( in(i) )
      println("")
    }
  }

  /*
   * Prints the command line usage information to the console
   */
  def printUsage() {
    println ("UA-QuestionDecomposer: The University of Arizona NY Regent's Exam Question Decomposer (Alpha v" + version + ")")
    println ("")
    println ("To enter an interactive shell: ")
    println ("Usage: uaqd-commandline.sh")
    println ("")
  }

  /*
   * Main entry point for console use
   */
  def main(args:Array[String]) {
    // Step 1: Check that arguments were specified
    // e.g. "edu.arizona.sista.mc.focusword.QuestionDecomposerAPI -props qd.properties"
    if ((args.length == 0)) {
      printUsage()
      System.exit(1)
    }

    // Step 2: Initialize Question Decomposer API using properties file specified on command line
    println ("Initializing...")
    val props = StringUtils.argsToProperties(args)
    initialize(props)

    // Step 3: For non-API use, run using the interactive shell
    shell()
    println ("Exiting...")
  }

}