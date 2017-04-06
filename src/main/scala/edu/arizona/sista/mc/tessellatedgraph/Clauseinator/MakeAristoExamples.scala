package edu.arizona.sista.mc.tessellatedgraph.Clauseinator

import edu.arizona.sista.mc.focusword.QuestionDecomposerAPI
import edu.arizona.sista.mc.tessellatedgraph.structures.Schema
import org.clulab.struct.Lexicon
import org.clulab.utils.StringUtils

import scala.collection.mutable.ArrayBuffer

import java.io.PrintWriter

/**
  * Created by rebeccas on 4/3/17.
  */

case class QuestionExample(questionIndex: String, questionText: String, answerSentence: String, backgroundSentence: Seq[String])

object MakeAristoExamples {

  def loadQuestionExamples(filename: String): Seq[QuestionExample] = {
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines().toArray
    val out = new ArrayBuffer[QuestionExample]
    for (line <- lines) {
      println ("Processing: " + line)
      val fields = line.trim.split("\t")
      if (fields.length == 4) {
        val qIdx = fields(0)
        val qText = fields(1)
        val ansText = fields(2)
        val bkgrd = fields(3).split("###")
        out.append(QuestionExample(qIdx, qText, ansText, bkgrd))
      }
    }
    source.close()

    out
  }

  def exportExampleSchemasHTMLVis(
    question: QuestionExample,
    schemasAns:Array[Schema],
    lexiconAns:Lexicon[String],
    schemasBackground:Array[Schema],
    lexiconBackground:Lexicon[String],
    path:String = "output/"): Unit = {
    //val path = "output/"

    println("\n*********************")
    println(s"Doing html for question ${question.questionIndex}")
    println("*********************\n")


    println ("exportSchemasHTMLVis: Starting at 0 of " + schemasAns.size)
    val filenameHTML = path + "index" + question.questionIndex + ".html"
    val pw = new PrintWriter(filenameHTML)
    // Header
    pw.println("<html> <head> </head> <body> <center>")

    // Generate indices
    val indicesAns = schemasAns.indices.toArray
    val indicesBackground = schemasBackground.indices.toArray

    // Generate HTML and PNG files
    pw.println(s"Question [${question.questionIndex}]: <br>\n")
    pw.println (question.questionText + "\n")
    pw.println("<br> <br>\n")

    pw.println ("(Correct) Answer Sentences: <br>\n")
    val htmlAns = CLUtils.exportSchemasHTMLString(
      indicesAns,
      schemasAns,
      lexiconAns,
      path + "_" + question.questionIndex + "_ans_")
    pw.println(htmlAns)
    pw.println ("======================================================== <br> \n")

    pw.println ("Background Sentences: <br>\n")
    val htmlBackground = CLUtils.exportSchemasHTMLString(
      indicesBackground,
      schemasBackground,
      lexiconBackground,
      path + "_" + question.questionIndex + "_bg_")
    pw.println(htmlBackground)

    // Footer
    pw.println("</center> </body> </html>")

    pw.close()
  }



  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)

    // Initialize question decomposer API
    QuestionDecomposerAPI.initialize(props)

    // Load questions
    val inputFilename = "/Users/rebeccas/data/brittleness/tail100.omni4_train_brittleness.tuples.q.labeled.examples"
    val questions = loadQuestionExamples(inputFilename)

    val schemasStorage = "/Users/rebeccas/data/brittleness/schemas/"
    val pathOut = "/Users/rebeccas/data/brittleness/TAGExamples_tail100/"
    val pwIndex = new PrintWriter(pathOut + "index.html")
    pwIndex.println("<html> <head> </head> <body>")
    // For each question, load sentences for answer and background
    var sentenceIndex = 1
    var questionNumber = 0
    for (question <- questions) {
      // Convert to CLInputSentences
      val (answerCLInput, newSentIndex) = Clauseinator.stringToCLInputSentence(question.answerSentence, "ARISTO_FITB", sentenceIndex)
      sentenceIndex = newSentIndex
      val backgroundCLInput = new ArrayBuffer[CLInputSentence]
      for (bg <- question.backgroundSentence) {
        val (bgCLInput, newSentIndex2) = Clauseinator.stringToCLInputSentence(bg, "ARISTO_WATERLOO", sentenceIndex)
        sentenceIndex = newSentIndex2
        backgroundCLInput.appendAll(bgCLInput)
      }

      // Clausinate and save
      val verbose = false
      val filenameSchemasPrefix = schemasStorage + "schemas_omni4brittleness_examples_apr3_" + question.questionIndex
//      Clauseinator.clausinateAndSave(answerCLInput.toArray, filenameSchemasPrefix + ".answer", verbose)
//      Clauseinator.clausinateAndSave(backgroundCLInput.toArray, filenameSchemasPrefix + ".background", verbose)

      // Generate the html file
      // Step 3: Export to HTML
      val quote = "\""
      val (answerSchemas, answerLexicon) = Clauseinator.loadSchemas(filenameSchemasPrefix + ".answer")
      val (backgroundSchemas, backgroundLexicon) = Clauseinator.loadSchemas(filenameSchemasPrefix + ".background")

      val extraInfo = s"${answerSchemas.length} answer sentences, ${backgroundSchemas.length} background sentences"
      val link = s"<a href=${quote}index${question.questionIndex}.html${quote}>"
      pwIndex.println(s"${link}Question $questionNumber </a> ($extraInfo) <br>${question.questionText}<br><br>")

      exportExampleSchemasHTMLVis(question, answerSchemas, answerLexicon, backgroundSchemas, backgroundLexicon, pathOut)
      questionNumber += 1
    }
    pwIndex.println("</body></html>")
    pwIndex.close()


  }
}
