package edu.arizona.sista.mc.tessellatedgraph.Clauseinator

import edu.arizona.sista.mc.focusword.QuestionDecomposerAPI
import edu.arizona.sista.mc.tessellatedgraph.structures.{ClauseLink, Concept, Schema}
import org.clulab.processors.Processor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Lexicon
import org.clulab.utils.StringUtils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import java.io.PrintWriter

/**
  * Created by rebeccas on 3/28/17.
  */
object MakeGraphletTuples {

  val processor:Processor = new FastNLPProcessor(withDiscourse = 0)
  var lexicon = new Lexicon[String]
  val TUPLE_FIELD_DELIM = "<>"

  def clauseinateAndSaveAnswerCandidates(line: String, pw: PrintWriter, questionAnswerIndex: Map[String, Seq[String]]): Unit = {
    // Extract the KB tuples from the string, keep the rest
    val fields = line.trim.split("\t")
    //    val kept = fields.slice(0,3)
    val keptQuestionAndIndex = fields.slice(0,2) // Keep 0 and 1
    val keptBackgroundAndLabel = fields.slice(2,fields.length) // keep background and label

    // Map the question text form this line to the FillInTheBlank sentences for its answer candudidates
    val questionText = fields(1).trim
    val answerSentences = questionAnswerIndex.get(questionText)
    if (answerSentences.nonEmpty) {
      // One of the questions is missing from the map...
//      val answerTuples = ""
      val answerTuples = answerSentences.get
          // Convert to CLInputSentences
          .map(sentence => Clauseinator.stringToCLInputSentence(sentence, "ARISTO_FITB"))
          .map(tup => tup._1)
          // Clauseinate each sentence
          .map(seqCLInputs => Clauseinator.extract(seqCLInputs.toArray, verbose = false))
          // Convert to tuples
          .map(seqGraphlets => seqGraphlets.flatMap(graphlet => graphletToTuples(graphlet, useEdgeLabel = true)).toSeq)
          // Convert each answer candidates's set of tuples to a single output string
          .map(seqTuples => tuplesToOutputString(seqTuples))
          // Join together to make one string
          .mkString("###")

      //    println("****************DEBUG*****************")
      //    println("Question: " + questionText)
      //    println("Answer Sentencess: \n\t" + answerSentences.get.mkString("\n\t"))
      //    println("Answer Tuples: \n" + answerTuples)
      //    println("")


      // append to the kept portion of the line
      val newVersionLine = ((keptQuestionAndIndex ++ Array(answerTuples)) ++ keptBackgroundAndLabel).mkString("\t")

      // write the new version of the line
      pw.println(newVersionLine)
      pw.flush()
    }


  }

  def tuplesToOutputString(tuples: Seq[(String, String, String)]): String = {
    val tuplesAsStrings = tuples.map(tup => s"${tup._1}$TUPLE_FIELD_DELIM${tup._2}$TUPLE_FIELD_DELIM${tup._3}")
    tuplesAsStrings.mkString("$$$")
  }


  def clauseinateAndSaveBackground(line: String, pw: PrintWriter): Unit = {
    // Extract the KB tuples from the string, keep the rest
    val fields = line.trim.split("\t")
//    val kept = fields.slice(0,3)
    val kept = fields.slice(0,2)
    val label = fields(4)
    val background = fields(3)
    val backgroundTuples = background.split("\\$\\$\\$")

    // From each tuple, get the background sentences (as a set)
    val backgroundSentences = backgroundTuples
        .map(tupleString => contextSentenceFromTupleString(tupleString))
        .toSet
    println(s"There were ${backgroundSentences.size} background sentences from this line.")


    // clauseinate each sentence
    var docid: Int = 1
    val clInputSentences = new ArrayBuffer[CLInputSentence]
    for (sentence <- backgroundSentences) {
      val (inputSentences, docidAfter) = Clauseinator.stringToCLInputSentence(sentence, "ARISTO_BKGRD", docid)
      docid = docidAfter
      clInputSentences.appendAll(inputSentences)
    }
    val schemas = Clauseinator.extract(clInputSentences.toArray, verbose = false)

    // tupleize each graphlet
    val tuples = schemas.map(graphletToTuples(_, useEdgeLabel = true))

//    println ("****************DEBUG*****************")
//    for (i <- 0 until math.min(10, schemas.length)) {
//      println (s"Schema/graphlet $i: " + schemas(i).toString)
//      println (s"Tuples from this schema: ")
//      tuples(i).foreach(tuple_guy => println("\t" + tuple_guy))
//      println("")
//    }

    // rejoin these new tuples
    val tuplesAsString = tuples
        .flatten
        .map(tup => s"${tup._1}$TUPLE_FIELD_DELIM${tup._2}$TUPLE_FIELD_DELIM${tup._3}")
        .mkString("$$$")

    // append to the kept portion of the line
    val newVersionLine = ((kept ++ Array(tuplesAsString)) ++ Array(label)).mkString("\t")

    // write the new version of the line
    pw.println(newVersionLine)
    pw.flush()
  }

  def contextSentenceFromTupleString(tupleString: String): String = {
    tupleString.split("context:")(1).split("<>")(0).trim.toLowerCase
  }

  // Extracts a tuple from each edge in a graphlet
  def graphletToTuples(graphlet: Schema, useEdgeLabel: Boolean = true): Seq[(String, String, String)] = {
    val tuplesOut = new ArrayBuffer[(String, String, String)]
    for (link <- graphlet.links.flatten) {
      val source = link.source
          .concepts
          .flatMap(concept => concept._1
              .terms
              .flatMap(term => term.lemmas))
          .toSet
          .mkString(" ")
      val destination = link.destination
          .concepts
          .flatMap(concept => concept._1
              .terms
              .flatMap(term => term.lemmas))
          .toSet
          .mkString(" ")
      val edgeLabel = if (useEdgeLabel && link.label != ClauseLink.CL_UNKNOWN) link.label else ""
      tuplesOut.append((source, edgeLabel, destination))
    }

    tuplesOut
  }

  def loadQuestionAnswerIndex (filename: String): Map[String, Seq[String]] = {
    val index = new mutable.HashMap[String, Seq[String]]()
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines().toArray
    for {
      line <- lines
      fields = line.split("\t")
      question = fields(0).trim
      answers = fields.slice(1, fields.length).toSeq.map(_.trim)
    } index.put(question, answers)
    source.close()
    index.toMap
  }

  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)

    // Initialize question decomposer API
    QuestionDecomposerAPI.initialize(props)

    val fold = "dev"
    val inputFilename = s"/Users/rebeccas/data/brittleness/omni4_${fold}_brittleness.tuples.q.labeled.TAG"
    val outputFilename = s"/Users/rebeccas/data/brittleness/omni4_${fold}_brittleness.tuples.q.labeled.TAG3"
    val pw = new PrintWriter(outputFilename)

//    val answerSentenceFile = "/Users/rebeccas/data/brittleness/questionSentences.txt"
    val answerSentenceFile = "/Users/rebeccas/data/brittleness/questionSentencesDev.txt"
    val questionAnswerIndex = loadQuestionAnswerIndex(answerSentenceFile)
    println("The question-answers index has " + questionAnswerIndex.size + " questions in it.")

    val source = scala.io.Source.fromFile(inputFilename)
    val lines = source.getLines().toArray
    for (lineIndex <- lines.indices) {
      if (lineIndex % 10 == 0) println (s"Processing line $lineIndex of ${lines.length}")
      // Clausinate, extract tuples, save
//      clauseinateAndSaveBackground(lines(lineIndex), pw)
      clauseinateAndSaveAnswerCandidates(lines(lineIndex), pw, questionAnswerIndex)

    }

    source.close()
    pw.close()
  }

}
