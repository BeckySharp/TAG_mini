package edu.arizona.sista.mc.tessellatedgraph.Clauseinator

import java.io.PrintWriter

import edu.arizona.sista.mc.focusword.{QuestionDecomposerAPI, FocusSentence}
import edu.arizona.sista.mc.tessellatedgraph.structures._
import org.clulab.processors.{Processor, Document}
import org.clulab.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.qa.linearalg.SparseMatrix
import org.clulab.struct.{Counter, Lexicon}
import scala.sys.process._
import scala.util.control.Breaks._
import FocusSentence.{FOCUS_NORMAL, BASELINE_IDF, BASELINE_UNIFORM}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by peter on 1/26/15.
 */

object CLUtils {
  /*
   * Supporting functions (Array helpers)
   */

  // Removes the duplicates from an array of integers
  def removeDuplicates(in:Array[Int]):Array[Int] = {
    val out = new ArrayBuffer[Int]()
    for (i <- 0 until in.size) {
      var found:Boolean = false
      for (j <- 0 until out.size) {
        if (in(i) == out(j)) found = true
      }

      if (!found) out.append( in(i) )
    }
    out.toArray
  }

  def findIntersection(in1:Array[Int], in2:Array[Int]):Array[Int] = {
    val out = new ArrayBuffer[Int]
    for (a <- in1) {
      for (b <- in2) {
        if (a == b) {
          out.append( a )
        }
      }
    }
    out.toArray
  }

  def findFirstOccurrenceInt(in:Array[Int], searchFor:Int):Int = {
    for (i <- 0 until in.size) {
      if (in(i) == searchFor) return i
    }
    return -1
  }

  def removeDuplicates(in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]()
    for (i <- 0 until in.size) {
      var found:Boolean = false
      for (j <- 0 until out.size) {
        if (in(i) == out(j)) found = true
      }

      if (!found) out.append( in(i) )
    }
    out.toArray
  }

  def removeOccurrences(in:Array[Int], removeThese:Array[Int]):Array[Int] = {
    val out = new ArrayBuffer[Int]()
    for (i <- 0 until in.size) {
      breakable {
        for (j <- 0 until removeThese.size) {
          if (in(i) == removeThese(j)) break()
        }
        // Not found
        out.append( in(i) )
      }
    }
    out.toArray
  }


  /*
   * Supporting functions (Tagged/Grouped lemmas)
   */

  def groupPOSTag(in:String):String = {
    if (in.length > 2) {
      return in.slice(0, 2)
    }
    in
  }

  // Makes a string containing a tagged-grouped lemma, of the form "NN_tree" or "VB_eat".
  def mkTLemma(tag:String, lemma:String):String = {
    groupPOSTag(tag) + "_" + lemma.toLowerCase()
  }

  // Extracts the first part of strings of the format "tag_word"
  def getPOSTag(in:String):String = {
    val split = in.split("_")
    if (split.size == 2) {
      return split(0)
    }
    ""
  }

  // Returns true if a given word has a content POS tag (NN*, VB*, JJ*, RB*)
  def hasContentTag(fSent:FocusSentence, idx:Int):Boolean = {
    val contentTags = Array("NN", "VB", "JJ", "RB", "RP")
    val queryTag = fSent.tags.get(idx)
    for (contentTag <- contentTags) {
      if (queryTag.startsWith(contentTag)) {
        return true
      }
    }
    false
  }

  // Returns all tLemma lexicon indices in a given schema
  def getAllTLemmas(in:Schema):Array[Int] = {
    val out = new ArrayBuffer[Int]

    for (clause <- in.clauses) {
      for (conceptRolePair <- clause.concepts) {
        val concept = conceptRolePair._1
        val role = conceptRolePair._2
        for (term <- concept.terms) {
          for (tLemma <- term.lexiconIDs) {
            out.append(tLemma)
          }
        }
      }
    }

    // Return
    removeDuplicates(out.toArray)
  }

  // Make a string of tagged lemmas from an array of lexicon IDs
  def mkTLemmaString(in:Array[Int], lexicon:Lexicon[String]):String = {
    val os = new StringBuilder
    for (tLemmaIdx <- in) {
      os.append(lexicon.get(tLemmaIdx))
      os.append(" ")
    }
    os.toString().trim
  }

  // Make a string of tagged lemmas with their assocated focus weights from a list of (tLemma, weight) pairs
  def mkTLemmaString(in:Array[(Int, Double)], lexicon:Lexicon[String]):String = {
    val os = new StringBuilder
    for (focusWord <- in) {
      val tLemmaIdx = focusWord._1
      val rank:Double = focusWord._2
      val lemma = lexicon.get(tLemmaIdx)
      os.append("(" + lemma + ", " + rank.formatted("%3.2f") + ") " )
    }
    os.toString().trim
  }

  /*
   * Supporting functions (Focus Sentences)
   */
  val MODE_FIRSTSENTENCE = 1
  val MODE_LASTSENTENCE = 2
  val MODE_LAST2SENTENCES = 3
  val MODE_ALLSENTENCES = 4
  def getFocusSentences(text:String, processor:CoreNLPProcessor, sentMode:Int = MODE_ALLSENTENCES, focusMode:Int = FOCUS_NORMAL):Array[FocusSentence] = {
    val annotation = processor.annotate(text)
    val out = new ArrayBuffer[FocusSentence]

    if (sentMode == MODE_FIRSTSENTENCE) {
      out.append(new FocusSentence(annotation.sentences(0), QuestionDecomposerAPI.concretenessDB, isAnswerSentence = false, mode = focusMode))
    } else if (sentMode == MODE_LASTSENTENCE) {
      out.append(new FocusSentence(annotation.sentences(annotation.sentences.size - 1), QuestionDecomposerAPI.concretenessDB, isAnswerSentence = false, mode = focusMode))

    } else if (sentMode == MODE_LAST2SENTENCES) {
      out.append(new FocusSentence(annotation.sentences(annotation.sentences.size - 1), QuestionDecomposerAPI.concretenessDB, isAnswerSentence = false, mode = focusMode))
      if ((annotation.sentences.size - 2) > 0) {
        out.append(new FocusSentence(annotation.sentences(annotation.sentences.size - 2), QuestionDecomposerAPI.concretenessDB, isAnswerSentence = false, mode = focusMode))
      }
    } else {
      // MODE_ALLSENTENCES (Default -- All sentences)
      for (sent <- annotation.sentences) {
        out.append( new FocusSentence(sent, QuestionDecomposerAPI.concretenessDB, isAnswerSentence = false, mode = focusMode) )
      }
    }

    out.toArray
  }

  def setFocusSentenceMode(sentMode:String):Int = {
    if (sentMode.toLowerCase == "allsentences")     return MODE_ALLSENTENCES
    if (sentMode.toLowerCase == "lastsentence")     return MODE_LASTSENTENCE
    if (sentMode.toLowerCase == "lasttwosentences") return MODE_LAST2SENTENCES
    if (sentMode.toLowerCase == "firstsentence")    return MODE_FIRSTSENTENCE

    // Otherwise
    throw new RuntimeException(" WARNING: Focus sentence mode (" + sentMode + ") not recognized. ")
  }

  def setFocusExtractionMode(focusMode:String):Int = {
    if (focusMode.toLowerCase == "baselineuniform")     return BASELINE_UNIFORM
    if (focusMode.toLowerCase == "baselineidf")         return BASELINE_IDF
    if (focusMode.toLowerCase == "normal")              return FOCUS_NORMAL

    // Otherwise
    throw new RuntimeException(" WARNING: Focus sentence extraction mode (" + focusMode + ") not recognized. ")
  }


  def getDepLabel(fSent:FocusSentence, idxFrom:Int, idxTo:Int):String = {
    val outgoingEdges = fSent.dependencies.get.outgoingEdges
    val UNKNOWN_DEP_LABEL:String = "UNKNOWN_DEP_LABEL"

    // Bound checking
    if (idxFrom > outgoingEdges.size) {
      return UNKNOWN_DEP_LABEL
    }

    // Find dependency (if it exists)
    for (e <- outgoingEdges(idxFrom)) {
      val depToIdx = e._1
      val depLabel = e._2
      if (depToIdx == idxTo) {
        return depLabel
      }
    }

    // Not found: return empty string
    UNKNOWN_DEP_LABEL
  }

  def getFocusListElems(in:Array[FocusSentence], lexicon:Lexicon[String]):Array[(Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int)]
    for (fsent <- in) {
      val focus = fsent.focusListElems
      for (i <- 0 until focus.size) {
        val wordIndex = focus(i)._2
        val lemma = fsent.lemmas.get(wordIndex)
        val tag = fsent.tags.get(wordIndex)
        val rank = fsent.ranks(wordIndex)
        val tLemmaIdx = lexicon.add( mkTLemma(tag, lemma) )
        out.append( (tLemmaIdx, rank) )
      }
    }
    out.toArray
  }

  def getFocusPrimary(in:Array[FocusSentence], lexicon:Lexicon[String]):Array[(Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int)]
    for (fsent <- in) {
      val focus = fsent.focusPrimary
      for (i <- 0 until focus.size) {
        val wordIndex = focus(i)._2
        val lemma = fsent.lemmas.get(wordIndex)
        val tag = fsent.tags.get(wordIndex)
        val rank = fsent.ranks(wordIndex)
        val tLemmaIdx = lexicon.add( mkTLemma(tag, lemma) )
        out.append( (tLemmaIdx, rank) )
      }
    }
    out.toArray
  }

  def getFocusBackoff(in:Array[FocusSentence], lexicon:Lexicon[String]):Array[(Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int)]
    for (fsent <- in) {
      val focus = fsent.focusBackoff
      for (i <- 0 until focus.size) {
        val wordIndex = focus(i)._2
        val lemma = fsent.lemmas.get(wordIndex)
        val tag = fsent.tags.get(wordIndex)
        val rank = fsent.ranks(wordIndex)
        val tLemmaIdx = lexicon.add( mkTLemma(tag, lemma) )
        out.append( (tLemmaIdx, rank) )
      }
    }
    out.toArray
  }

  def getFocusAnswerType(in:Array[FocusSentence], lexicon:Lexicon[String]):Array[(Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int)]
    for (fsent <- in) {
      val focus = fsent.focusAnswerType
      for (i <- 0 until focus.size) {
        val wordIndex = focus(i)._2
        val lemma = fsent.lemmas.get(wordIndex)
        val tag = fsent.tags.get(wordIndex)
        val rank = fsent.ranks(wordIndex)
        val tLemmaIdx = lexicon.add( mkTLemma(tag, lemma) )
        out.append( (tLemmaIdx, rank) )
      }
    }
    out.toArray
  }


  // Normalize focus words such that the sum of all the ranks is 1
  def normalizeFocusRanks(in:Array[(Int, Int)]):Array[(Int, Double)] = {
    val out = new Array[(Int, Double)](in.size)
    var sum:Double = 0
    // Sum
    for (i <- 0 until in.size) {
      sum += in(i)._2
    }
    // Normalize
    for (i <- 0 until in.size) {
      out(i) = (in(i)._1, in(i)._2.toDouble / sum)
    }
    out
  }

  // Normalize focus word ranks for both question and answer focus terms.
  def normalizeFocusRanks(inQ:Array[(Int, Int)], inA:Array[Array[(Int, Int)]]):(Array[(Int, Double)], Array[Array[(Int, Double)]]) = {
    val outQ = normalizeFocusRanks(inQ)
    val outA = new Array[Array[(Int, Double)]](inA.size)
    for (i <- 0 until inA.size) {
      outA(i) = normalizeFocusRanks(inA(i))
    }
    (outQ, outA)
  }


  // Remove focus ranks (int) (rarely used)
  def stripRanksInt(in:Array[(Int, Int)]):Array[Int] = {
    val out = new Array[Int](in.size)
    for (i <- 0 until in.size) {
      out(i) = in(i)._1
    }
    out
  }

  // Remove focus ranks (double)
  def stripRanks(in:Array[(Int, Double)]):Array[Int] = {
    val out = new Array[Int](in.size)
    for (i <- 0 until in.size) {
      out(i) = in(i)._1
    }
    out
  }


  /*
   * Supporting functions: Focus words: Extracting focus words from a question
   */



  def extractFocusFromText(text:String, processor:CoreNLPProcessor, lexicon:Lexicon[String], sentMode:Int = MODE_ALLSENTENCES, focusMode:Int = FOCUS_NORMAL):Array[(Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int)]
    val focusSentences = CLUtils.getFocusSentences(text, processor, sentMode, focusMode)

    // Individual focus word categories
    val focusList = CLUtils.getFocusListElems(focusSentences, lexicon)
    val focusPrimary = CLUtils.getFocusPrimary(focusSentences, lexicon)
    val focusBackoff = CLUtils.getFocusBackoff(focusSentences, lexicon)
    val focusAnswerType = CLUtils.getFocusAnswerType(focusSentences, lexicon)

    // Assemble list
    out.insertAll(out.size, focusList)
    out.insertAll(out.size, focusPrimary)
    out.insertAll(out.size, focusBackoff)
    out.insertAll(out.size, focusAnswerType)

    // Return
    out.toArray
  }

  /*
   * Supporting functions (Schema lookup)
   */

  def findSchemasWithTLemma(tLemma:Int, wordSchemaLUT:SparseMatrix[Double]):Array[Int] = {
    val sparseRow = wordSchemaLUT.getRowVecSparse(tLemma)
    val out = new ArrayBuffer[Int]

    // This isn't terribly clean -- sparseVector should have a .toArray method
    val labels = sparseRow.labels
    val values = sparseRow.values
    for (i <- 0 until labels.size) {
      if (values(i) > 0) out.append(labels(i))
    }

    out.toArray
  }


  // Check whether a given tLemma is defined in a schema
  def isTLemmaDefined(tLemma:Int, schema:Schema):Boolean = {
    var defTypes = Array(Clause.CTYPE_DEF)                // Clause types to search for (here, just those that mark a definition word)

    for (clauseIdx <- 0 until schema.clauses.size) {
      val clause = schema.clauses(clauseIdx)

      // Defined word
      if (defTypes.contains( clause.clauseType )) {
        for (conceptIdx <- 0 until clause.concepts.size) {
          val concept = clause.concepts(conceptIdx)._1
          for (term <- concept.terms) {
            for (tLemmaID <- term.lexiconIDs) {
              if (tLemma == tLemmaID) return true
            }
          }
        }
      }

    }

    // Return
    false
  }

  /*
   * Supporting functions (Schema combination)
   */

  def combineSchemaArrays(in1:Array[Schema], lexicon:Lexicon[String], toCombine:Array[Schema]):(Array[Schema], Lexicon[String]) = {
    val out = new ArrayBuffer[Schema]

    // Step 1: Add existing set
    out.insertAll(0, in1)

    // Step 2: Add new schemas.  Update lexicon indicies with indices for existing lexicon
    for (newSchema <- toCombine) {
      newSchema.updateWithNewLexicon(lexicon)
      out.append(newSchema)
    }

    // Return
    (out.toArray, lexicon)
  }


  /*
   * Supporting functions (Schema)
   */
  // Find overlapping words between schema.  schemaIdx is an array of schema indicies.
  // Words must appear in at least two schemas to be included.
  def findOverlappingWords(schemaIdx:Array[Int], schemas:Array[Schema]):Array[Int] = {
    val out = new ArrayBuffer[Int]
    val tLemmasPerSchema = new Array[Array[Int]](schemaIdx.size)

    // Find all tLemmas in each schema
    for (i <- 0 until schemaIdx.size) {
      tLemmasPerSchema(i) = CLUtils.getAllTLemmas( schemas(schemaIdx(i)) )
    }

    // Find overlap
    for (i <- 0 until schemaIdx.size) {
      for (j <- 0 until schemaIdx.size) {
        if (i != j) {
          out.insertAll(out.size, CLUtils.findIntersection(tLemmasPerSchema(i), tLemmasPerSchema(j)) )
        }
      }
    }

    // Return
    CLUtils.removeDuplicates(out.toArray)
  }


    // Find all locations of a given tLemma in a schema
  def findTLemmaInSchema(tLemma:Int, schemaIdx:Int, schema:Schema):Array[(Int, Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int, Int)]    // (Schema, Clause, Concept)

    for (clauseIdx <- 0 until schema.clauses.size) {
      val clause = schema.clauses(clauseIdx)
      for (conceptIdx <- 0 until clause.concepts.size) {
        val conceptRolePair = clause.concepts(conceptIdx)
        val concept = conceptRolePair._1
        val role = conceptRolePair._2

        var found:Boolean = false
        for (term <- concept.terms) {
          for (tLemmaTerm <- term.lexiconIDs) {
            if (tLemmaTerm == tLemma) {
              found = true
            }
          }
        }

        if (found) {
          out.append((schemaIdx, clauseIdx, conceptIdx))
        }
      }
    }

    out.toArray
  }

  /*
   * Sentiment Analysis
   */

  // Returns the (countPositive, countNegative, countPositive-CountNegative) sums of the sentiment of each word in an annotated sentence/document.
  def getSentiment(annotation:Document, lexicon:Lexicon[String], sentimentDB:Counter[Int]):(Double, Double, Double) = {
    var countPositive:Double = 0
    var countNegative:Double = 0

    for (sent <- annotation.sentences) {
      for (i <- 0 until sent.words.size) {
        val word = sent.words(i).toLowerCase()
        val lemma = sent.lemmas.get(i).toLowerCase()
        val tag = sent.tags.get(i)

        // Find tLemma lexicon indices
        val tLemmaIdx = lexicon.add(CLUtils.mkTLemma(tag, lemma))
        val tWordIdx = lexicon.add(CLUtils.mkTLemma(tag, word))

        // Find sentiment
        var sentiment = sentimentDB.getCount(tLemmaIdx)
        if (sentiment == 0) sentiment = sentimentDB.getCount(tWordIdx)

        // Count
        if (sentiment > 0) countPositive += sentiment
        if (sentiment < 0) countNegative -= sentiment
      }
    }

    // Return
    (countPositive, countNegative, countPositive - countNegative)
  }



  /*
   * CoreNLP/Processors helper functions
   */
  // Fast inexpensive partial annotation
  def mkPartialAnnotation(text:String, processor:Processor):Document = {
    val doc = processor.mkDocument(text)
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    doc.clear
    doc
  }



  /*
   * Graphical functions for exporting schema
   */

  def exportSchemasPNG(schemaIndices:Array[Int], linksBetweenSchema:Array[(Int, Int)], highlightTLemmas:Array[Int], secondaryHighlight:Array[Int],
                       allSchemas:Array[Schema],
                       lexicon:Lexicon[String],
                       filename:String) = {
    val randInt = scala.util.Random.nextInt(10000)
    val tempFilename = "dot/temp." + randInt + ".dot"

    // Export DOT
    val dotMarkup = schemaGroupToDot(schemaIndices, linksBetweenSchema, highlightTLemmas, secondaryHighlight, allSchemas, lexicon)
    val pw = new PrintWriter(tempFilename)
    pw.print(dotMarkup)
    pw.close()

    // Make DOT directory
    //val cmd1:String = "mkdir dot"
    //cmd1.!

    // Run GraphViz to create PNG
    // Note: Timelimit ensures that when dot crashes (infrequently), that the simulation continues running, just missing that one image
    val cmd:String = "timelimit -t 10 -T 3 dot -Tpng " + tempFilename + " -o " + filename
    val exitCode:Int = cmd.!
    if (exitCode != 0) {
      println ("Warning: Failed to execute command " + cmd)
      //throw new RuntimeException("ERROR: Failed to execute command " + cmd)
    }

  }


  def schemaGroupToDot(schemaIndices:Array[Int], linksBetweenSchema:Array[(Int, Int)], highlightTLemmas:Array[Int], secondaryHighlight:Array[Int],
                      allSchemas:Array[Schema],
                      lexicon:Lexicon[String]): String = {

    val os = new StringBuilder
    val tLemmaAnchors = new ArrayBuffer[(Int, String)]      // (tLemmaIdx, anchorText)

    // Step 1: Header
    os.append("digraph GMaster {\n")
    os.append("\tcompound = true;\n")
    os.append("\tgraph [ dpi = 60 ];\n")

    // Step 2A: Links (invisible nodes to connect subgraphs)
    for (sIdx <- schemaIndices) {
      os.append("\ts" + sIdx + " [style=invisible, height=0, width=0, label=\"\"];\n")
    }
    os.append("\n")

    // Step 2B: Links (connections between invisible nodes)
    for (sLinkIdx <- linksBetweenSchema) {
      val sFromIdx = sLinkIdx._1
      val sToIdx = sLinkIdx._2
      os.append("\ts" + sFromIdx + " -> s" + sToIdx + " [lhead=clusters" + sToIdx + ", ltail=clusters" + sFromIdx + "];\n")
    }
    os.append("\n")

    // Step 3: Export schema subgraphs
    for (sIdx <- schemaIndices) {
      val schema = allSchemas(sIdx)
      os.append ("\tsubgraph clusters" + sIdx + " { s" + sIdx + "\n")
      val (schemaDot, tLemmaAnchorsSchema) = schemaToDot(schema, sIdx, highlightTLemmas, secondaryHighlight, lexicon)
      tLemmaAnchors.insertAll(tLemmaAnchors.size, tLemmaAnchorsSchema)      // Store list of (tLemmaIdx, anchorText) pairs, for drawing links between highlighted terms
      os.append ( schemaDot )
      os.append ("\t}\n")
    }

    // Step 4: Populate links between highlighted words (primary highlighting, optional)
    for (highlightLemma <- highlightTLemmas) {
      val toLink = new ArrayBuffer[String]
      for (tLemmaAnchor <- tLemmaAnchors) {
        val tLemmaIdx = tLemmaAnchor._1
        val anchorText = tLemmaAnchor._2
        if (tLemmaIdx == highlightLemma) {
          toLink.append( anchorText )
        }
      }

      // If the word exists in more than one schema, then link
      if (toLink.size > 1) {
        val populated = new ArrayBuffer[(Int, Int)]
        for (i <- 0 until toLink.size) {
          for (j <- 0 until toLink.size) {
            if (i != j) {
              // Check if we've already populated this link
              var found: Boolean = false
              for (k <- 0 until populated.size) {
                if (populated(k) == (i, j)) found = true
              }
              // If the link is unpopulated, then populate it
              if (!found) {
                os.append("\t" + toLink(i) + " -> " + toLink(j) + " [color=\"grey\"];\n")
                populated.append((i, j))
                populated.append((j, i))
              }
            }
          }
        }
      }
    }

    // Step 4B: Populate links between highlighted words (secondary highlighting, optional)
    for (highlightLemma <- secondaryHighlight) {
      val toLink = new ArrayBuffer[String]
      for (tLemmaAnchor <- tLemmaAnchors) {
        val tLemmaIdx = tLemmaAnchor._1
        val anchorText = tLemmaAnchor._2
        if (tLemmaIdx == highlightLemma) {
          toLink.append( anchorText )
        }
      }

      // If the word exists in more than one schema, then link
      if (toLink.size > 1) {
        val populated = new ArrayBuffer[(Int, Int)]
        for (i <- 0 until toLink.size) {
          for (j <- 0 until toLink.size) {
            if (i != j) {
              // Check if we've already populated this link
              var found: Boolean = false
              for (k <- 0 until populated.size) {
                if (populated(k) == (i, j)) found = true
              }
              // If the link is unpopulated, then populate it
              if (!found) {
                os.append("\t" + toLink(i) + " -> " + toLink(j) + " [color=\"red\"];\n")
                populated.append((i, j))
                populated.append((j, i))
              }
            }
          }
        }
      }

    }


    // Step 4: Footer
    os.append("}\n")

    // Return
    os.toString
  }

  def schemaToDot(schema:Schema, schemaIdx:Int, highlightTLemmas:Array[Int], secondaryHighlight:Array[Int], lexicon:Lexicon[String]):(String, Array[(Int, String)]) = {
    val os = new StringBuilder
    val lemmaCellLinks = new ArrayBuffer[(Int, String)]      // (tLemmaIdx, anchorText)
    var anchorIdx:Int = 1

    os.append("\tsubgraph schema" + schemaIdx + " { \n")
    os.append("\t\tnode [shape=record, style=filled]; \n")

    // Step 1: Populate clauses
    for (clauseIdx <- 0 until schema.clauses.size) {
      val clause = schema.clauses(clauseIdx)
      val clauseAnchor = "clause" + clauseIdx + "s" + schemaIdx
      os.append("\t\t\t" + clauseAnchor + " [shape=record, label=\"")

      var numHighlightTerms:Int = 0
      var numHighlightTerms2:Int = 0

      for (conceptIdx <- 0 until clause.concepts.size) {
        val conceptRolePair = clause.concepts(conceptIdx)
        val concept = conceptRolePair._1
        val role = conceptRolePair._2

        if (concept.terms.size > 1) {
          // List
          var highlight:Boolean = false
          var highlight2:Boolean = false

          os.append(" { LIST | ")
          for (termIdx <- 0 until concept.terms.size) {
            val term = concept.terms(termIdx)
            val tLemmaAnchor = anchorIdx
            os.append( "<" + tLemmaAnchor + "> " )

            for (tLemmaIdx <- term.lexiconIDs) {
              val tLemma = lexicon.get(tLemmaIdx).replaceAll("[<>]", "")

              os.append(tLemma + " ")                 // Anchor and Label
              lemmaCellLinks.append( (tLemmaIdx, clauseAnchor + ":" + tLemmaAnchor) )   // Store (lemma,anchor) pairs for easy linking between schema

              if (highlightTLemmas.contains(tLemmaIdx)) {
                highlight = true
                os.append("*")  // Add asterisck to matched highlighted terms
              }
              if (secondaryHighlight.contains(tLemmaIdx)) {
                highlight2 = true
                os.append("#")  // Add asterisck to matched highlighted terms
              }

            }
            if (termIdx < (concept.terms.size-1)) {
              os.append (" | ")
            }
            anchorIdx += 1
          }
          os.append(" }")
          if (highlight) numHighlightTerms += 1
          if (highlight2) numHighlightTerms2 += 1

        } else {
          // Single term
          var highlight:Boolean = false
          var highlight2:Boolean = false
          for (term <- concept.terms) {
            val tLemmaAnchor = anchorIdx
            os.append( "<" + tLemmaAnchor + "> " )

            for (tLemmaIdx <- term.lexiconIDs) {
              val tLemma = lexicon.get(tLemmaIdx).replaceAll("[<>]", "")

              os.append(tLemma + " ")                 // Anchor and Label
              lemmaCellLinks.append( (tLemmaIdx, clauseAnchor + ":" + tLemmaAnchor) )   // Store (lemma,anchor) pairs for easy linking between schema

              if (highlightTLemmas.contains(tLemmaIdx)) {
                highlight = true
                os.append("*") // Add asterisck to matched highlighted terms
              }
              if (secondaryHighlight.contains(tLemmaIdx)) {
                highlight2 = true
                os.append("#")  // Add asterisck to matched highlighted terms
              }
            }
            anchorIdx += 1
          }
          if (highlight) numHighlightTerms += 1
          if (highlight2) numHighlightTerms2 += 1
        }

        if (conceptIdx < (clause.concepts.size - 1)) {
          os.append (" | ")
        }
      }
      os.append("\"")

      // Highlight clause if highlighted terms are found
      if (numHighlightTerms == clause.concepts.size) {
        os.append(",fontcolor=\"green3\"")     // Full match
      } else if (numHighlightTerms > 0) {
        os.append(",fontcolor=\"blue\"")      // Partial match
      } else if (numHighlightTerms2 > 0) {
        os.append(",fontcolor=\"deeppink4\"")      // Secondary highlight
      }

      os.append("];\n")
    }

    // Step 2: Populate links between clauses
    for (clauseIdx <- 0 until schema.clauses.size) {
      val clause = schema.clauses(clauseIdx)
      val linksOut = schema.getLinks(clause)
      for (linkOut <- linksOut) {
        val dest = linkOut.destination
        val destClauseIdx = schema.getClauseIdx(dest)

        os.append("\t\t\tclause" + clauseIdx + "s" + schemaIdx + "-> clause" + destClauseIdx + "s" + schemaIdx)

        os.append(" [label=\"" + linkOut.label + "\"")
        if (linkOut.label == ClauseLink.CL_UNKNOWN) {      // Grey out unknown links
          os.append(",fontcolor=\"grey\"")
        } else {
          os.append(",fontcolor=\"purple4\"")
        }
        os.append("];\n")
      }
    }

    os.append("\t\t\tcolor = black;\n")
    os.append("\t\t}\n")

    // Step 3: Populate sentence text
    os.append ("\t\tlabel = \"S" + schemaIdx + ": " + breakSentence(schema.originalText) + "\\n" + schema.originalCorpus + " " + schema.originalDocID + " \";\n")

    // Return
    (os.toString(), lemmaCellLinks.toArray)
  }

  // Formats a sentence into multiple lines of a maximum length (as best as it can), for the schema visualizer
  def breakSentence(in:String, maxCharLength:Int = 40):String = {
    var os = new StringBuilder
    val inFiltered = in.replaceAll("\"", "'")               // Sanitize
    val inFiltered1 = inFiltered.replaceAll("[<>]", "")

    val split = inFiltered1.split(" ")
    var lengthCounter:Int = 0

    for (word <- split) {
      if ((lengthCounter + word.length) < maxCharLength) {
        os.append(word + " ")
        lengthCounter += word.length
      } else if (word.length >= maxCharLength) {
        os.append("\\n" + word + "\\n")
        lengthCounter = 0
      } else {
        os.append("\\n" + word + " ")
        lengthCounter = word.length
      }
    }

    os.toString()
  }


  // Writes the schemas in schemasIdx to PNG files, and returns a string referencing them
  def exportSchemasHTMLString(schemasIdx:Array[Int], schemas:Array[Schema], lexicon:Lexicon[String], path:String): String = {
    val os = new StringBuilder

    for (schemaIdx <- schemasIdx) {
      val schema = schemas(schemaIdx)

      // Export one schema PNG
      val filenamePNG = "schema" + schemaIdx + ".png"
      CLUtils.exportSchemasPNG(Array(schemaIdx), Array.empty[(Int, Int)], Array.empty[Int], Array.empty[Int], schemas, lexicon, path + filenamePNG)

      // Add to HTML log file
      os.append("Schema " + schemaIdx + ": " + schema.originalText + " <br>\n")
      os.append("<img src=\"" + path + filenamePNG + "\"> <br>\n")
      os.append("<br> <br>\n")

    }

    // Return
    os.toString()
  }


  def exportSchemasHTMLVis(schemas:Array[Schema], lexicon:Lexicon[String], path:String = "output/"): Unit = {
    //val path = "output/"

    // Chunking: Limit maximum schemas per file to about 1000 for easy browsing
    var maxPerFile:Int = 1000
    var chunkStart:Int = 0
    var chunkEnd:Int = 0
    var chunkNum:Int = 1

    while (chunkEnd < (schemas.size-1)) {
      println ("exportSchemasHTMLVis: Starting at " + chunkStart + " of " + schemas.size)
      val filenameHTML = path + "index" + chunkNum + ".html"
      val pw = new PrintWriter(filenameHTML)
      // Header
      pw.println("<html> <head> </head> <body> <center>")

      // Generate indices
      val indices = new ArrayBuffer[Int]
      for (i <- 0 until maxPerFile) {
        val idx = chunkStart + i
        if (idx < schemas.size) {
          indices.append(idx)
          chunkEnd = idx
        }
      }

      // Generate HTML and PNG files
      val html = exportSchemasHTMLString(indices.toArray, schemas, lexicon, path)
      pw.println(html)

      // Footer
      pw.println("</center> </body> </html>")

      pw.close()

      // Chunking
      chunkStart = chunkEnd
      chunkNum += 1
    }
  }

}



