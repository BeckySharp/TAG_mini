package edu.arizona.sista.mc.tessellatedgraph.Clauseinator

import java.io._
import edu.arizona.sista.mc.focusword.{FocusSentence, QuestionDecomposerAPI}
import edu.arizona.sista.mc.tessellatedgraph.structures._
import edu.arizona.sista.mc.tessellatedgraph.structures.ClauseLink._
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.qa.linearalg.SparseMatrix
import org.clulab.struct.{DirectedGraph, Lexicon}
import org.clulab.utils.StringUtils

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * Created by peter on 1/15/15.
 */

object Clauseinator {
  val processor:Processor = new CoreNLPProcessor(withDiscourse = 1)
//  val processor:Processor = new FastNLPProcessor(withDiscourse = true)
  var lexicon = new Lexicon[String]

  val SCIENCEDICT_SOURCE = "SCIENCE_DICT"
  val DICTIONARY_SOURCE = "DICTIONARY"
  val WIKTIONARY_SOURCE = "WIKTIONARY"

  /*
   * Display methods
   */

  def displayDiscourse(annotation:Document) {
    // Display the discourse tree
    val dt = annotation.discourseTree.get
    println ("Discourse tree: \n" + dt.toString())
  }

  def displayDependencies(annotation:Document) {
    // Display syntactic dependencies
    println ("Syntactic Dependencies: ")
    for (sent <- annotation.sentences) {
      println (displayDependencies(sent))
    }
  }

  def displayDependencies(sent:Sentence):String = {
    if ((sent.lemmas == None) || (sent.tags == None) || (sent.dependencies == None)) throw new RuntimeException ("ERROR: Annotation not populated")
    val words = sent.words
    val lemmas = sent.lemmas.get
    val tags = sent.tags.get
    val dependencies = sent.dependencies.get

    val outgoingEdges = dependencies.outgoingEdges
    val incomingEdges = dependencies.incomingEdges
    val roots = dependencies.roots

    val os = new StringBuilder

    os.append("roots: " + roots.mkString(sep = ",") + "\n")
    os.append("outgoing:\n")
    var n = 0
    while(n < dependencies.size) {
      os.append("\t" + n + "(" + lemmas(n) + "):")
      for (e <- outgoingEdges(n)) {
        //        os.append(" " + e)
        val depIdx = e._1
        val depType = e._2
        os.append ("(" + depIdx + "," + lemmas(depIdx) + "," + depType + ") ")

      }
      os.append("\n")
      n += 1
    }
    // Return string
    os.toString()
  }



  // Input: File with one or more sentences per line.
  // Output: Array of CLInputSentences
  def loadFreeText(filename:String, sourceText:String):Array[CLInputSentence] = {
    // Files of the format ("docID <tab> sentence")
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines()
    val out = new ArrayBuffer[CLInputSentence]

    var docid:Int = 1
    for (oneline <- lines) {
      val annotation = CLUtils.mkPartialAnnotation(oneline, processor)

      for (sent <- annotation.sentences) {
        val text = sent.getSentenceText()
        if (text.length > 2) {
          out.append(new CLInputSentence(docid.toString, sent.getSentenceText(), sourceText))
          docid += 1
        }
      }
    }

    out.toArray
  }

  def stringToCLInputSentence(s: String, sourceText: String, docidIn: Int = 1): (Seq[CLInputSentence], Int) = {
    val out = new ArrayBuffer[CLInputSentence]

    var docid:Int = docidIn

    val annotation = CLUtils.mkPartialAnnotation(s, processor)

    for (sent <- annotation.sentences) {
      val text = sent.getSentenceText()
      if (text.length > 2) {
        out.append(new CLInputSentence(docid.toString, sent.getSentenceText(), sourceText))
        docid += 1
      }
    }

    (out, docid)
  }



  /*
   * Supporting functions (business end)
   */

  // Assembles a term, whether a single word or compound
  // 'includedTerms': An array of size (words.size), which specifies whether a given word has been included in a Term or not yet.
  def getTerm(fSent:FocusSentence, wordIdx:Int, includedTerms:Array[Boolean]):Term = {
    val words = new ArrayBuffer[String]
    val lemmas = new ArrayBuffer[String]
    val tags = new ArrayBuffer[String]
    val originalIndicies = new ArrayBuffer[Int]
    val lexiconIDs = new ArrayBuffer[Int]

    val indicies = getTermHelper(fSent, wordIdx)
    for (index <- indicies) {
      // Check to see if we've already included this term
      if (includedTerms(index) == false) {
        val word = fSent.words(index).toLowerCase
        val lemma = fSent.lemmas.get(index).toLowerCase
        val tag = fSent.tags.get(index)
        words.append( word )
        lemmas.append( lemma )
        tags.append( tag )
        originalIndicies.append( index )
        includedTerms(index) = true

        // Make tagged lemma.  TODO: Here taggedLemmas are made out of each word, so compounds are not handled well.

        val tLemma = CLUtils.mkTLemma(tag, lemma)
        val lexiconID = lexicon.add(tLemma)
        lexiconIDs.append(lexiconID)
      }
    }

    // Create lexicon indices


    new Term(words.toArray, lemmas.toArray, tags.toArray, originalIndicies.toArray, lexiconIDs.toArray)
  }

  private def getTermHelper(fSent:FocusSentence, wordIdx:Int, alreadyTraversed:Array[Int] = Array.empty[Int]):Array[Int] = {
    val out = new ArrayBuffer[Int]
    val incomingEdges = fSent.dependencies.get.incomingEdges
    val outgoingEdges = fSent.dependencies.get.outgoingEdges
    // Traversal labels for compounds
    val validTraversalLabels = Array("nn", "prt")      // TODO: Are there other dependency tags that we want to make compounds over?

    // Exit case
    if (!CLUtils.hasContentTag(fSent, wordIdx)) {
      return Array.empty[Int]
    }

    // Add current index to list
    out.append(wordIdx)

    // Recursive cases
    for (e <- outgoingEdges(wordIdx)) {
      val depToIdx = e._1
      val depLabel = e._2
      if (validTraversalLabels.contains(depLabel)) {
        if (!alreadyTraversed.contains(depToIdx)) {       // Do not traverse if we've already been to that word
          out.insertAll(out.size, getTermHelper(fSent, depToIdx, out.toArray))
        }
      }
    }
    for (e <- incomingEdges(wordIdx)) {
      val depToIdx = e._1
      val depLabel = e._2
      if (validTraversalLabels.contains(depLabel)) {
        if (!alreadyTraversed.contains(depToIdx)) {       // Do not traverse if we've already been to that word
          out.insertAll(out.size, getTermHelper(fSent, depToIdx, out.toArray))
        }
      }
    }

    CLUtils.removeDuplicates(out.toArray)
  }

  // Creates a Concept, which may contain one or more Terms.
  // 'includedTerms': An array of size (words.size), which specifies whether a given word has been included in a Term or not yet.
  def getConceptTermOrList(fSent:FocusSentence, wordIdx:Int, includedTerms:Array[Boolean]):Concept = {
    val terms = new ArrayBuffer[Term]
    // Clause index of the term to extract.  We will only assemble lists that exist within the same clause.
    val clauseIdx = fSent.clauses(wordIdx)

    // Case: check if term is part of a list
    val listIdx = fSent.lists(wordIdx)
    if (listIdx > 0) {
      for (i <- 0 until fSent.lists.size) {
        if ((fSent.lists(i) == listIdx) && (fSent.clauses(i) == clauseIdx)) {
          if (CLUtils.hasContentTag(fSent, i)) {
            val candidateTerm = getTerm(fSent, i, includedTerms)
            if (!candidateTerm.isEmpty) {
              terms.append(candidateTerm)
            }
          }
        }
      }
    } else {
      // Case: term is not a part of a list
      if (CLUtils.hasContentTag(fSent, wordIdx)) {
        val candidateTerm = getTerm(fSent, wordIdx, includedTerms)
        if (!candidateTerm.isEmpty) {
          terms.append(candidateTerm)
        }
      }
    }

    new Concept(terms.toArray)
  }

  // Returns TRUE if a clause contains a negation flag (e.g. "the tree is not green")
  def detectNegatedClause(fSent:FocusSentence, clauseIdx:Int):Boolean = {
    val negatedLabels = Array("neg")
    val outgoingEdges = fSent.dependencies.get.outgoingEdges

    for (i <- 0 until outgoingEdges.size) {
      if (fSent.clauses(i) == clauseIdx) {
        for (e <- outgoingEdges(i)) {
          val depToIdx = e._1
          val depLabel = e._2
          if (negatedLabels.contains(depLabel)) {
            return true
          }
        }
      }
    }

    false
  }

  def detectTransitiveVerbClause(fSent:FocusSentence, clauseIdx:Int):Boolean = {
    // TODO:
    return false
  }

  /*
   * Extraction methods
   */


  def schematizer(annotation:Document, fSent:FocusSentence, docid:String, text:String, source:String,
    verbose: Boolean = true):Schema = {

    val outSchema = new Schema(text, source, docid, "UNKNOWN_CATEGORY")

    // Step 1: Display dependencies
    if (verbose) {
      displayDiscourse(annotation)
      displayDependencies(annotation)
    }


    // Step 2: Display focus sentence annotation
    if (verbose) println ( fSent.toString() )

    // Step 3: Extract clauses
    val clauses = new ArrayBuffer[Clause]
    val includedTerms = Array.fill[Boolean](fSent.words.size)(false)
    for (clauseIdx <- 1 to fSent.numClauses) {
      // Extract concepts
      val concepts = new ArrayBuffer[(Concept, String)]     // (Concept, role) tuples
      for (wordIdx <- 0 until fSent.words.size) {
        if (fSent.clauses(wordIdx) == clauseIdx) {
          val candidateConcept = getConceptTermOrList(fSent, wordIdx, includedTerms)
          // Ensure that a concept has one or more terms before adding
          if (!candidateConcept.isEmpty) {
            val role:String = Clause.ROLE_OTHER            // TODO: val role = Clause.depToRole()...
            concepts.append( (candidateConcept, role) )
          }
        }
      }

      val clauseType = Clause.CTYPE_OTHER                 // TODO: Detect clause type
      val clause = new Clause(concepts.toArray,
                              clauseType,
                              negationFlag = detectNegatedClause(fSent, clauseIdx),
                              transitiveVerbFlag = detectTransitiveVerbClause(fSent, clauseIdx))

      clauses.append(clause)

      // Add clause to schema
      outSchema.addClause(clause)
    }


    // Step 4: Link clauses
    for(clauseLink <- fSent.clauseLinks) {
      breakable {
        val wordFromIdx = clauseLink._1
        val wordToIdx = clauseLink._2

        // Break if one of the indicies is invalid (e.g. clause started from root)
        if (wordToIdx == -1) break()
        if (wordFromIdx == -1) break()

        // Note: Clause index in clauses array (from Step 3, above) is zero indexed, so the indicies from fSent must be reduced by 1
        var clauseToIdx = fSent.clauses(wordToIdx) - 1
        var clauseFromIdx = fSent.clauses(wordFromIdx) - 1

        // Add link
        val clauseTo = clauses(clauseToIdx)
        val clauseFrom = clauses(clauseFromIdx)

        // Determine link type (first-pass)
        val depLabel = CLUtils.getDepLabel(fSent, wordFromIdx, wordToIdx)
        val linkLabel = ClauseLink.dependencyToLinkType(depLabel)

        // TODO: Detect extended link types (definition, etc)
        outSchema.addLink(clauseFrom, clauseTo, linkLabel, isBidirectional = false)
      }
    }


    // Return
    return outSchema
  }

  // Input:  Array of CLInputSentences.  'source' is document source (e.g. "Barrons", "Dictionary", etc)
  def  extract(in:Array[CLInputSentence], verbose: Boolean = true): Array[Schema] = {
    val out = new ArrayBuffer[Schema]

    for (i <- 0 until in.size) {
      val CLSent = in(i)
      val docid = CLSent.docid
      val sentenceText = CLSent.text
      val source = CLSent.source

      // Step 1: Display sentence
      if (verbose && i % 10 == 0) println (s"  extracting schema from sentence $i of ${in.length}: " + sentenceText)

      // Step 2: Annotate with CoreNLP and focus word extractor
      val annotation = processor.annotate(sentenceText)
      // Pass through question decomposer to tag clauses, lists, etc
      val focusSentences = QuestionDecomposerAPI.analyze(annotation)
      val fSent = focusSentences(0)

      // Step 3: Schematize
      val newSchema = schematizer(annotation, fSent, docid, sentenceText, source, verbose)

      if (verbose) {
        println ( "* Raw Schema: ")
        println ( newSchema.toString )      // Display
      }


      // Step 4: Post-processing: Re-break
      schemaPostRebreak(annotation, fSent, newSchema)
      if (verbose) {
        println ( "* Schema (after re-break): ")
        println ( newSchema.toString )      // Display
      }


      //TODO: the schemaPostRebreaking will give good idea on re-breaking on dict term...

      // Step 5: Post-processing: Repair/Filtering
      // Filter out stop-words
      schemaPostFilter(annotation, fSent, newSchema)
      if (verbose) {
        println ( "* Schema (after filtering): ")
        println ( newSchema.toString )      // Display
      }

      // Repair the dictionary schemas by ensuring that (a) the correct POS was assigned and (b) there is a labeled link to the defined word clause
      if (CLSent.isDefinition){
        schemaPostRepairDefinitions(newSchema, CLSent.definedWord, CLSent.definedWordPOS)

        if (verbose) {
          println ( "* Schema (after repairing dictionary definitions): ")
          println ( newSchema.toString )      // Display
        }

      }


      // Step 6: Post-processing: Inheritance?
//      if (newSchema != filtered2) {
//        println ( ">>> newSchema : ")
//        println ( newSchema.toString )      // Display
//        println ( ">>> filtered2 : ")
//        println ( filtered2.toString )      // Display
//        throw new RuntimeException ("SCHEMAS DON'T EQUAL")
//      }

      if (newSchema.clauses.size > 1) {
        out.append(newSchema)
      }


      if (verbose) println (" ========================================================================= ")
    }

    out.toArray
  }


  /*
   * Schema filtering
   */
  def schemaPostRebreak(annotation:Document, fSent:FocusSentence, in:Schema): Schema = {
    // TODO

    return in
  }

  def schemaPostRepairDefinitions(in:Schema, definedWord: String, definedTag: String):Schema = {

    var definedWordClause:Clause = null
    var definedWordFound = false

    for (clause <- in.clauses) {

      // TODO: currently this only works for single word defined words (i.e. one concept in clause, one term in the concept, and one word in the term... EXPAND!
      // Look for the clause with the defined word and double check its POS tag, update as necessary with info from dictionary
      if (clause.concepts.size == 1) {
        val currConcept = clause.concepts(0)._1

        if (currConcept.terms.size == 1) {
          val currTerm = currConcept.terms(0)

          if (currTerm.words.size == 1) {
            val currWord = currTerm.words(0)
            if (currWord == definedWord) {

              // make note of which clause is the defined word
              definedWordFound = true
              definedWordClause = clause

              // check POS and update it
              val currPOS = currTerm.tags(0)
              println("definedTag: " + definedTag)
              if (!currPOS.startsWith(definedTag) && (definedTag != "")) {
                currTerm.tags(0) = definedTag                          // Update tag in Term
                val lemma = currTerm.lemmas(0)
                val tLemma = CLUtils.mkTLemma(definedTag, lemma)       // TODO: Conceivably the lemmatization could have been done improperly if the original tag was incorrect
                val lexiconID = lexicon.add(tLemma)
                currTerm.lexiconIDs(0) = lexiconID                     // TODO: currently works bc only looking for 1 word terms, but should be changed at some point...
              }
            }
          }
        }
      }
    }


    // If a clause containing only a definition word is found, iterate through the clauseLinks in the schema and add DEF label to links to/from that clause
    // TODO: While ideally the link direction would go /from/ the clause containing the definition word /to/ the root clause or other clauses
    // containing the definition, currently the link may be of any direction.
    if (definedWordFound) {
      for (clause <- in.clauses) {
        for (link <- in.getLinks(clause)) {
          if (link.source == definedWordClause) {
            link.relabel(ClauseLink.CL_DEFINITION)
          } else if (link.destination == definedWordClause) {
            // Link is in incorrect direction (e.g. definition -> definedWord).  Swap direction and add label
            val newSource = link.destination
            val newDest = link.source
            in.removeLink(link.source, link.destination)
            in.addLink(newSource, newDest, ClauseLink.CL_DEFINITION, isBidirectional = false)
          }
        }
      }

      //Relabel the clause as a DEFINITION
      definedWordClause.relabel(Clause.CTYPE_DEF)
    }

    in
  }

  def schemaPostFilter(annotation:Document, fSent:FocusSentence, in:Schema): Schema = {
    val stopFWTags = Array(FocusSentence.TAG_STOP, FocusSentence.TAG_STOPPH)

    var idxClause:Int = 0
    while (idxClause < in.clauses.size) {
      val clause = in.clauses(idxClause)

      var idxCRPair:Int = 0
      while(idxCRPair < clause.concepts.size) {
        val concept = clause.concepts(idxCRPair)._1

        var idxTerm:Int = 0
        while(idxTerm < concept.terms.size) {
          val term = concept.terms(idxTerm)

          // Step 1: Check to see if the term contains at least one non-stop word
          var nonStopPresent:Boolean = false
          for (index <- term.originalIndicies) {
            // Lookup focus word tag
            val fwTag = fSent.focusTag(index)
            if (!stopFWTags.contains(fwTag)) {
              nonStopPresent = true
            }
          }

          // Step 2: If the term is all stop words, remove the term from the concept
          if (!nonStopPresent) {
            concept.removeTerm(term)
          } else {
            idxTerm += 1
          }
        }

        // Step 3: If we've removed all the valid terms from a concept, then filter out the concept too
        if (concept.isEmpty) {
          clause.removeConcept(concept)
        } else {
          idxCRPair += 1
        }
      }

      // Step 4: If we've removed all the valid concepts from a clause, then filter that out too
      if (clause.isEmpty) {
        in.removeClause(clause)
      } else {
        idxClause += 1
      }
    }

    // TODO: Also remove clauses that are empty.  Clause removal will have to remove both clauses and links to/from that clause

    // Return filtered schema
    in
  }

  /*
   * Connectivity
   */

  def makeWordSchemaLookupTable(schemasIn:Array[Schema]):SparseMatrix[Double] = {
    val wordToSchemaLUT = new SparseMatrix[Double](0.0)

    for (schemaIdx <- 0 until schemasIn.size) {
      val schema = schemasIn(schemaIdx)

      // Step 1: Find all words that are in this schema (by lexicon index)
      for (clause <- schema.clauses) {
        for (conceptRolePair <- clause.concepts) {
          val concept = conceptRolePair._1
          val role = conceptRolePair._2
          for (term <- concept.terms) {
            for (tLemmaLexiconIdx <- term.lexiconIDs) {
              // Add lemma index/schema index pair to look-up table
              wordToSchemaLUT.set(tLemmaLexiconIdx, schemaIdx, 1)
            }
          }
        }
      }

    }

    println ("makeWordSchemaLookupTable: (numRows: " + wordToSchemaLUT.numRows + "   numCols: " + wordToSchemaLUT.numCols + ")")
    println ("Sparsity: " + wordToSchemaLUT.sparsity)

    // Return
    wordToSchemaLUT
  }


  def makeSchemaSchemaLookupTable(schemasIn:Array[Schema], wordSchemaLUT:SparseMatrix[Double], lexiconIn:Lexicon[String]):SparseMatrix[Double] = {
    val schemaToSchemaLUT = new SparseMatrix[Double](0.0)

    println ("makeSchemaSchemaLookupTable: Started... ")
    // For each word in the word->Schema LUT
    for (wordIdx <- 0 until wordSchemaLUT.numRows) {
      //if ((wordIdx % 100) == 0) println ("  Lexicon index " + wordIdx + " of " + wordSchemaLUT.numRows)
      println ("  Lexicon index " + wordIdx + " of " + wordSchemaLUT.numRows)

      val schemasWithLemmaSV = wordSchemaLUT.getRowVecSparse(wordIdx)
      val schemaIndicies = new ArrayBuffer[Int]

      // Step 1: Extract all the schemas that contain this lemma
      schemasWithLemmaSV.foreach(e =>
        if (e._2 == 1.0f) {
          schemaIndicies.append(e._1)
        })

      // Step 2: Interconnect each schema on the list
      val tLemma = lexiconIn.get(wordIdx)
      val tag = CLUtils.getPOSTag(tLemma)
      val validConnections = Array("NN", "VB", "JJ")
      // Only connect on nouns, verbs, and adjectives
      if (validConnections.contains(tag)) {
        for (idx1 <- schemaIndicies) {
          for (idx2 <- schemaIndicies) {
            if (idx1 != idx2) {
              // Bidirectional
              schemaToSchemaLUT.set(idx1, idx2, 1.0)
              schemaToSchemaLUT.set(idx2, idx1, 1.0)
              //println("Connecting schemas " + idx1 + " and " + idx2 + " through word " + tLemma)
            }
          }
        }
      }
    }

    println ("makeSchemaSchemaLookupTable: (numRows: " + schemaToSchemaLUT.numRows + "   numCols: " + schemaToSchemaLUT.numCols + ")")
    println ("Sparsity: " + schemaToSchemaLUT.sparsity)

    // Return
    schemaToSchemaLUT
  }



  /*
   * Load/Save Schema Array
   */
  def saveSchemas(filenamePrefix:String, in:Array[Schema]): Unit = {
    // Step 1: Save schemas
    println ("Saving schemas to: " + filenamePrefix + ".schemas")
    val os = new ObjectOutputStream(new FileOutputStream(filenamePrefix + ".schemas"))
    os.writeObject(in)
    os.close()

    // Step 2: Save lexicon
    lexicon.saveTo(filenamePrefix + ".lexicon")

  }

  def loadSchemas(filenamePrefix:String):(Array[Schema], Lexicon[String]) = {
    // Step 1: Load schemas
    println ("Loading schemas from: " + filenamePrefix + ".schemas" )
    val is = new ObjectInputStream(new FileInputStream(filenamePrefix + ".schemas"))
    val schArray = is.readObject().asInstanceOf[Array[Schema]]
    is.close()
    println ("Loaded " + schArray.size + " schemas.")

    // Step 2: Load lexicon
    val loadedLexicon = Lexicon.loadFrom[String](filenamePrefix + ".lexicon")

    // Return
    (schArray, loadedLexicon)
  }



  /*
   * Subtasks
   */

  def clausinateAndSave(sentences:Array[CLInputSentence], filenameOut:String, verbose: Boolean = true): Unit = {
    // Load barrons sentences

    // Extract
    val schemas = extract(sentences, verbose)

    // Save schemas
    saveSchemas(filenameOut, schemas)

  }



//  def main(args:Array[String]): Unit = {
//    val props = StringUtils.argsToProperties(args)
//
//    // Initialize question decomposer API
//    QuestionDecomposerAPI.initialize(props)
//
//    val sentences = loadDictionarySentences("/data1/nlp/corpora/AriResources/Text-resources-11-13-13/dictionaries/Science_Dictionary_for_Kids_book_filtered-pruned-feb4.txt", "SCIENCE_DICTIONARY")
//
//
//    // Free text
//    //val sentences = loadFreeText("resources/virginia_SOL_framework_science4.filtered1.txt", "VIRGINIA_FRAMEWORK")
//    //val sentences = loadFreeText("resources/virginia_SOL_flashcards-science5.filtered.txt", "VIRGINIA_FLASHCARDS")
//    //val sentences = loadFreeText("resources/virginia_SOL20Study20Guide.filtered.noquestions.txt", "VIRGINIA_STUDYGUIDE_NOQ")
//
//    // Step 2: Clausinate
//    //val filenameSchemas = "schemas_VirginiaFlashcards_mar4"
//    //val filenameSchemas = "schemas_VirginiaFrameworkFourth_mar4"
//    //val filenameSchemas = "schemas_VirginiaStudyGuide_noq_mar4"
//    //val filenameSchemas = "schemas_Dictionary11_mar4"
//    //val filenameSchemas = "schemas_scienceDictionary7_mar4"
//    //val filenameSchemas = "schemas_Barrons7_mar4"
//    val filenameSchemas = "schemas_wiktionary4_may1"
//    clausinateAndSave(sentences, filenameSchemas)
//
//    // Step 3: Export to HTML
//    val pathOut = "/home/bsharp/wiktionaryOut4/"
//    val (schemas, lexicon) = loadSchemas(filenameSchemas)
//    CLUtils.exportSchemasHTMLVis(schemas, lexicon, pathOut)
//
//
//  }
//
}


// Storage class for a single input sentence
class CLInputSentence(val docid:String, val text:String, val source:String,
                      val definedWord:String, val definedWordPOS:String) {

  def this(docid:String, text:String, source:String) = this(docid, text, source, "", "")

  def isDefinition:Boolean = {
    if (definedWord.length > 0) return true
    false
  }

}
