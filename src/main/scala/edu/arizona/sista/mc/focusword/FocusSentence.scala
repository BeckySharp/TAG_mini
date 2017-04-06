package edu.arizona.sista.mc.focusword

import org.slf4j.LoggerFactory
import FocusSentence._
import org.clulab.processors.Sentence
import org.clulab.struct.{Counter, Tree}

import collection.mutable.ArrayBuffer
import scala.math._
import scala.util.control.Breaks._

/**
 * Storage class for an annotated sentence that also includes focus word information
 * User: peter
 * Date: 10/7/14
 */

class FocusSentence(in:Sentence, concretenessNormsDB:Counter[String], isAnswerSentence:Boolean = false, mode:Int = FOCUS_NORMAL) extends Sentence(in.words, in.startOffsets, in.endOffsets) {
  // Constructor
  tags =  in.tags
  lemmas = in.lemmas
  entities = in.entities
  norms = in.norms
  chunks = in.chunks
  syntacticTree = in.syntacticTree
  dependenciesByType = in.dependenciesByType
  var focusTag = Array.fill[String](in.size)("")
  var concreteness = Array.fill[Double](in.size)(-1)
  var ranks = Array.fill[Int](in.size)(-1)
  val tuples = new ArrayBuffer[(String, Array[Int])]()    // Array of (Tuple_Type, Indicies of words involved in tuple)
  val clauses = Array.fill[Int](in.size)(0)
  val clauseLinks = new ArrayBuffer[(Int, Int)]           // (from, to) tuples, both are word indicies in the sentence
  val lists = Array.fill[Int](in.size)(0)

  var focusListElems = Array.empty[(String, Int, Int)]    // Array of (Lemma, Index in sentence, Rank) tuples
  var focusPrimary = Array.empty[(String, Int, Int)]
  var focusBackoff = Array.empty[(String, Int, Int)]
  var focusAnswerType = Array.empty[(String, Int, Int)]

  var numClauses:Int = 0
  var numLists:Int = 0


  // Step 0: Check for baseline methods
  if (mode == BASELINE_UNIFORM) {
    uniformWeightBaseline()
    populateFocusCategories()

  } else {

    // Step 1: Populate concretenesss
    populateConcreteness()

    // Step 2: Tag stop words
    tagStopWords()
    tagStopPhrases()

    // Step 3: Tag transparent nouns
    tagTransparentWords()

    // Step 4: Tag answer type words
    tagAnswerTypeWords()

    // Step 5: Tag examples that are ascertained by named entities
    tagNamedEntityExampleWords()

    // Step 6: Tag words that are too concrete as example words
    tagConcreteWords()

    // Step 7: Tag words that are too abstract as abstract words
    tagAbstractWords()


    // Step 8: Tag unfiltered words that have not been captured by any seive (and are inferred to be focus words) as focus words
    tagUnfilteredWordsAsFocus()

    // Step 9: (Answer Focus Words): Elevate non-stop verbs to focus position
    if (isAnswerSentence) {
      tagNonStopVerbsAsFocus()
    }

    // Detect and tag tuples
    detectFromToTuples()

    // Detect and tag lists
    findLists()

    // Detect sequences (these currently aren't tagged -- it's unclear how useful they are)
    //  if (isAnswerSentence) {
    //    detectToSequenceTuples(tag = true)
    //  } else {
    detectToSequenceTuples(tag = false)
    //  }

    // Answer focus words: Tag words with specific dependencies as focus
    if (isAnswerSentence) {
      tagDependenciesAsFocus()
    }

    // Detect clause boundaries
    findClauseBoundaries()

    //## Detect lists


    // Step 10: Rank answer type words
    incrementRanksWithTag(TAG_ANSWERTYPEWORD, 1)

    // Stop 11: Rank example words filtered out with named-entity recognition
    incrementRanksWithTag(TAG_EXAMPLENAMEDENT)

    // Step 12: Rank words based on concreteness
    setRanksConcreteness()

    // Artificially separate the scores of the backoff words and the primary focus words by a bit of a rift
    val primaryFocusMinRank = findMaxRank + 10

    // Step 13: Rank words that have been tagged as focus words
    incrementRanksWithTag(TAG_FOCUS, primaryFocusMinRank)

    // Step 14: Rank words that have been tagged as focus tuples such as lists or from-X-to-Y tuples
    incrementRanksWithTag(TAG_FOCUSLISTELEM, primaryFocusMinRank)
    incrementRanksWithTag(TAG_FOCUSFROMTO, primaryFocusMinRank)
    //  if (isAnswerSentence) {
    //    incrementRanksWithTag(TAG_FOCUSSEQ)
    //  }

    // Step 15: Sort ranked focus words into focus categories for the user (lists, primary focus words, backoff words, answer type words).
    populateFocusCategories()

  }

  // Populate the psycholinguistic concreteness norm values of each lemma.  (If no concreteness value is found, the default of -1 is kept)
  def populateConcreteness() {
    if (words.size == 0) return
    if (lemmas.get.size == 0) throw new RuntimeException ("ERROR: Lemmas must be populated in sentence annotation for FocusSentence")

    for (i <- 0 until lemmas.get.size) {
      val lemma = lemmas.get(i).toLowerCase
      val conc = concretenessNormsDB.getCount(lemma)
      if (conc > 0) concreteness(i) = conc
    }
  }


  // set focusTag to TAG_STOP for any words that are in the stopWordsFocus list
  def tagStopWords() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)

      // Is word on stop word list?
      if (stopWordsFocus.contains(word) || stopWordsFocus.contains(lemma)) {
        focusTag(i) = TAG_STOP
      } else {
        // Check to see if the word starts with a non-stop tag  (e.g. a tag other than NN*, VB*, JJ*, RB*)
        var found = false
        for (nonStopTag <- nonStopTags) {
          if (tag.startsWith(nonStopTag)) found = true
        }
        if (!found) focusTag(i) = TAG_STOP
      }

    }
  }

  // set focusTag to TAG_STOPPH for any phrases that are in the stopPhrases list
  def tagStopPhrases() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)

      for (stopPhrase <- stopPhrases) {
        breakable {
          if (i + stopPhrase.size >= words.size) break()      // Break if there isn't room for the stop phrase to exist beyond this point
          // Check phrase
          for (j <- 0 until stopPhrase.size) {
            val sentWord = words(i + j)
            val sentLemma = lemmas.get(i + j)
            val stopWord = stopPhrase(j)
            if ((sentWord != stopWord) && (sentLemma != stopWord)) break()
          }
          // Match -- set flags
          for (j <- 0 until stopPhrase.size) {
            focusTag(i + j) = TAG_STOPPH
          }
        }
      }
    }
  }

  // set focusTag to TAG_STOP for any words that are in the stopWordsFocus list
  def tagTransparentWords() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)

      if (transparentNouns.contains(word) || transparentNouns.contains(lemma)) {
        focusTag(i) = TAG_TRANSPARENT
      }

    }
  }

  // set focusTag to TAG_ANSWERTYPE for any words that are dominated by a WHNP
  def tagAnswerTypeWords() {
    val tree = syntacticTree.get
    findAnswerTypeWHNPRec(tree, false)
    findAnswerTypeWHVBZRec(tree)
    findAnswerTypeAnXThatRec(tree)
    findAnswerTypeXOfYIsRec(tree)
  }

  // Answer type: Find structures of the form "What <NP>" (e.g. "What <kind of> energy"), tagging these words as answer type words
  def findAnswerTypeWHNPRec(node:Tree, active:Boolean) {
    // Step 1: Check for active condition
    var isActive = active
    if (node.value == "WHNP") isActive = true
    if (node.isLeaf) return

    // Step 2: If active, tag words as TAG_ANSWERTYPE
    if (isActive) {
      // Set tag to TAG_ANSWERTYPE
      val idx = node.startOffset
      val tag = tags.get(idx)
      if (tag.startsWith("NN")) {
        if (focusTag(idx).length == 0) {      // only tag if this word has not yet been tagged
          focusTag(idx) = TAG_ANSWERTYPEWORD
        }
      } else {
        if (focusTag(idx).length == 0) {
          focusTag(idx) = TAG_ANSWERTYPEWORDMOD     // Modifier of an answer type word
        }
      }
    }

    // Step 3: Recurse down tree
    for (c <- node.children.get) {
      findAnswerTypeWHNPRec(c, isActive)
    }
  }

  // Answer type: What is <NP> that/of...
  def findAnswerTypeWHVBZRec(node:Tree) {
    // Check if leaf
    if (node.isLeaf) return

    val children = node.children.get

    // Check for pattern
    breakable {
      if (children.size < 3) break()
      if (!(children(0).value == "WHNP")) break()     // What
      if (!(children(1).value == "VBZ")) break()      // is
      if (!(children(2).value == "NP")) break()
      if (children(2).children == None) break()
      val npChildren = children(2).children.get
      val npChild = npChildren(0)
      if (!(npChild.value == "NP")) break()           // NP

      // Tag all elements in this NP as answer type words
      for (idx <- npChild.startOffset until npChild.endOffset) {
        val tag = tags.get(idx)
        if (tag.startsWith("NN")) {
          if (focusTag(idx).length == 0) {            // only tag if this word has not yet been tagged
            focusTag(idx) = TAG_ANSWERTYPEWORD
          }
        } else {
          if (focusTag(idx).length == 0) {
            focusTag(idx) = TAG_ANSWERTYPEWORDMOD     // Modifier of an answer type word
          }
        }
      }
    }

    // Step 3: Recurse down tree
    for (c <- children) {
      findAnswerTypeWHVBZRec(c)
    }
  }

  // Answer type: An X that...
  def findAnswerTypeAnXThatRec(node:Tree) {
    // Check if leaf
    if (node.isLeaf) return

    val children = node.children.get

    // Check for pattern
    breakable {
      if (children.size < 2) break()
      if (!(children(0).value == "NP")) break()     // NP (e.g. "A simple machine")
      if (!(children(1).value == "SBAR")) break()
      val sbarChildren = children(1).children.get
      val sbarChild = sbarChildren(0)
      if (!(sbarChild.value == "WHNP")) break()     // WHNP (e.g. "that")

      // Tag all elements in the first NP as answer type words
      val npStartOffset = children(0).startOffset
      val npEndOffset = children(0).endOffset
      for (idx <- npStartOffset until npEndOffset) {
        val tag = tags.get(idx)
        if (tag.startsWith("NN")) {
          if (focusTag(idx).length == 0) {            // only tag if this word has not yet been tagged
            focusTag(idx) = TAG_ANSWERTYPEWORD
          }
        } else {
          if (focusTag(idx).length == 0) {
            focusTag(idx) = TAG_ANSWERTYPEWORDMOD     // Modifier of an answer type word
          }
        }
      }
    }

    // Step 3: Recurse down tree
    for (c <- children) {
      findAnswerTypeAnXThatRec(c)
    }
  }

  // Answer type: X of ... is ...  (e.g. "The main function of X is to ...")
  def findAnswerTypeXOfYIsRec(node:Tree) {
    // Check if leaf
    if (node.isLeaf) return

    val children = node.children.get

    // Check for pattern
    breakable {
      if (children.size < 2) break()
      if (!(children(0).value == "NP")) break()     // NP (e.g. "The main function of ... ")
      if (!(children(1).value == "VP")) break()     // VP (e.g. "is to ... ");

      val npChildren = children(0).children.get
      if (!(npChildren(0).value == "NP")) break()     // NP (e.g. "The main function")
      if (!(npChildren(1).value == "PP")) break()     // PP (e.g. "of ...");

      val vpChildren = children(1).children.get
      if (!(vpChildren(0).value == "VBZ")) break()     // VP (e.g. "is ...")

      // Tag all elements in the first NP as answer type words
      val npStartOffset = npChildren(0).startOffset
      val npEndOffset = npChildren(0).endOffset
      for (idx <- npStartOffset until npEndOffset) {
        val tag = tags.get(idx)
        if (tag.startsWith("NN")) {
          if (focusTag(idx).length == 0) {            // only tag if this word has not yet been tagged
            focusTag(idx) = TAG_ANSWERTYPEWORD
          }
        } else {
          if (focusTag(idx).length == 0) {
            focusTag(idx) = TAG_ANSWERTYPEWORDMOD     // Modifier of an answer type word
          }
        }
      }
    }

    // Step 3: Recurse down tree
    for (c <- children) {
      findAnswerTypeXOfYIsRec(c)
    }
  }



  // set focusTag to TAG_EXAMPLE for any words that are in a list of named entity examples
  def tagNamedEntityExampleWords() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)
      val entity = entities.get(i)

      if (namedEntityExampleCategories.contains(entity)) {
        if (focusTag(i).length == 0) {
          focusTag(i) = TAG_EXAMPLENAMEDENT
        }
      }
    }
  }

  // set focusTag to TAG_EXAMPLE for any words that are too concrete
  def tagConcreteWords() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)
      val conc = concreteness(i)

      if (conc > CONCRETENESS_FILTERHIGH) {
        if (focusTag(i).length == 0) {
          focusTag(i) = TAG_EXAMPLE
        }
      }

    }
  }

  // set focusTag to TAG_ABSTRACT for any words that are too concrete
  def tagAbstractWords() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)
      val conc = concreteness(i)

      if ((conc > 0) && (conc < CONCRETENESS_FILTERLOW)) {
        if (focusTag(i).length == 0) {
          focusTag(i) = TAG_ABSTRACT
        }
      }

    }
  }

  // tag leftover words that have not been captured by a sieve as focus words
  def tagUnfilteredWordsAsFocus() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)
      val fwTag = focusTag(i)

      if (fwTag.length == 0) {
        focusTag(i) = TAG_FOCUS
      }
    }
  }

  /*
   * Special functions for handling answer focus words
   */

  // Answer focus words: tag non-stop verbs as focus words
  def tagNonStopVerbsAsFocus() {
    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)
      val fwTag = focusTag(i)

      if ((fwTag != TAG_STOP) && (fwTag != TAG_STOPPH) && (fwTag != TAG_TRANSPARENT)) {
        if (tag.startsWith("VB") || word.endsWith("ing")) {
          focusTag(i) = TAG_FOCUS
        }
      }
    }
  }

  // Answer focus words: tag words with specific dependency tags as focus words
  def tagDependenciesAsFocus() {
    val outgoingEdges = dependencies.get.outgoingEdges
    val incomingEdges = dependencies.get.incomingEdges
    val roots = dependencies.get.roots
    var found:Boolean = false

//    println("Incoming: ")
    for (i <- 0 until incomingEdges.size) {
//      print(words(i) + " : ")
      for (e <- incomingEdges(i)) {
        val depIdx = e._1
        val depType = e._2
        if ((focusTag(depIdx) != TAG_STOP) && (focusTag(depIdx) != TAG_STOPPH)) {
          if ((depType.contains("comp")) || (depType == "dobj")) {
            focusTag(depIdx) = TAG_FOCUS
//            print("TRUE-")
            found = true
          }
          if ((depType == "dep") || (depType == "amod")) {
            focusTag(depIdx) = TAG_FOCUS
//            print("TRUE-")
          }
        }
//        print("(" + words(depIdx) + ", " + depType + ")    ")
      }
//      println("")
    }


//    println("Outgoing: ")
    for (i <- 0 until outgoingEdges.size) {
//      print(words(i) + " : ")
      for (e <- outgoingEdges(i)) {
        val depIdx = e._1
        val depType = e._2
        if ((focusTag(depIdx) != TAG_STOP) && (focusTag(depIdx) != TAG_STOPPH)) {
          if ((depType.contains("comp")) || (depType == "dobj")) {
            focusTag(depIdx) = TAG_FOCUS
//            print("TRUE-")
            found = true
          }
          if ((depType == "dep") || (depType == "amod")) {
            focusTag(depIdx) = TAG_FOCUS
//            print("TRUE-")
          }
        }
//        print("(" + words(depIdx) + ", " + depType + ")    ")
      }
//      println("")
    }



    if (!found) {
      // Extend to prepositions
      //    println("Incoming: ")
      for (i <- 0 until incomingEdges.size) {
        //      print(words(i) + " : ")
        for (e <- incomingEdges(i)) {
          val depIdx = e._1
          val depType = e._2
          if ((focusTag(depIdx) != TAG_STOP) && (focusTag(depIdx) != TAG_STOPPH)) {
            if (depType.startsWith("prep")) {
              focusTag(depIdx) = TAG_FOCUS
              //            print("TRUE-")
            }
          }
          //        print("(" + words(depIdx) + ", " + depType + ")    ")
        }
        //      println("")
      }


      //    println("Outgoing: ")
      for (i <- 0 until outgoingEdges.size) {
        //      print(words(i) + " : ")
        for (e <- outgoingEdges(i)) {
          val depIdx = e._1
          val depType = e._2
          if ((focusTag(depIdx) != TAG_STOP) && (focusTag(depIdx) != TAG_STOPPH)) {
            if (depType.startsWith("prep")) {
              focusTag(depIdx) = TAG_FOCUS
              //            print("TRUE-")
            }
          }
          //        print("(" + words(depIdx) + ", " + depType + ")    ")
        }
        //      println("")
      }
    }

  }


  /*
   * Tuples
   */

  // search for tuples of the form "from X to Y"
  def detectFromToTuples() {
    val outgoingEdges = dependencies.get.outgoingEdges

    for (i <- 0 until outgoingEdges.size) {
      val fromIdx = getDepType(outgoingEdges(i), "prep_from")
      if (fromIdx > -1) {
        val toIdx = getDepType(outgoingEdges(fromIdx), "prep_to")
        if (toIdx > -1) {
          // Store from-to tuple
          tuples.append( (TUPLE_FROMTO, Array(fromIdx, toIdx)) )

          // Set tags
          focusTag(fromIdx) = TAG_FOCUSLISTELEM
          focusTag(toIdx) = TAG_FOCUSLISTELEM
        }
      }
    }

  }

  // search for tuples of the form "X to Y to ... "
  def detectToSequenceTuples(tag:Boolean = false) {
    val outgoingEdges = dependencies.get.outgoingEdges
    val found = Array.fill[Boolean](outgoingEdges.size)(false)

    for (i <- 0 until outgoingEdges.size) {
      val indices = new ArrayBuffer[Int]()
      var idx = getDepType(outgoingEdges(i), "prep_to")
      if (idx != -1) indices.append(i)
      while (idx != -1) {
        if (found(idx) == false) {
          indices.append( idx )
          found(idx) = true               // To avoid detecting partial sequences.  E.g. for "X to Y to Z", prevents returning both "X to Y to Z" and "Y to Z"
          idx = getDepType(outgoingEdges(idx), "prep_to")
        } else {
          idx = -1
        }
      }

      // Store
      if (indices.size > 0) {
        tuples.append( (TUPLE_SEQUENCE, indices.toArray) )
        if (tag == true) {
          for (fidx <- indices) {
            focusTag(fidx) = TAG_FOCUSSEQ
          }
        }
      }
    }

  }

  /*
  def detectListTuples() {
    val found = Array.fill[Boolean](words.size)(false)

    // Step 1: Look for lists of the form "x, y, ...., and z", with a minimum of 3 list elements
    for (i <- 0 until words.size) {
      val indices = new ArrayBuffer[Int]()
      var idx = nextListElem(i)
      if (idx != -1) indices.append(i)
      while (idx != -1) {
        if (found(idx) == false) {
          indices.append( idx )
          found(idx) = true               // To avoid detecting partial sequences.  E.g. for "X to Y to Z", prevents returning both "X to Y to Z" and "Y to Z"
          idx = nextListElem(idx)
        } else {
          idx = -1
        }
      }

      // Store
      if (indices.size > 2) {
        tuples.append( (TUPLE_LIST, indices.toArray) )

        // Set tags
        for (index <- indices) {
          focusTag(index) = TAG_FOCUSLISTELEM
        }
      }
    }


    // Step 2: Look for parallel structures that suggest a short list of 2 items
    val parallelStructures = findParallelStructures( syntacticTree.get )
    for (parStruct <- parallelStructures) {
      // Check for the off case that this has already been found by the above heuristic
      var wasFound:Boolean = false
      for (idx <- parStruct) {
        if (found(idx) == true) wasFound = true
      }

      // Store
      if (!wasFound) {
        tuples.append( (TUPLE_LIST, parStruct) )

        // Set tags
        for (index <- parStruct) {
          focusTag(index) = TAG_FOCUSLISTELEM
        }
      }
    }

  }

  // Searches for parallel structures in the syntactic tree indicative of short two-element lists, e.g. NN and NN
  def findParallelStructures(node:Tree[String]):ArrayBuffer[Array[Int]] = {
    val out = new ArrayBuffer[Array[Int]]()

    // Leaf node case
    if (node.isLeaf) return out

    val children = node.children.get

    // Detection case
    if ((node.value == "NP") && (children.size == 3) && (children(1).value == "CC")) {      // e.g. X and Y
      if (children(0).value == children(2).value) {               // e.g. X and X  (such as NN and NN)
        // Parallel structure detected
        val idx1 = children(0).startOffset
        val idx2 = children(2).startOffset
        out.append( Array(idx1, idx2) )
        return out
      }
    }

    // Recursive case
    for (c <- children) {
      out.insertAll(out.size, findParallelStructures(c))
    }

    // Return
    out
  }
*/
/*
  // Returns the index of the next word in a list, if one is detected using commas and "and"s -- or -1 otherwise.
  def nextListElem(start:Int):Int = {
    val connectingWords = Array("and", "or")
    val idx = start + 1

    // Check for end of list (, and X)
    if (idx < (words.size - 2)) {
      if ((words(idx) == ",") && (connectingWords.contains(words(idx + 1)))) {
        return idx + 2
      }
    }
    // Check for start/middle of list (,)
    if (idx < (words.size - 1)) {
      if (words(idx) == ",") {
        return idx + 1
      }
    }

    // no list element found
    -1
  }
*/


  // Search 'startEdge' for a dependency of type 'findDepType' (e.g. prep_from).  If it exists, return the index of the word that the dependency points to.  Otherwise -1.
  def getDepType(startEdge:Array[(Int, String)], findDepType:String):Int = {
    for (e <- startEdge) {
      val depIdx = e._1
      val depType = e._2
      if (depType == findDepType) {
        return depIdx
      }
    }
    // Return failure
    -1
  }


  /*
   * Lists
   */


  def findLists(): Unit = {
    // Sentence/Dependency structures
    val outgoingEdges = dependencies.get.outgoingEdges
    val roots = dependencies.get.roots.toList

    // Checks
    if (outgoingEdges.size < 2) return

    // Construct all possible candidate lists
    val candidateLists = new ArrayBuffer[Array[Int]]
    for (i <- 0 until outgoingEdges.size) {
      val (candidateList, atLeastOneConj) = findListsHelper(i)
      if ((candidateList.size > 0) && (atLeastOneConj)) {
        candidateLists.append(candidateList)
      }
    }

    // Label candidate lists
    var curListIdx: Int = 1
    for (list <- candidateLists) {
      for (listIdx <- list) {
        lists(listIdx) = curListIdx
      }
      curListIdx += 1
    }

    // Store lists as tuples, and mark list elements as 'focus list elements'
    for (list <- candidateLists) {
      tuples.append((TUPLE_LIST, list))

      // Set tags
      for (index <- list) {
        focusTag(index) = TAG_FOCUSLISTELEM
      }
    }

    // Store number of lists detected
    numLists = candidateLists.size

  }

  def findListsHelper(startIdx:Int):(Array[Int], Boolean) = {
    val out = new ArrayBuffer[Int]
    // Sentence/Dependency structures
    val outgoingEdges = dependencies.get.outgoingEdges
    val roots = dependencies.get.roots.toList
    var atLeastOneConjGlobal:Boolean = false

    val listDepLabels = Array("conj_and", "conj_or")
    val listDepLabelsAlt = Array("dep")


    for (e <- outgoingEdges(startIdx)) {
      val depToIdx = e._1
      val depLabel = e._2

      // Check if startIdx is a list boundary
      if (listDepLabels.contains(depLabel) ||
          listDepLabelsAlt.contains(depLabel) ||
          ((depLabel == "appos") && isWithinParentheses(depToIdx)) )  {   // Appositions within parentheses (e.g. ... is eventually absorbed (taken in) by the ground.)
        // Ensure that the traversal index is different than the start index, or we'll infinitely recurse (infrequenty bug in stanford dependency parser)
        if (depToIdx != startIdx) {
          out.append(depToIdx)
          val (partialList, atLeastOneConj) = findListsHelper(depToIdx)
          out.insertAll(out.size, partialList)

          if (atLeastOneConj) atLeastOneConjGlobal = true
          if (listDepLabels.contains(depLabel)) atLeastOneConjGlobal = true
          if ((depLabel == "appos")) atLeastOneConjGlobal = true
        }
      }
    }

    // If the lists is non-zero size, then add the starting node
    if (out.size > 0) {
      out.insert(0, startIdx)
    }

    (removeDuplicates(out.toArray), atLeastOneConjGlobal)
  }

  // Returns true if a term is bounded by parentheses
  def isWithinParentheses(startIdx:Int):Boolean = {
    val distance:Int = 2    // Maximum distance of parentheses from startIdx

    // Check for left parenthesis
    var foundLB:Boolean = false
    for (i <- startIdx to (startIdx-distance) by -1) {
      if (i >= 0) {
        if (tags.get(i) == "-LRB-") {
          foundLB = true
        }
      }
    }

    // Check for right parenthesis
    var foundRB:Boolean = false
    for (i <- startIdx to (startIdx+distance)) {
      if (i < words.size) {
        if (tags.get(i) == "-RRB-") {
          foundRB = true
        }
      }
    }

    if (foundLB && foundRB) {
      return true
    }
    false
  }

  /*
   * Clause boundaries
   */

  // Populates the clauses[Int] array, with a unique integer identifier for each clause
  def findClauseBoundaries() {
    // Sentence/Dependency structures
    val outgoingEdges = dependencies.get.outgoingEdges
    val roots = dependencies.get.roots.toList

    // Checks
    if (outgoingEdges.size < 2) return
    if (roots.size == 0) return

    // Traverse
    var done:Boolean = false
    var curClauseIdx:Int = 1
    val rootIdx = roots(0)
    var toTraverse = new ArrayBuffer[(Int, Int)]    // (to, from)
    toTraverse.append( (rootIdx, -1) )     // Start at root  (from = -1, to siginify root)

    while (toTraverse.size > 0) {
      // Pop off next element to traverse
      val startIdx = toTraverse(0)._1   // to
      val fromIdx = toTraverse(0)._2    // from
      toTraverse.remove(0)

      val (withinClause, clauseBoundaries) = traverseUntilClauseBoundaryHelper(startIdx, rootIdx)
//      println ("traverseUntilClauseBoundary (within): " + withinClause.toList)
//      println ("traverseUntilClauseBoundary (boundaries): " + clauseBoundaries.toList)

      // Assign all elements in the current clause
      for (withinIdx <- withinClause) {
        clauses(withinIdx) = curClauseIdx
      }
      curClauseIdx += 1

      // Record where the link between these two clauses originated
      clauseLinks.append( (fromIdx, startIdx) )

      // Add any clause boundaries to the list of indicies to traverse
      toTraverse.insertAll(toTraverse.size, clauseBoundaries)
    }

    // Store number of clauses detected
    numClauses = curClauseIdx - 1
  }

  def traverseUntilClauseBoundaryHelper(startIdx:Int, rootIdx:Int):(Array[Int], Array[(Int, Int)]) = {
    // Included in clause: nsubj, dobj, prep_*, etc
    val outClause = new ArrayBuffer[Int]
    // Clause boundaries: advcl, xcomp, ccomp, etc
    val outBoundaries = new ArrayBuffer[(Int, Int)]     // (to, from).  Allows tracking the source of links between clauses

    // Sentence/Dependency structures
    val outgoingEdges = dependencies.get.outgoingEdges

    // Add current startIdx to the clause
    outClause.append( startIdx )

    var hasCopularDep = false
    var nsubjIdx = -1

    // Search outward on the dependencies from the startIdx
    for (e <- outgoingEdges(startIdx)) {
      val depToIdx = e._1
      val depLabel = e._2
      val lemmaTo = lemmas.get(depToIdx)

      // Ensure that the traversal index is different than the start index, or we'll infinitely recurse (infrequent bug in stanford dependency parser)
      if (depToIdx != startIdx) {

        if (defWordLabels.contains(depLabel)) {
          nsubjIdx = depToIdx
        }

        if (copulaLabels.contains(depLabel) && lemmaTo == "be") {
          hasCopularDep = true
        }

        if (hasCopularDep && nsubjIdx != -1) {
          outBoundaries.append((nsubjIdx, startIdx))
          hasCopularDep = false
          nsubjIdx = -1
        }

        // Check if startIdx is a clause boundary
        // Case 1: On a normal clause boundary as identified by the clauseBoundaryLabels array (advcl, ccomp, xcomp, etc)
        if (clauseBoundaryLabels.contains(depLabel)) {
          outBoundaries.append((depToIdx, startIdx))
          //        print ("(" + startIdx + "," + lemmas.get(startIdx) + "): ")
          //        println ("\t(" + depToIdx + "," + lemmas.get(depToIdx) + "," + depLabel + ") BOUNDARY")

          // Case 2: A conjunction is present on the root of the sentence (e.g. "Plants take in carbon dioxide and give off oxygen")
        } else if (depLabel.startsWith("conj") && (startIdx == rootIdx)) {
          outBoundaries.append((depToIdx, startIdx))

          // Case 3: Not on a clause boundary
        } else {
          // Not a boundary
          outClause.append(depToIdx)
          //        print ("(" + startIdx + "," + lemmas.get(startIdx) + "): ")
          //        println ("\t(" + depToIdx + "," + lemmas.get(depToIdx) + "," + depLabel + ") WITHIN")
          val (withinClause, clauseBoundaries) = traverseUntilClauseBoundaryHelper(depToIdx, rootIdx)
          outClause.insertAll(outClause.size, withinClause)
          outBoundaries.insertAll(outBoundaries.size, clauseBoundaries)
        }
      }
    }

    (removeDuplicates(outClause.toArray), removeDuplicates(outBoundaries.toArray))
  }

  /*
   * Supporting functions
   */

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


  def removeDuplicates(in:Array[(Int, Int)]):Array[(Int, Int)] = {
    val out = new ArrayBuffer[(Int, Int)]()
    for (i <- 0 until in.size) {
      var found:Boolean = false
      for (j <- 0 until out.size) {
        if (in(i) == out(j)) found = true
      }

      if (!found) out.append( in(i) )
    }
    out.toArray
  }

  /*
   * Ranking
   */

  def incrementRanksWithTag(findTag:String, minValue:Int = 1) {
    var curRank = findMaxRank + 1

    // Bound checking
    if (curRank < minValue) curRank = minValue

    // Find answer type words, and rank them at the current (low) value.
    for (i <- 0 until focusTag.size) {
      val fwTag = focusTag(i)
      if (fwTag == findTag) {
        ranks(i) = curRank
      }
    }
  }

  // Rank words filtered out by concreteness in order of how close they are to the concreteness filter border
  def setRanksAnswerType() {
    var curRank = findMaxRank + 1

    // Bound checking
    if (curRank < 0) curRank = 0

    // Find answer type words, and rank them at the current (low) value.
    for (i <- 0 until focusTag.size) {
      val fwTag = focusTag(i)
      if (fwTag == TAG_ANSWERTYPEWORD) {
        ranks(i) = curRank
      }
    }
  }

  // Rank words filtered out by concreteness in order of how close they are to the concreteness filter border
  def setRanksConcreteness() {
    var curRank = findMaxRank + 1
    val midpoint = (CONCRETENESS_FILTERHIGH - CONCRETENESS_FILTERLOW)/2 + CONCRETENESS_FILTERLOW

    // Bound checking
    if (curRank < 1) curRank = 1

    // Find abstract/concrete words that have been filtered out, and determine how far they are from the concreteness filter border
    val items = new ArrayBuffer[(Int, Double)]    // (Index, deltaScore)
    for (i <- 0 until concreteness.size) {
      val fwTag = focusTag(i)
      val deltaScore = abs(midpoint - concreteness(i))    // Absolute value, so that concrete and abstract words are weighted equally
      if ((fwTag == TAG_ABSTRACT) || (fwTag == TAG_EXAMPLE)) {
        items.append( (i, deltaScore) )
      }
    }

    // Sort
    val sorted = items.sortBy(- _._2)

    // Assign ranks
    for (i <- 0 until sorted.size) {
      val idx = sorted(i)._1
      ranks(idx) = curRank
      curRank += 1
    }

  }


  def findMaxRank:Int = {
    var max:Int = ranks(0)
    for (i <- 0 until ranks.size) {
      if (ranks(i) > max) max = ranks(i)
    }
    max
  }

  // Returns a sorted list of all words with a rank of zero or greater
  def getRankedFocusWords:Array[(String, String, Int)] = {
    val out = new ArrayBuffer[(String, String, Int)]()
    for (i <- 0 until words.size) {
      val word = words(i).toLowerCase
      val lemma = lemmas.get(i).toLowerCase
      val rank = ranks(i)

      if (rank >= 0) {
        out.append( (word, lemma, rank) )
      }
    }

    out.sortBy(-_._3).toArray
  }


  def populateFocusCategories() {
    val itemsLists = new ArrayBuffer[(String, Int, Int)]
    val itemsFocus = new ArrayBuffer[(String, Int, Int)]
    val itemsBackoff = new ArrayBuffer[(String, Int, Int)]
    val itemsAnswerType = new ArrayBuffer[(String, Int, Int)]

    // Step 1: Collect
    for (idx <- 0 until ranks.size) {
      val lemma = lemmas.get(idx).toLowerCase
      val rank = ranks(idx)
      val fwTag = focusTag(idx)

      if ((fwTag == TAG_FOCUS) || (fwTag == TAG_UNIFORM)) {
        itemsFocus.append( (lemma, idx, rank) )
      } else if ((fwTag == TAG_FOCUSLISTELEM) || (fwTag == TAG_FOCUSFROMTO) || (fwTag == TAG_FOCUSSEQ)) {
        itemsLists.append( (lemma, idx, rank) )
      } else if (fwTag == TAG_ANSWERTYPEWORD) {
        itemsAnswerType.append( (lemma, idx,rank) )
      } else if (rank >= 1) {
        itemsBackoff.append( (lemma, idx, rank) )
      }
    }

    // Step 2: Sort
    val sortedFocus = itemsFocus.sortBy(-_._3)      // Currently unordered?
    val sortedBackoff = itemsBackoff.sortBy(-_._3)

    // Step 3: Store
    focusListElems = itemsLists.toArray
    focusPrimary = sortedFocus.toArray
    focusBackoff = sortedBackoff.toArray
    focusAnswerType = itemsAnswerType.toArray

  }

  /*
   * Baseline methods
   */
  // Populate all content words with a uniform weight and label
  def uniformWeightBaseline() {
    for (i <- 0 until tags.get.size) {
      val tag = tags.get(i)
      val lemma = lemmas.get(i)
      if (tag.startsWith("NN") || tag.startsWith("VB") || tag.startsWith("JJ") || tag.startsWith("RB")) {
        focusTag(i) = TAG_UNIFORM
        ranks(i) = 1
      } else {
        focusTag(i) = TAG_STOP
        ranks(i) = -1
      }
    }
  }

  /*
   * Display
   */

  def rankedWordsString:String = {
    val os = new StringBuilder

    // Step 1: Make string
    os.append ("List elements: ")
    if (focusListElems.size > 0) {
      for (i <- 0 until focusListElems.size) {
        os.append( focusListElems(i)._1 + " ")
      }
    } else {
      os.append(" NONE ")
    }
    os.append ("\n")

    os.append ("Ranked focus words: ")
    if (focusPrimary.size > 0) {
      for (i <- 0 until focusPrimary.size) {
        os.append( focusPrimary(i)._1 + " ")
      }
    } else {
      os.append(" NONE ")
    }
    os.append ("\n")

    os.append ("Ranked back-off words: ")
    if (focusBackoff.size > 0) {
      for (i <- 0 until focusBackoff.size) {
        os.append( focusBackoff(i)._1 + " ")
      }
    } else {
      os.append(" NONE ")
    }
    os.append ("\n")

    os.append ("Answer-type words: ")
    if (focusAnswerType.size > 0) {
      for (i <- 0 until focusAnswerType.size) {
        os.append( focusAnswerType(i)._1 + " ")
      }
    } else {
      os.append(" NONE ")
    }
    os.append ("\n")

    // Return
    os.toString
  }

  override def toString():String = {
    val os = new StringBuilder
    val osWords = new StringBuilder
    val osLemmas = new StringBuilder
    val osTags = new StringBuilder
    val osEntities = new StringBuilder
    val osFWTags = new StringBuilder
    val osConc = new StringBuilder
    val osClause = new StringBuilder
    val osList = new StringBuilder
    val osRank = new StringBuilder

    /*
    // (Optional -- debug)
    os.append ("Syntactic tree: ")
    os.append( syntacticTree.get.toString() + "\n")
    os.append ("Dependencies: ")
    os.append( displayDependencies(this) + "\n" )
    */

    osWords.append( constLen("Words:", 10) )
    osLemmas.append( constLen("Lemmas:", 10) )
    osTags.append( constLen("Tags:", 10) )
    osEntities.append( constLen("NEnt:", 10) )
    osFWTags.append( constLen("FWTags:", 10) )
    osConc.append( constLen("Conc:", 10) )
    osClause.append( constLen("Clause:", 10) )
    osList.append( constLen("List:", 10) )
    osRank.append( constLen("Rank:", 10) )


    for (i <- 0 until words.size) {
      val word = words(i)
      val lemma = lemmas.get(i)
      val tag = tags.get(i)
      val namedEnt = entities.get(i)
      val fwTag = focusTag(i)
      val conc = concreteness(i)
      val clause = clauses(i)
      val list = lists(i)
      val rank = ranks(i)

      val length = word.length + 6

      osWords.append( constLen(word, length) )
      osLemmas.append( constLen(lemma, length) )
      osTags.append( constLen(tag, length) )
      osEntities.append( constLen(namedEnt, length) )
      osFWTags.append( constLen(fwTag, length) )
      osClause.append( constLen(clause.toString, length) )
      osList.append( constLen(list.toString, length) )
      osRank.append( constLen(rank.toString, length) )

      var concFlag:String = "*"
      if (conc < CONCRETENESS_FILTERLOW) concFlag = "A"
      if (conc > CONCRETENESS_FILTERHIGH) concFlag = "C"
      osConc.append( constLen(conc.formatted("%1.2f") + concFlag, length) )

    }

    os.append( osWords + "\n" )
    os.append( osLemmas + "\n" )
    os.append( osTags + "\n" )
    os.append( osEntities + "\n" )
    os.append( osFWTags + "\n" )
    os.append( osConc + "\n" )
    os.append( osClause + "\n" )
    os.append( osList + "\n" )
    os.append( osRank + "\n" )

    os.append ("\n")
    os.append(rankedWordsString)

    // Tuples
    os.append (constLen("Tuples:", 10) )
    if (tuples.size > 0) {
      for (i <- 0 until tuples.size) {
        val tupleType = tuples(i)._1
        val tupleElems = tuples(i)._2
        os.append ("[" + tupleType + " (")
        for (j <- 0 until tupleElems.size) {
          val tupleIdx = tupleElems(j)
          val lemma = lemmas.get(tupleIdx)
          os.append (lemma)
          if (j < (tupleElems.size-1)) os.append(" ")
        }
        os.append(")] ")
      }
    } else {
      os.append (" NONE ")
    }
    os.append ("\n")

    // Clause links
    os.append (constLen("Clause links: ", 10))
    if (clauseLinks.size > 0) {
      for (i <- 0 until clauseLinks.size) {
        val fromIdx = clauseLinks(i)._1
        val toIdx = clauseLinks(i)._2

        os.append("(")
        if (fromIdx >= 0) {
          os.append(words(fromIdx) + ",")
        } else {
          os.append("ROOT,")
        }
        os.append(fromIdx.toString())
        os.append(" -> ")
        if (toIdx >= 0) {
          os.append(words(toIdx) + ",")
        } else {
          os.append("ROOT,")
        }
        os.append(toIdx.toString())
        os.append(")\t")
      }

    } else {
      os.append (" NONE ")
    }
    os.append ("\n")

    os.toString
  }


  def constLen(in:String, length:Int):String = {
    val os = new StringBuilder
    os.append(in)
    for (i <- 0 until (length - in.size)) {
      os.append(" ")
    }
    os.toString
  }


}


object FocusSentence {
  val logger = LoggerFactory.getLogger(classOf[FocusSentence])

  /*
   * Operating modes
   */
  val FOCUS_NORMAL          = 1
  val BASELINE_UNIFORM      = 2
  val BASELINE_IDF          = 3

  /*
   * Focus word category tags
   */
  val TAG_STOP              = "STOP"
  val TAG_STOPPH            = "STOPPH"
  val TAG_ANSWERTYPEWORD    = "ATYPE"
  val TAG_ANSWERTYPEWORDMOD = "ATYPEM"
  val TAG_TRANSPARENT       = "TRAN"
  val TAG_EXAMPLE           = "EX"
  val TAG_EXAMPLENAMEDENT   = "EX_NE"
  val TAG_ABSTRACT          = "ABS"
  val TAG_FOCUS             = "FOCUS"
  val TAG_FOCUSLISTELEM     = "FW_LE"
  val TAG_FOCUSFROMTO       = "FW_FT"
  val TAG_FOCUSSEQ          = "FW_SEQ"

  // Uniform (baseline)
  val TAG_UNIFORM           = "UNI"

  /*
   * Tuple types
   */
  val TUPLE_LIST            = "LIST"
  val TUPLE_SEQUENCE        = "SEQ"
  val TUPLE_FROMTO          = "FROMTO"

  /*
   * Concreteness score range
   */
  val CONCRETENESS_FILTERHIGH = 4.25
  val CONCRETENESS_FILTERLOW  = 3.00

  /*
   * Clause boundary dependency labels
   */
  val clauseBoundaryLabels = Array("advcl", "xcomp", "ccomp", "rcmod", "parataxis",
                                   "prep_with", "prep_through", "prep_from", "prep_to", "prep_as", "prep_into", "prep_by",
                                   "prep_in", "prep_such_as", "prep_because_of", "prep_over", "prep_on", "prep_between",
                                   "prepc_as", "prepc_by", "prepc_of", "prepc_with", "prepc_after", "vmod", "prepc_for",
                                   "prepc_before", "prep_before", "prep_after", "prep_during", "prepc_during", "prepc_because_of",
                                   "prep_without", "prepc_without")


  /*
   * Named-entity example categories
   */
  val namedEntityExampleCategories = Array("LOCATION", "DURATION")


  /*
   * Transparent words
   */
  val transparentNouns = Array("kind", "type", "part", "form")



  /*
   * Stop Words
   */
  val nonStopTags   = Array("NN", "VB", "JJ", "RB")
  val stopGeneral   = Array("it", "its", "they", "best", "worst", "everything", "certain", "main" , "s")

  // QA-specific stop words
  val stopQA        = Array("example", "describe", "call", "show", "make", "object", "one", "way", "method", "determine", "help")   // function
  val stopQAStarts  = Array("who", "what", "where", "why", "when", "which", "how")

  // Grounded-example stop words
  val stopGrounded  = Array("person", "human")

  // Type/form/kind
  val stopTransparent = Array("kind", "type", "part", "form", "go", "act", "affect")

  // Verbs
  val stopVerbs     = Array("are", "be", "do", "get", "use", "is", "have")

  // Numbers/quantifiers
  val stopQuant     = Array("most", "many", "some", "likely", "usually", "often", "mostly", "small", "large")

  // Other
  val stopOther     = Array("come", "from", "back", "able", "something", "someone")

  // Barron's inference stop words
  val stopBarrons   = Array("other", "say", "also", "consider", "process", "thing", "own", "place", "take", "term", "help")

  // Combined list of stop words for QA focus word extraction
  val stopWordsFocus = stopGeneral ++ stopQA ++ stopQAStarts ++ stopGrounded ++ stopTransparent ++ stopVerbs ++ stopQuant ++ stopOther ++ stopBarrons

  /*
   * Stop Phrases
   */
  val stopPhrases = new ArrayBuffer[Array[String]]()

  stopPhrases.append( Array("in", "order", "to") )
  stopPhrases.append( Array("does", "it", "take") )     // e.g. How long does it take ...
  stopPhrases.append( Array("over", "time") )           // e.g. Over time, these decay into ...

  /*
    * Definition word dependency labels
   */

  val defWordLabels = Array("nsubj", "nsubjpass")
  val copulaLabels = Array ("cop", "aux", "auxpass")



// Temporary

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

    /*
    os.append("incoming:\n")
    n = 0
    while(n < dependencies.size) {
      os.append("\t" + n + ":")
      for (e <- incomingEdges(n)) {
        //os.append(" " + e)
        val depIdx = e._1
        val depType = e._2
        os.append ("(" + depIdx + "," + lemmas(depIdx) + "," + depType + ") ")
      }
      os.append("\n")
      n += 1
    }
    */

    os.toString()

  }


}