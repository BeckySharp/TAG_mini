package edu.arizona.sista.mc.tessellatedgraph.structures

import org.clulab.struct.Lexicon

import scala.collection.mutable.ArrayBuffer
import Clause._
import ClauseLink._

/**
 * Created by peter on 1/14/15.
 */
class Schema(val originalText:String,
             val originalCorpus:String,
             val originalDocID:String,
             val category:String) extends Serializable {

  val clauses = new ArrayBuffer[Clause]
  val links = new ArrayBuffer[ArrayBuffer[ClauseLink]]


  /*
   * Clauses
   */
  // Add a clause to the list.  Returns the index of the new clause in 'clauses' array
  def addClause(in:Clause):Int = {
    clauses.append(in)
    links.append( new ArrayBuffer[ClauseLink])
    // Return index of new clause
    clauses.size-1
  }

  // Remove a clause from the list, as well as any links it may have.
  // Automatically repairs broken links.  E.g. in A->B->C, if B is removed, then will link A->C.
  def removeClause(in:Clause) = {
    val linksTo = new ArrayBuffer[ClauseLink]
    val linksFrom = new ArrayBuffer[ClauseLink]
    val removeIdx = getClauseIdx(in)

    // Step 1: Search for links to clause
    for (i <- 0 until links.size) {
      if (i != removeIdx) {
        var idx:Int = 0
        while (idx < links(i).size) {
          val link = links(i)(idx)
          if (link.destination == in) {
            // Pop off link and move it to another list
            linksTo.append(link)
            links(i).remove(idx)
          } else {
            idx += 1
          }
        }
      }
    }

    // Step 2: Search for links from clause
    var idx:Int = 0
    for (link <- links(removeIdx)) {
      linksFrom.append(link)
    }

    // Step 3: Repair links
    // Case 1 (normal): Links incoming and outgoing (e.g. A -> B -> C, removing B)
    // Make a new link from each source link to each destination link
    if ((linksTo.size > 0) && (linksFrom.size > 0)) {
      for (linkTo <- linksTo) {
        for (linkFrom <- linksFrom) {
          // Use original information from source link, only modify with destination link
          if (linkTo.source != linkFrom.destination) {
            // No self-linking
            addLink(linkTo.source, linkFrom.destination, linkTo.label, linkTo.bidirectionalFlag)
          }
        }
      }
    }
    // Case 2 (special): Links only incoming (e.g. A-> B <- C, removing B)
    // Here we connect A to C.  This is non-ideal, but better than having unconnected nodes.
    if ((linksTo.size > 1) && (linksFrom.size == 0)) {
      val populatedLinks = new ArrayBuffer[(Clause, Clause)]
      for (linkTo <- linksTo) {
        for (linkFrom <- linksTo) {
          // Use original information from source link, only modify with destination link
          if (linkTo != linkFrom) {
            // No self-linking
            var found:Boolean = false
            for (populatedLink <- populatedLinks) {
              if ((populatedLink._1 == linkTo.source) && (populatedLink._2 == linkFrom.source)) found = true
              if ((populatedLink._2 == linkTo.source) && (populatedLink._1 == linkFrom.source)) found = true
            }
            if (!found) {
              addLink(linkTo.source, linkFrom.source, linkTo.label, linkTo.bidirectionalFlag)
              populatedLinks.append( (linkTo.source, linkFrom.source) )
            }

          }
        }
      }

    }
    // Case 3 (special): Links only outgoing (e.g. A -> B, A -> C, removing A)
    // Here we connect B to C.  This is non-ideal, but better than having unconnected nodes.
    if ((linksTo.size == 0) && (linksFrom.size > 1)) {
      val populatedLinks = new ArrayBuffer[(Clause, Clause)]
      for (linkTo <- linksFrom) {
        for (linkFrom <- linksFrom) {
          // Use original information from source link, only modify with destination link
          if (linkTo != linkFrom) {
            // No self-linking
            var found:Boolean = false
            for (populatedLink <- populatedLinks) {
              if ((populatedLink._1 == linkTo.source) && (populatedLink._2 == linkFrom.source)) found = true
              if ((populatedLink._2 == linkTo.source) && (populatedLink._1 == linkFrom.source)) found = true
            }
            if (!found) {
              addLink(linkTo.destination, linkFrom.destination, linkTo.label, linkTo.bidirectionalFlag)
              populatedLinks.append( (linkTo.source, linkFrom.source) )
            }
          }
        }
      }
    }


    // Step 4: Remove clause & it's links
    clauses.remove(removeIdx)
    links.remove(removeIdx)

  }

  // Return the index of 'in:Clause' in the 'clauses' array
  def getClauseIdx(in:Clause):Int = {
    for (i <- 0 until clauses.size) {
      if (clauses(i) == in) {
        return i
      }
    }
    -1
  }

  /*
   * Links
   */
  // Add a link between two clauses
  // Automatically creates bidirectional links, if isBidirectional = true
  def addLink(from:Clause, to:Clause, linkLabel:String, isBidirectional:Boolean = false) {
    addLinkHelper(from, to, linkLabel, isBidirectional)
    if (isBidirectional) {
      addLinkHelper(to, from, linkLabel, isBidirectional)
    }
  }

  // Add link between two clauses (simplified, with default unknown link type)
  def addLink(from:Clause, to:Clause):Unit = {
    addLinkHelper(from, to, CL_UNKNOWN, isBidirectional = false)
  }

  // Helper for addLink.  Should not be called to add links -- use addLink()
  private def addLinkHelper(from:Clause, to:Clause, linkLabel:String, isBidirectional:Boolean = false) = {
    val fromIdx = getClauseIdx(from)
    val toIdx = getClauseIdx(to)

    // Add new link, and store in links(fromIdx)
    val newLink = new ClauseLink(from, to, label = linkLabel, bidirectionalFlag = isBidirectional)
    links(fromIdx).append(newLink)
  }


  // Return the list of ClauseLinks for a given clause
  def getLinks(from:Clause):Array[ClauseLink] = {
    links( getClauseIdx(from) ).toArray
  }

  // Removes link(s) between two clauses
  def removeLink(from:Clause, to:Clause) = {
    val fromIdx = getClauseIdx(from)

    // Note: There should not be more than one link between the same set of clauses, but incase there is
    var idx:Int = 0
    while (idx < links(fromIdx).size) {
      val link = links(fromIdx)(idx)
      if (link.destination == to) {
        links(fromIdx).remove(idx)
      } else {
        idx += 1
      }
    }
  }

  /*
   * Supporting functions
   */

  // When combining lists of Schemas that were generated at different times, this function updates the lexicon indicies
  // in each of the Terms to agree with the new, common lexicon
  def updateWithNewLexicon(lexicon:Lexicon[String]) {
    for (clause <- clauses) {
      for (conceptRolePair <- clause.concepts) {
        val concept = conceptRolePair._1
        val role = conceptRolePair._2
        for (term <- concept.terms) {
          term.updateWithNewLexicon(lexicon)
        }
      }
    }
  }

  override def toString:String = {
    val os = new StringBuilder

    os.append("SCHEMA: Text: " + originalText + " (category: " + category + ") [" + originalCorpus + " / docid:" + originalDocID + "] \n")

    os.append("Clauses: (size = " + clauses.size + ")\n")
    for (i <- 0 until clauses.size) {
      // Display clause
      os.append("  Clause " + i + ": " + clauses(i).toString)

      // Display links from clause
      os.append("  Links outgoing: ")
      val linksFromClause = getLinks(clauses(i))
      if (linksFromClause.size == 0) {
        os.append("\tNONE")
      } else {
        for (j <- 0 until linksFromClause.size) {
          val link = linksFromClause(j)
          val toLinkIdx = getClauseIdx(link.destination)
          os.append("\t" + j + ":(CL" + toLinkIdx + "," + link.label + ",")
          if (link.bidirectionalFlag) {
            os.append("BIDIRECTIONAL")
          } else {
            os.append("UNIDIRECTIONAL")
          }
          os.append(") ")
        }
      }
      os.append("\n\n")
    }

    os.toString()
  }

}



object Schema {

  // Test
  def main(args:Array[String]): Unit = {
    // Quick test

    val term1 = new Term("trees", "tree", "NN", 0, 0)
    val term2 = new Term("have", "have", "VB", 1, 1)
    val term3 = new Term("leaves", "leaf", "NN", 2, 2)
    val term4 = new Term("bark", "bark", "NN", 3, 3)

    val term5 = new Term("produce", "produce", "VB", 4, 4)
    val term6 = new Term("fruit", "fruit", "NN", 5, 5)

    val term7 = new Term("need", "need", "VB", 6, 6)
    val term8 = new Term("sunlight", "sunlight", "NN", 7, 7)

    val term9 = new Term("grow", "grow", "VB", 8, 8)
    val term10 = new Term("tall", "tall", "NN", 9, 9)

    val concept1 = new Concept(term1)
    val concept2 = new Concept(term2)
    val concept3 = new Concept( Array(term3, term4) )

    val concept4 = new Concept(term5)
    val concept5 = new Concept(term6)

    val concept6 = new Concept(term7)
    val concept7 = new Concept(term8)

    val concept8 = new Concept(term9)
    val concept9 = new Concept(term10)

    // Pack
    val concepts1 = new ArrayBuffer[(Concept, String)]
    concepts1.append( (concept1, ROLE_AGENT) )
    concepts1.append( (concept2, ROLE_VERB) )
    concepts1.append( (concept3, ROLE_PATIENT) )

    val concepts2 = new ArrayBuffer[(Concept, String)]
    concepts2.append( (concept4, ROLE_VERB) )
    concepts2.append( (concept5, ROLE_AGENT) )

    val concepts3 = new ArrayBuffer[(Concept, String)]
    concepts3.append( (concept6, ROLE_VERB) )
    concepts3.append( (concept7, ROLE_AGENT) )

    val concepts4 = new ArrayBuffer[(Concept, String)]
    concepts4.append( (concept8, ROLE_VERB) )
    concepts4.append( (concept9, ROLE_AGENT) )

    // Create clause
    val clause1 = new Clause( concepts1.toArray, clauseType = CTYPE_OTHER, negationFlag = false, transitiveVerbFlag = false )
    val clause2 = new Clause( concepts2.toArray, clauseType = CTYPE_OTHER, negationFlag = false, transitiveVerbFlag = false )
    val clause3 = new Clause( concepts3.toArray, clauseType = CTYPE_OTHER, negationFlag = false, transitiveVerbFlag = false )
    val clause4 = new Clause( concepts4.toArray, clauseType = CTYPE_OTHER, negationFlag = false, transitiveVerbFlag = false )

    // Create schema
    val schema1 = new Schema("Trees have leaves etc", "SOURCE", "DOCID", "BOTANY")
    schema1.addClause(clause1)
    schema1.addClause(clause2)
    schema1.addClause(clause3)
    schema1.addClause(clause4)

    schema1.addLink(clause1, clause2, CL_UNKNOWN, false)
    schema1.addLink(clause2, clause3)
    schema1.addLink(clause4, clause2)

    // Display
    //println ( clause1.toString )
    println ( schema1.toString )

    println (" =========================================================================== ")
    println ("After removal: ")
    schema1.removeClause(clause2)
    println ( schema1.toString )

  }

}
