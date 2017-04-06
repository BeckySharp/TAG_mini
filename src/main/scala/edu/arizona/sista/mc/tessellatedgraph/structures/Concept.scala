package edu.arizona.sista.mc.tessellatedgraph.structures

import scala.collection.mutable.ArrayBuffer

/**
 * Created by peter on 1/14/15.
 */
class Concept(val termsIn:Array[Term]) extends Serializable {

  // Alternative constructor for when we're populating a concept with just a term instead of a list
  def this(term:Term) = {
    this(Array(term))
  }

  // Constructor
  val terms = new ArrayBuffer[Term]
  terms.insertAll(0, termsIn)       // Convert terms to ArrayBuffer. TODO: Fix double storing, alternate constructor issue



  /*
   * Accessor methods
   */

  def removeTerm(in:Term): Unit = {
    var idx:Int = 0
    while(idx < terms.size) {
      if (terms(idx) == in) {
        terms.remove(idx)
      } else {
        idx += 1
      }
    }
  }

  def isEmpty:Boolean = {
    if (terms.size == 0) return true
    false
  }


  /*
   * Save methods
   */
  def saveToString():String = {
    val os = new StringBuilder

    // Size
    os.append(terms.size + "\n")
    // Terms
    for (term <- terms) {
      os.append( term.saveToString() + "\n")
    }

    os.toString
  }


  /*
   * String methods
   */


  override def toString:String = {
    val os = new StringBuilder

    // Case: Empty concept
    if (terms.size == 0) {
      os.append("EMPTY ")
      return os.toString()
    }

    if (terms.size > 1) {
      // Case: multiple terms
      os.append("LIST: \t")
    } else if (terms.size == 1) {
      // Case: One term
      os.append("TERM: \t")
    }

    for (i <- 0 until terms.size) {
      os.append(i + ":" + terms(i).toString + "\t" )
    }

    os.toString()
  }

}


object Concept {
  // TODO: Load

}