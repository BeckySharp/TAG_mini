package edu.arizona.sista.mc.tessellatedgraph.structures

/**
 * Storage class for a single raw input sentence to the Clauseinator
 * Created by peter on 2/12/15.
 */

class CLInputSentence(val docid:String, val text:String, val source:String,
                      val definedWord:String, val definedWordPOS:String) {

  def this(docid:String, text:String, source:String) = this(docid, text, source, "", "")

  def isDefinition:Boolean = {
    if (definedWord.length > 0) return true
    false
  }

}