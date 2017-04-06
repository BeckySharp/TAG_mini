package edu.arizona.sista.mc.tessellatedgraph.structures

import edu.arizona.sista.mc.tessellatedgraph.Clauseinator.CLUtils
import org.clulab.struct.Lexicon

import scala.collection.mutable.ArrayBuffer

/**
 * Created by peter on 1/14/15.
 */
class Term(val words:Array[String],
            val lemmas:Array[String],
            val tags:Array[String],
            val originalIndicies:Array[Int],
            var lexiconIDs:Array[Int]) extends Serializable {

  // Alternate constructor: Instantitate with single words
  def this(word:String, lemma:String, tag:String, originalIndex:Int, lexiconID:Int) = {
    this(Array(word), Array(lemma), Array(tag), Array(originalIndex), Array(lexiconID))
  }

  // Check that all arrays are of equal size
  assert(lemmas.size == words.size)
  assert(tags.size == words.size)


  def isEmpty:Boolean = {
    if (words.size == 0) return true
    false
  }


  /*
   * Update
   */
  def updateWithNewLexicon(lexicon:Lexicon[String]) {
    for (i <- 0 until lemmas.size) {
      val lemma = lemmas(i)
      val tag = tags(i)
      val tLemma = CLUtils.mkTLemma(tag, lemma)
      lexiconIDs(i) = lexicon.add( tLemma )
    }
  }


  /*
   * Save
   */
  def saveToString():String = {
    val os = new StringBuilder
    os.append( mkCSVFromArray(words) + "\t")
    os.append( mkCSVFromArray(lemmas) + "\t")
    os.append( mkCSVFromArray(tags) + "\t")
    os.append( mkCSVFromArray(originalIndicies) + "\t")
    os.append( mkCSVFromArray(lexiconIDs) )
    os.toString()
  }

  /*
   * Save (supporting functions)
   */

  def mkCSVFromArray(in:Array[String]):String = {
    val os = new StringBuilder
    for (i <- 0 until in.size) {
      os.append (in(i))
      if (i < (in.size - 1)) os.append (",")
    }
    os.toString()
  }

  def mkCSVFromArray(in:Array[Int]):String = {
    val os = new StringBuilder
    for (i <- 0 until in.size) {
      os.append (in(i))
      if (i < (in.size - 1)) os.append (",")
    }
    os.toString()
  }

  /*
   * String
   */

  def mkWordsString() = {
    val os = new StringBuilder
    for (word <- words) os.append(words + " ")
    os.toString().trim()
  }

  def mkLemmasString() = {
    val os = new StringBuilder
    for (lemma <- lemmas) os.append(lemma + " ")
    os.toString().trim()
  }

  def mkTaggedLemmas():Array[String] = {
    val out = new ArrayBuffer[String]
    for (i <- 0 until lemmas.size) {
      out.append(tags(i) + "_" + lemmas(i))
    }
    out.toArray
  }

  def mkTaggedLemmaString():String = {
    val os = new StringBuilder
    val tLemmas = mkTaggedLemmas()
    for (tLemma <- tLemmas) {
      os.append(tLemma + " ")
    }
    os.toString.trim()
  }

  override def toString:String = {
    val os = new StringBuilder

    for (i <- 0 until words.size) {
      os.append("(")
      os.append(words(i) + "," + lemmas(i) + "," + tags(i) + "," + originalIndicies(i))
      os.append(",L" + lexiconIDs(i))
      os.append(") ")
    }

    os.toString()
  }

}


object Term {

  /*
   * Loading
   */
  def parseFromString(in:String):Term = {
    val split = in.split("\t")
    if (split.size != 5) throw new RuntimeException("Term.parseFromString: ERROR: Input string has a different number of fields than expected (" + split.size + ") [String = " + in + "]")

    val words = loadStringArrayFromCSVString( split(0) )
    val lemmas = loadStringArrayFromCSVString( split(1) )
    val tags = loadStringArrayFromCSVString( split(2) )
    val originalIndicies = loadIntArrayFromCSVString( split(3) )
    val lexiconIDs = loadIntArrayFromCSVString( split(4) )

    // Return
    new Term(words, lemmas, tags, originalIndicies, lexiconIDs)
  }

  /*
   * Loading (supporting functions)
   */

  def loadStringArrayFromCSVString(in:String):Array[String] = {
    in.split(",")
  }

  def loadIntArrayFromCSVString(in:String):Array[Int] = {
    val stringArray = in.split(",")
    val out = new Array[Int](stringArray.size)
    for (i <- 0 until stringArray.size) {
      out(i) = stringArray(i).toInt
    }
    out
  }
}
