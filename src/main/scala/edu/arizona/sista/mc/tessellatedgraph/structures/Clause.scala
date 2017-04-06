package edu.arizona.sista.mc.tessellatedgraph.structures

import scala.collection.mutable.ArrayBuffer

/**
 * Created by peter on 1/14/15.
 */
class Clause(val conceptsIn:Array[(Concept, String)],     // Concept/Role pair
              var clauseType:String,
              val negationFlag:Boolean,
              val transitiveVerbFlag:Boolean) extends Serializable {

  // Constructor
  val concepts = new ArrayBuffer[(Concept, String)]
  concepts.insertAll(0, conceptsIn)       // Convert concepts to ArrayBuffer. TODO: Fix double storing, alternate constructor issue


  /*
   * Accessor methods
   */
  def removeConcept(in:Concept): Unit = {
    var idx:Int = 0
    while (idx < concepts.size) {
      if (concepts(idx)._1 == in) {
        concepts.remove(idx)
      } else {
        idx += 1
      }
    }
  }


  def isEmpty:Boolean = {
    if (concepts.size == 0) return true
    false
  }

  def relabel(newLabel:String){
    clauseType = newLabel
  }

  /*
   * String Methods
   */
  override def toString:String = {
    val os = new StringBuilder

    os.append("Clause  (type: " + clauseType + "\t negated: " + negationFlag + "\t transitiveVerb: " + transitiveVerbFlag + ") \n")

    for (i <- 0 until concepts.size) {
      val concept = concepts(i)._1
      val role = concepts(i)._2
      os.append("\tConcept " + i + " (role: " + role + ") : \t" + concept.toString + "\n")
    }

    os.toString()
  }

}


object Clause {

  /*
   * Role types
   */
  val ROLE_AGENT        = "AGENT"
  val ROLE_PATIENT      = "PATIENT"
  val ROLE_VERB         = "VERB"
  val ROLE_OTHER        = "OTHER"
  // Role types for definitions and other clause types?


  /*
   * Clause types
   */
  val CTYPE_DEF         = "DEFINITION"
  val CTYPE_PROCESS     = "PROCESS"
  val CTYPE_OTHER       = "OTHER"


  // Convert dependencies to role types (roughly)
  def depToRole(dep:String, tag:String):String = {
    val agents = Array("nsubj", "nsubjpass", "agent")
    val patients = Array("dobj")

    // Agent
    if (agents.contains(dep)) {
      return ROLE_AGENT
    }

    // Patient
    if (patients.contains(dep)){
      return ROLE_PATIENT
    }

    // Verb
    if (tag.startsWith("VB")) {
      return ROLE_VERB
    }

    // Other
    ROLE_OTHER
  }

}