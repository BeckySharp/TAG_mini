package edu.arizona.sista.mc.tessellatedgraph.structures

/**
 * Created by peter on 1/14/15.
 */
class ClauseLink(var source:Clause, val destination:Clause, var label:String, val bidirectionalFlag:Boolean) extends Serializable {
  // Weight?

  def relabel (newLabel:String) = {
    label = newLabel
  }

}

object ClauseLink {

  /*
   * Clause link labels
   */
  val CL_INSTRUMENT     = "INSTRUMENT"
  val CL_PROCESS        = "PROCESS"
  val CL_EXAMPLE        = "EXAMPLE"
  val CL_DEFINITION     = "DEFINITION"
  val CL_STATE          = "STATE"
  val CL_TEMPORAL       = "TEMPORAL"
  val CL_CONTRAST       = "CONTRAST"
  val CL_UNKNOWN        = "UNKNOWN"


  // Converts/groups dependency labels into a clause link labels
  def dependencyToLinkType(depLabel:String):String = {
    if ( Array("prep_with", "prep_through", "prep_by").contains(depLabel) ) {
      // Instrument
      return CL_INSTRUMENT
    } else if ( Array("prep_from", "prep_to", "prep_into", "prep_because_of", "prepc_because_of").contains(depLabel) ) {
      // Process
      return CL_PROCESS
    } else if ( Array("prep_as", "prepc_as", "prep_such_as", "prepc_such_as").contains(depLabel) ) {
      // Example
      return CL_EXAMPLE
    } else if ( Array("prep_before", "prepc_before", "prep_after", "prepc_after", "prep_during", "prepc_during").contains(depLabel) ) {
      // Example
      return CL_TEMPORAL
    } else if ( Array("prep_without", "prepc_without").contains(depLabel) ) {
      // Example
      return CL_CONTRAST
    }

    // Default
    CL_UNKNOWN
  }


}

// TODO: 1) toString, 2) what kinds of labels do we need?