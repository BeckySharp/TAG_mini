package edu.arizona.sista.mc.tessellatedgraph.Clauseinator

import edu.arizona.sista.mc.focusword.QuestionDecomposerAPI
import edu.arizona.sista.mc.tessellatedgraph.structures.Schema
import org.clulab.utils.StringUtils


/**
  * Created by rebeccas on 4/5/17.
  */
class GraphletMaker () {
  val props = StringUtils.argsToProperties(Array("-props", "src/main/resources/concreteness.props"))

  // Initialize question decomposer API
  QuestionDecomposerAPI.initialize(props)

  def textToGraphlets(sentence: String, source: String = "UNK", verbose: Boolean = false): Seq[Schema] = {
    val cLInputSentences = Clauseinator.stringToCLInputSentence(sentence, source)._1
    Clauseinator.extract(cLInputSentences.toArray, verbose)
  }

}
