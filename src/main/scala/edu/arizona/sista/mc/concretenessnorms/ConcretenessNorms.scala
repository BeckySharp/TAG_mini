package edu.arizona.sista.mc.concretenessnorms

import org.clulab.struct.Counter
import org.slf4j.LoggerFactory

/**
 * Created with IntelliJ IDEA.
 * User: peter
 * Date: 9/17/14
 */
class ConcretenessNorms()  {

}

object ConcretenessNorms {
  val logger = LoggerFactory.getLogger(classOf[ConcretenessNorms])

  def loadConcretenessNormsDatabase(filename:String):Counter[String] = {
    logger.info (" * Loading concreteness norms database (" + filename + ")...")
    val out = new Counter[String]
    for(line <- io.Source.fromFile(filename, "ISO-8859-1").getLines()) {
      //println (line)
      val split = line.toLowerCase.split("\t")
      val word = split(0).trim.toLowerCase
      val bigram = split(1).trim.toDouble             // 0 if single word, 1 if a bigram
      val concreteness = split(2).trim.toDouble       // concreteness score (0-5, 0/abstract, 5/concrete)
      val sd = split(3).trim.toDouble                 // standard deviation of concreteness score

      out.setCount(word, concreteness)
    }
    logger.info (" * Concreteness norms loaded.  (" + out.keySet.size + " normed lemmas)" )
    out
  }


}
