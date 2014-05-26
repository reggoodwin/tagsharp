package org.tagsharp.test

import org.jsoup.nodes.Element


class TestRunner(val element: Element, val checkpointSuite: CheckpointSuite) {

  def resultsOfTests():Results = {
    val results = {
      for (checkpoint <- checkpointSuite.checkpoints)
      yield (checkpoint, checkpoint.test.test(element))
    }
    val parted = results.toList partition (r => r match {
      case (c,Pass()) => true
      case (c,Fail(_)) => false
    })
    new Results(parted._1, parted._2)
  }

}
