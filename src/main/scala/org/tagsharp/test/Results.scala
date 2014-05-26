package org.tagsharp.test

import org.tagsharp.checkpoint.Checkpoint


class Results(val passes: List[(Checkpoint,Result)], val failures: List[(Checkpoint,Result)]) {

  def all = failures ::: passes
  val total:Int = passes.size + failures.size

}
