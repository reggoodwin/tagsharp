package org.tagsharp.test

import org.tagsharp.checkpoint.Checkpoint


class CheckpointSuite(val checkpoints: Seq[Checkpoint]) {
  def size():Int = checkpoints.size
}
