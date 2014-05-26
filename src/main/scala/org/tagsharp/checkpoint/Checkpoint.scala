package org.tagsharp.checkpoint

import org.tagsharp.test.Test

case class Checkpoint(
  val id: String,
  val name: String,
  val test: Test,
  val highlightable: Highlightable
)
