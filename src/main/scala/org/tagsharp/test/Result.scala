package org.tagsharp.test

/**
 * Represents the outcome of a Checkpoint Test.
 */
sealed abstract class Result

case class Fail(errors: List[Error]) extends Result
case class Pass() extends Result