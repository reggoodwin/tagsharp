package org.tagsharp.checkpoint


/**
 * A Checkpoint is associated with a Highlightable to indicate the kind
 * of error highlighting that can be performed on errors discovered by
 * the checkpoint rule.
 *
 * <ul>
 *     <li>Page - indicates that the error can be highlighted for an HTML page view representation.</li>
 *     <li>Source - indicates that the error can be highlighted in a source view representation.
 *                  When a page is rendered for 'source' view its tags are escaped so that
 *                  the HTML can be represented with HTML.</li>
 * </ul>
 */
case class Highlightable(page: Boolean, source: Boolean)

object Highlightable {
  val Both = Highlightable(true, true)
  val PageOnly = Highlightable(true, false)
  val SourceOnly = Highlightable(false, true)
  val None = Highlightable(false, false)
}