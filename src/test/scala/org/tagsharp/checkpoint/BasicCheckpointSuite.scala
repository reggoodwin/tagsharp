package org.tagsharp.checkpoint

import org.tagsharp.checkpoint.Highlightable.{Both, None}
import org.tagsharp.test.CheckpointSuite


object BasicCheckpointSuite {

  def defaultSuite():CheckpointSuite = new CheckpointSuite(Seq(
    
    Checkpoint(
      id = "a456646f-1453-4b2c-878e-9cb76236cb03",
      name = "A web page should not contain underlined text that is not a hyperlink",
      test = new ShouldTest("<u> should not occur"),
      highlightable = Both
    ),
    Checkpoint(
      id = "0b51aab0-04fd-4ca6-bd06-eeca70783090",
      name = "Image elements must contain an alt attribute",
      test = new ShouldTest("""<img> should contain attribute "alt" """),
      highlightable = None
    ),
    Checkpoint(
      id = "486a3c4c-b268-4eb4-be3d-933ecc0f7c66",
      name = "A web page should contain a main <h1> heading",
      test = new ShouldTest("<h1> should occur"),
      highlightable = None
    ),
    Checkpoint(
      id = "7706ec04-5ee1-4ea9-9c64-a92c5b86d120",
      name = "Heading text should not contain more than 6 words",
      test = new ShouldTest("elements <h1>, <h2>, <h3>, <h4>, <h5>, <h6> should contain at most 6 words"),
      highlightable = Both
    ),
    Checkpoint(
      id = "3426ec04-12a1-2ba9-8c44-a92c5b86d920",
      name = "The page title should be limited to 60 characters",
      test = new ShouldTest("<title> should contain <= 60 characters"),
      highlightable = Both
    )

  ))

}