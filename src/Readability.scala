import java.io._

object Readability {

	/** 
      * Solves a sudoku problem.
      * @param args Not used.
      */
    def main(args: Array[String]) {
        val file = IOHelper.openFile()

        file match {
            case f: File =>
                val lines = IOHelper.fetchText(f) map (_.trim)
                line foreach println
            case _ => ()
        }
    }
	
}

/** An object that handles IO.
  * @author Christopher Arriola
  * @version November 19, 2010
  */
object IOHelper {

    /**
      * Opens a file by using JFileChooser.
      * @return The opened file 
      */
    def openFile(): File = {
        import javax.swing._

        val chooser = new JFileChooser("Select sudoku problem to solve")
        chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
        chooser.showOpenDialog(null)
        chooser.getSelectedFile 
    }
 
    /**
      * Will read text from just about anywhere, and will return it as a
      * list of Strings. (from Dr. Dave)
      * @param from The text to read from.
      * @return A list of strings. 
      */
    def fetchText(from: Any): List[String] = {
        import scala.io._
        val inStream: java.io.InputStream = from match {
            case stream: InputStream => stream
            case file: File => new FileInputStream(file)
            case fileName: String =>
                new FileInputStream(new File(fileName))
            case _ => null
        }

        val lines = Source.fromInputStream(inStream).getLines.toList
        inStream close()
        lines
    }
}
