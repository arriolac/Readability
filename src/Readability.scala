import java.io._

/**
  * This program provides a measure for readability -- how good a reader
  * needs to be in order to understand a passage of English text. The measures
  * used are based on the average length of words, and the average length of 
  * sentences.
  *
  */
object Readability {
   
    /** Counts the number of words in a list of Strings */ 
    val getWordCount = (l: List[String]) => 
            (asSingleStr(l)) split " " length

    /** Counts the number of long words (6 letters or more) */
    val getLongWordCount = (l: List[String]) =>
            ((asSingleStr(l)) split " ") map (w => w.toList filter (Character.isLetter(_))) filter 
                                             (s => s.length >= 6) length

    /** Counts the number of complex words in a list of Strings */
    val getComplexWordCount = (l: List[String]) =>
            ((asSingleStr(l)) split " " toList) count (word => getSyllableCount(word) >= 3)

    /** Counts the number of letters in a a list of String */
    val getLetterCount = (l: List[String]) => 
            (asSingleStr(l)) filter (Character isLetter _) length
   
    /** Returns the total number of syllables */ 
    val getTotalSyllableCount = (l: List[String]) =>
            (((asSingleStr(l)) split " " toList) map (word => getSyllableCount(word))) reduceLeft (_ + _) 

    /** Checks to see if ch is a vowel */
    val isVowel = (ch: Char) => 
            ("aeiouy" toList) exists (_ == ch)

	/** 
      * Starts the program
      * @param args Not used.
      */
    def main(args: Array[String]) {
        val file = IOHelper.openFile()
        
        if (file != null) {
            val lines = IOHelper.fetchText(file) map (_ trim)
            printReadabilityMeasures(lines)
        } else
            ()
    }

    /**
      * Prints on the terminal the readability measures.
      */
    def printReadabilityMeasures(l: List[String]) {
        val complexWordCount = getComplexWordCount(l)
        val wordCount = getWordCount(l)
        val sentenceCount = getSentenceCount(l)
        val letterCount = getLetterCount(l)
        val syllableCount =  getTotalSyllableCount(l) 
        val longWordCount = getLongWordCount(l)


        val kincaid = getKincaidMeasure(wordCount, sentenceCount, syllableCount)
        val ari = getARI(letterCount, wordCount, sentenceCount)
        val cli = getCLI(letterCount, wordCount, sentenceCount)
        val flesch = getFlesch(wordCount, sentenceCount, syllableCount)
        val fog = getFog(wordCount, complexWordCount, sentenceCount)
        val lix = getLix(wordCount, longWordCount, sentenceCount)
        val smog = getSmog(sentenceCount, complexWordCount)

        println("Kincaid Measure: " + kincaid)
        println("Automated Readability Index: " + ari)
        println("Coleman-Liau: " + cli)
        println("Flesch: " + flesch)
        println("Fog(Gunning): " + fog)
        println("Lix: " + lix)
        println("Smog: " + smog)
    }

    /**
      * Returns the SMOG measure
      * @param sentences The total number of sentences
      * @param complexWords The total number of complexWords
      * @return The SMOG measure
      */
    def getSmog(sentences: Int, complexWords: Int) = {
        val nComplexWords: Double = complexWords
        val nSentences: Double = sentences
        3.1291 + (1.043 * Math.sqrt(30 * (nComplexWords/nSentences))) 
    }

    /**
      * Returns the Lix measure
      * @param words The total number of words
      * @param longWords The total number of long words
      * @param sentences The total number of sentences
      * @return The Lix measure
      */
    def getLix(words: Int, longWords: Int, sentences: Int) = {
        val nWords: Double = words
        val nLongWords: Double = longWords
        val nSentences: Double = sentences
        
        (nWords/nSentences) + (100 * (nLongWords/nWords))
    }

    /**
      * Returns the Flesch measure
      * @param words The total number of words
      * @param sentences The total number of sentences
      * @param syllables The total number of syllables
      * @return The Flesch measure
      */
    def getFog(words: Int, complexWords: Int, sentences: Int) = {
        val nWords: Double = words
        val nComplexWords: Double = complexWords
        val nSentences: Double = sentences
        0.4 * ((nWords/nSentences) + (100 * (nComplexWords/nWords)))
    }

    /**
      * Returns the Flesch measure
      * @param words The total number of words
      * @param sentences The total number of sentences
      * @param syllables The total number of syllables
      * @return The Flesch measure
      */
    def getFlesch(words: Int, sentences: Int, syllables: Int) = {
        val nWords: Double = words
        val nSentences: Double = sentences
        val nSyllables: Double = syllables
        208.835 - (1.015 * (nWords/nSentences)) - (84.6 * (nSyllables/nWords))
    }

    /**
      * Returns the automated readability index
      * @param letters The total number of letters
      * @param words The total number of words
      * @param syllables The total number of syllables
      * @return The CLI measure
      */
    def getCLI(letters: Int, words: Int, sentences: Int) = {
        val nLetters: Double = letters
        val nWords: Double = words
        val nSentences: Double = sentences
        (5.89 * (nLetters/nWords)) - (29.5 * (nSentences/nWords)) - 15.8
    }

    /**
      * Returns the automated readability index
      * @param letters The total number of letters
      * @param words The total number of words
      * @param sentences The total number of sentences
      * @return The ARI measure
      */
    def getARI(letters: Int, words: Int, sentences: Int) = {
        val nLetters: Double = letters
        val nWords: Double = words
        val nSentences: Double = sentences
        (4.71 * (nLetters/nWords)) + (0.5 * (nWords/nSentences)) - 21.43
    }

    /**
      * Returns the Kincaid measure
      * @param words The total number of words
      * @param sentences The total number of sentences
      * @param syllables The total number of syllables
      * @return The Kincaid measure
      */
    def getKincaidMeasure(words: Int, sentences: Int, syllables: Int) = {
        val nWords: Double = words
        val nSentences: Double = sentences
        val nSyllables: Double = syllables
        (0.39 * (nWords/nSentences)) + (11.8 * (nSyllables/nWords)) - 15.59
    }

    /**
      * Returns a list of String as a single String.
      * @param l The list of Strings
      * @return The single String
      */
    def asSingleStr(l: List[String]) = {
        val sb = new StringBuilder
        val listLength = l.length
        for (i <- 0 until listLength) {
            sb.append(l(i))
            sb.append(" ")
        }
        sb.toString.trim
    }

    /**
      * Gets the number of sentences in l.
      * @param l The list of Strings
      * @return The number of sentences
      */
    def getSentenceCount(l: List[String]): Int = {
        val lString = asSingleStr(l) 
        val strLength = lString.length
        var count = 0
                
        for (i <- 0 until strLength - 1) {
            if ((lString.charAt(i) == '.' && lString.charAt(i+1) == ' ') ||
                (lString.charAt(i) == '!' && lString.charAt(i+1) == ' ') ||
                (lString.charAt(i) == '?' && lString.charAt(i+1) == ' '))
                count = count + 1
            else if (i == strLength-2 && (lString.charAt(i+1) == '.' ||
                                          lString.charAt(i+1) == '!' ||
                                          lString.charAt(i+1) == '?'))
                count = count + 1
        } 
        count
    }

    /**
      * Gets the number of syllables in a string -- each vowel counts as one
      * syllable subject to the following rules:
      * -> ignore -es -ed, -e (except for -le)
      * -> words of three or fewer letters count as one syllable
      * -> consecutive vowels counts as one syllable (e.g. "delicious" has 3)
      * -> 'Y' at the beginning of a word doesn't count as a vowel.
      * @param word The word to count the syllables from.
      * @return The number of syllables in that word
      */
    def getSyllableCount(word: String): Int = {

        val processedWord = word filter (Character.isLetter(_)) mkString

        if (processedWord.length <= 3) 
            return 1

        val wordMod = (processedWord toLowerCase).toList
        val lastTwoLetters = (wordMod takeRight 2).mkString
        var totalNumberOfVowels = wordMod count isVowel 
       
        // Subtract 1 from count if word ends in -es, -ed, or -e but not -le 
        if (lastTwoLetters.equals("es") ||
            lastTwoLetters.equals("ed") ||
            (!lastTwoLetters.equals("le") &&
             !lastTwoLetters.equals("ee") &&
             (wordMod.last == 'e')))
            totalNumberOfVowels = totalNumberOfVowels - 1

        // Subtract 1 if word begins with y
        if (wordMod.head == 'y')
            totalNumberOfVowels = totalNumberOfVowels - 1  
    
        var consSyllCount = 0
        for (i <- 0 until wordMod.length-1) {
            if (isVowel(wordMod(i)) && isVowel(wordMod(i+1))) {
                if (i == 0)
                    totalNumberOfVowels = totalNumberOfVowels - 1
                else if (!isVowel(wordMod(i-1)))
                    totalNumberOfVowels = totalNumberOfVowels - 1
            }
        }
        totalNumberOfVowels
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
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY)
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
