import scala.collection.mutable.ListBuffer

/**
  * Created by bkasinadhuni on 3/9/19.
  */
object WordCount {
  def main(args: Array[String]): Unit = {
    // I had to do 2 filters here, since the file format was little doubtful and i want to eliminate dups
    val fullNames = scala.io.Source.fromFile("coding-test-data.txt").getLines.toList.map(_.split(" --").filter(_.contains(",")).headOption).filter(_.isDefined).map(_.get)

    var lastNamesBuffer = new ListBuffer[String]()
    var firstNamesBuffer = new ListBuffer[String]()
    // Calculating FirstName and LastName arrays, processing fullnames into a Tuple
    // Also i am calculating the unique FirstName and LastName combo for AC - #4, To avoid re-processing
    val tupledNames = fullNames.map(i => {
      val nameArray = i.split(",")
      val lastName = nameArray.head
      val firstName = nameArray.last

      // This is for AC - #4, which will be used as "tupledNames._3" later below
      val uniqueTuple = if (lastNamesBuffer.size > 0 && firstNamesBuffer.size > 0) {
        if (lastNamesBuffer.contains(lastName)) {
          None
        } else if (firstNamesBuffer.contains(firstName)) {
          None
        } else {
          // add to list
          lastNamesBuffer += lastName
          firstNamesBuffer += firstName

          Option((lastName, firstName))
        }
      } else {
        // add to list
        lastNamesBuffer += lastName
        firstNamesBuffer += firstName

        Option((lastName, firstName))
      }

      (lastName, firstName, uniqueTuple)
    })
    val lastNames: Seq[String] = tupledNames.map(_._1)
    val firstNames: Seq[String] = tupledNames.map(_._2)

    val fullNameCount = fullNames.groupBy(i => i).mapValues(_.size)

    // Sort by word Count desc
    val lastNameCount = lastNames.groupBy(i => i).mapValues(_.size).toSeq.sortBy(_._2)
    val firstNameCount = firstNames.groupBy(i => i).mapValues(_.size).toSeq.sortBy(_._2)


    // Sample of how Word count looks here
    println(s"Sample word count for Full Names = ${fullNameCount.head._1}  -> ${fullNameCount.head._2}")

    // AC - #1: The unique count of full, last, and first names (i.e., duplicates are
    //     counted only once)
    println(s"Unique fullNameCount = ${fullNameCount.size}")
    println(s"Unique lastNameCount = ${lastNameCount.size}")
    println(s"Unique firstNameCount = ${firstNameCount.size}")

    // AC - #2:  The ten most common last names (the names and number of occurrences
    //     sorted in descending order)
    val topTenLastNames = lastNameCount.take(10)
    println("Top 10 Last Names")
    println(topTenLastNames)

    // AC - #3: The ten most common first names (the names and number of occurrences
    //     sorted in descending order)
    val topTenFirstNames = firstNameCount.take(10)
    println("Top 10 Last Names")
    println(topTenFirstNames)

    // AC- #4 Starts here, FYI i already calculated the unique list above once for all in "tupledNames"

    val uniqueFullNames: Seq[(String, String)] = tupledNames.map(_._3).filter(_.isDefined).map(_.get)

    // As per AC - #4 : I took 25 here
    val top25UniqueFullNames = uniqueFullNames.take(25)
    println("Top 25 Unique Names")
    println(top25UniqueFullNames)

    // Now Starts the Fun part, Building the modified Names.
    // I am really not sure if i can swap firstName and lastNames to make it modified FullName :P
    val uniqueFirstNames = top25UniqueFullNames.toMap.values.toList

    //Idea here is to Right Shift FirstNames by 2 times and build a new tuple
    val uniqueFirstNamesShifted = uniqueFirstNames.drop(2) ++ uniqueFirstNames.take(2)
    val modifiedNames = top25UniqueFullNames.zipWithIndex.map { case (tuple, index) => (tuple._1, uniqueFirstNamesShifted(index)) }

    println("***** Top 25 Modified Unique Names *****")
    println(modifiedNames)
  }
}
