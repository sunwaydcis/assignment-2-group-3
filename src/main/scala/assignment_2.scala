import scala.collection.mutable.ArrayBuffer
import scala.io.Source //to read csv files
import scala.collection.mutable //to use Map to accumulate beds counts by state
import scala.math.BigDecimal

// Shared functionality to read and get lines from hospital.csv
trait CSVAnalysis{
  def source: Source = Source.fromFile("src/main/resources/hospital.csv") //get the hospital data
  def fetchLines: Iterator[String] = { // read lines as lazy iterator one line at a time -> no need to load into memory
    val lines = source.getLines()
    lines.next() // skipping the header in the csv file. (date,state,beds...)
    lines
  }
}

// Question 1: Which state has the highest total hospital bed?
class Question1 extends CSVAnalysis {
  def analyse(): Unit = {
    val stateBedCounts: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0) /* stores the total
        hospital bed count for each state */

    for (line <- fetchLines){
      val columns = line.split(",").map(_.trim)
      val state = columns(1) //state column
      val beds = columns(2).toInt //beds column
      stateBedCounts(state) += beds
    }
    //to find the state with the highest total hospital beds
    val (highestState, highestBeds) = stateBedCounts.maxBy(_._2)
    //Output the result
    println(s"# Question 1")
    println(s"State with the highest total hospital beds: $highestState ($highestBeds beds)")
  }
}
  // Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset?
class Question2 extends CSVAnalysis {
  // Define the data structure of the collection
  case class Row(beds: Int, beds_covid: Int) // Only reading necessary data

  def analyse(): Unit = {
    // Initialize collection -> Arraybuffer, and store the Row(s)
    val rows = ArrayBuffer[Row]()

    // Parsing the csv file for the 3rd (index 2) and 4th (index 3) column only
    for (line <- fetchLines) {
      val columns = line.split(",")
      val beds = columns(2).toInt
      val beds_covid = columns(3).toInt
      rows += Row(beds, beds_covid)
    }
    // Calculate the total of column 3 and 4
    val totalBeds = rows.map(_.beds).sum
    val totalCovidBeds = rows.map(_.beds_covid).sum

    // Calculate the ratio of the total and print result
    println("# Question 2")
    println(f"The total bed dedicated for COVID-19 is $totalCovidBeds and total of available hospital bed is $totalBeds.")
    if (totalBeds != 0){
      val ratio = totalCovidBeds.toDouble / totalBeds
      val roundedRatio = BigDecimal(ratio).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble // Rounded to 3 decimal places
      println(f"Therefore, the ratio of beds for COVID-19 to available beds is $roundedRatio") 
    } else {
      println("There is no amount of beds recorded in the database.")
    }
  }
}

// Question 3: What are the averages of  individuals in category x  where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?
class Question3 extends CSVAnalysis{
    //Case class to represent each record in the data set
    case class HospitalData(state: String, admittedPUI: Int, admittedCovid: Int, admittedNonCovid: Int)

    //Indices for the CSV columns
    val stateIndex = 1
    val admittedPUIIndex = 5
    val admittedCovidIndex = 6
    val admittedNonCovidIndex = 7

    //Function to parse a CSV line into a HospitalData class ensuring only valid rows are processed
    def parseLine(line: String): Option[HospitalData] = {
      val columns = line.split(",").map(_.trim)
      try {
        Some(
          HospitalData(
            state = columns(stateIndex), // State column index
            admittedPUI = columns(admittedPUIIndex).toInt, // Suspected/probable column index
            admittedCovid = columns(admittedCovidIndex).toInt, //  COVID-19 column index
            admittedNonCovid = columns(admittedNonCovidIndex).toInt // Non-COVID column index
          )
        )
      } catch {
        case _: Exception => None // Skip lines that can't be parsed
      }
    }

    def analyse(): Unit = {
      //Map to accumulate totals and counts for each state and category
      val stateData = mutable.Map[String, (Int, Int, Int, Int, Int, Int)]()
        .withDefaultValue((0,0,0,0,0,0))
      // Format: (totalPUI, countPUI, totalCovid, countCovid, totalNonCovid, countNonCovid)

      //Process each line and update and stateData map
      for (line <- fetchLines) {
        parseLine(line)match {
          case Some(data) =>
            val (totalPUI, countPUI, totalCovid, countCovid, totalNonCovid, countNonCovid) = stateData(data.state)
            stateData(data.state) = (
              totalPUI + data.admittedPUI, countPUI + 1,
              totalCovid + data.admittedCovid, countCovid + 1,
              totalNonCovid + data.admittedNonCovid, countNonCovid + 1
            )
          case None => //To skip the invalid rows
        }
      }

      //calculate averages and print results
      println("# Question 3")
      println("State-wide averages  of admitted individuals for each category: ")
      for ((state, (totalPUI, countPUI, totalCovid, countCovid, totalNonCovid, countNonCovid)) <- stateData) {
        val avgPUI = if (countPUI > 0) totalPUI.toDouble / countPUI else 0.0
        val avgCovid = if (countCovid > 0) totalCovid.toDouble / countCovid else 0.0
        val avgNonCovid = if (countNonCovid > 0) totalNonCovid.toDouble / countNonCovid else 0.0

        println(
          f"$state: Suspected/Probable = ${BigDecimal(avgPUI).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}, " +
          f"COVID-19 Positive = ${BigDecimal(avgCovid).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}, " +
          f"Non-COVID = ${BigDecimal(avgCovid).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}"
        )
      }
    }
}

// Run questions to get answers
object MainApp extends App {
  val question1 = new Question1
  question1.analyse()

  val question2 = new Question2
  question2.analyse()

  val question3 = new Question3
  question3.analyse()
}