import scala.io.Source //to read csv files
import scala.collection.mutable //to use Map to accumulate beds counts by state
import scala.math.BigDecimal

// Shared functionality to read and get lines from hospital.csv
trait CSVAnalysis{
  def source: Source = Source.fromFile("src/main/resources/hospital.csv") //get the hospital data
  
  //Parse all lines and store them in memory as a list
  lazy val data: List[Array[String]] = {
    val lines = source.getLines()
    lines.next() // skipping the header in the csv file. (date,state,beds...)
    lines.map(_.split(",").map(_.trim)).toList //Parse and store as a List of Columns
  }
}

// Question 1: Which state has the highest total hospital bed?
class Question1 (data: List[Array[String]]) {
  //define the column indices as constants for better maintainability
  val StateColumnIndex = 1
  val BedColumnIndex = 2

  def analyse(): Unit = {
    val stateBedCounts: mutable.Map[String, Int] = mutable.Map().withDefaultValue(0) /* stores the total
        hospital bed count for each state */

    for (columns <- data){
      val state = columns(StateColumnIndex) //state column
      val beds = columns(BedColumnIndex).toInt //beds column
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
class Question2 (data: List[Array[String]]) {
  // Define the data structure of the collection
  def analyse(): Unit = {
    // Calculate the total of column 3 and 4
    val totalBeds = data.map(_(2).toInt).sum
    val totalCovidBeds = data.map(_(3).toInt).sum

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
class Question3 (data: List[Array[String]]){
    //Indices for the CSV columns
    val stateIndex = 1
    val admittedPUIIndex = 5
    val admittedCovidIndex = 6
    val admittedNonCovidIndex = 7

  def analyse(): Unit = {
    //Map to accumulate totals and counts for each state and category
    val stateData = mutable.Map[String, (Int, Int, Int, Int, Int, Int)]()
      .withDefaultValue((0, 0, 0, 0, 0, 0))
    // Format: (totalPUI, countPUI, totalCovid, countCovid, totalNonCovid, countNonCovid)

    for (columns <-data) {
      try {
        val state = columns(stateIndex) // State column index
        val admittedPUI = columns(admittedPUIIndex).toInt // Suspected/probable column index
        val admittedCovid = columns(admittedCovidIndex).toInt //  COVID-19 column index
        val admittedNonCovid = columns(admittedNonCovidIndex).toInt // Non-COVID column index

        val (totalPUI, countPUI, totalCovid, countCovid, totalNonCovid, countNonCovid) = stateData(state)
        stateData(state) = (
          totalPUI + admittedPUI, countPUI + 1,
          totalCovid + admittedCovid, countCovid + 1,
          totalNonCovid + admittedNonCovid, countNonCovid + 1
        )
      } catch {
        case _: Exception =>//To skip the invalid rows
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
object MainApp extends App with CSVAnalysis {
  val question1 = new Question1(data)
  question1.analyse()

  val question2 = new Question2(data)
  question2.analyse()

  val question3 = new Question3(data)
  question3.analyse()
}