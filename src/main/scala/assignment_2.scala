import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.collection.mutable

val source = Source.fromFile("src/main/resources/hospital.csv") //get the hospital data

// Question 1: Which state has the highest total hospital bed?
object HospitalBeds {
  val stateBedCounts: mutable.Map[String,Int] = mutable.Map().withDefaultValue(0)

  val lines = source.getLines()
  lines.next()
  lines.foreach { line =>
    val columns =line.split(",").map(_.trim)
    val state = columns(1)
    val beds = columns(2).toInt
    stateBedCounts(state) += beds
  }
}

// Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset?
object question2 extends App {
  // 1. Define the data structure of the collection
  case class Row(beds: Int, beds_covid: Int) // Only reading necessary data
  // 2. Initialize collection -> Arraybuffer, and store the Row(s)
  val rows = ArrayBuffer[Row]()
  // 3. Read the csv files
  val lines = source.getLines()
  lines.next() // skipping the header in the csv file. (date,state,beds...)
  // 4. Parsing the csv file for the 3rd (index 2) and 4th (index 3) column only
  for (line <- lines){
    val columns = line.split(",")
    val beds = columns(2).toInt
    val beds_covid = columns(3).toInt
    rows += Row(beds, beds_covid)
  }
  // 5. Calculate the total of column 3 and 4
  val totalBeds = rows.map(_.beds).sum
  val totalCovidBeds = rows.map(_.beds_covid).sum
  // 6. Calculate the ratio of the total and print result
  println("Question 2")
  println(f"The total bed dedicated for COVID-19 is $totalCovidBeds and total of available hospital bed is $totalBeds.")

  val ratio = totalCovidBeds.toDouble / totalBeds
  println(f"Therefore, the ratio of beds for COVID-19 to available beds is $ratio")
}

// Question 3: What are the averages of  individuals in category x  where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?