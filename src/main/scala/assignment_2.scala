import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val source = Source.fromFile("src/main/resources/hospital.csv") //get the hospital data

// Question 1: Which state has the highest total hospital bed?


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
}

// Question 3: What are the averages of  individuals in category x  where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?