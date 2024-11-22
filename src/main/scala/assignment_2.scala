import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val source = Source.fromFile("hospital.csv") //get the hospital data

// Question 1: Which state has the highest total hospital bed?


// Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset?
object question2 extends App {
  // 1. Define the data structure of the collection
  case class Row(beds: Int, beds_covid: Int) // Only reading necessary data

  // 2. Initialize collection -> Arraybuffer, and store the Row(s)
  val rows = ArrayBuffer[Row]()

  // 3. Read the csv files
  val lines = source.getLines()
}

// Question 3: What are the averages of  individuals in category x  where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?