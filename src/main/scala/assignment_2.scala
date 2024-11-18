import scala.io.Source

val source = Source.fromFile("hospital.csv") //get the hospital data
val lines = source.getLines.toSet //put it into preferred collection (rn its Set -> must change according to use and justify)

// Question 1: Which state has the highest total hospital bed?


// Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset?


// Question 3: What are the averages of  individuals in category x  where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?