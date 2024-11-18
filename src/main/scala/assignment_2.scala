import scala.io.Source

val source = Source.fromFile("hospital.csv") //get the hospital data
val lines = source.getLines.toSet //put it into preferred collection (rn its Set -> must change according to use and justify)

//Which state has the highest total hospital bed ?