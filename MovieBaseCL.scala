/**
 *read in the movies and store them in an array buffer Connor Lehr CSE 250
 */

import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 *
 * @param index is the index at which the movie was found
 * @param title is title given
 * @param year is year based off the date given (not one given in the title)
 * @param genres a set based off of the condensed list of genres given in u.genre
 */
case class MovieEntry (index: String, title: String,year:String, genres: Set[String])

object MovieReader extends App {
  /**
   * First part of the object reads in the genres and forms them into a map, mapping the genre listed to our condensed
   * genres.
   */
  val genreFile = "u.genre"

  val genreText = Source.fromFile(genreFile, "windows-1250")
  var genreMap: Map[String, String] = Map()
  for (line <- genreText.getLines()) {
    val lineList = line.split('|').toList
    if (lineList.length > 1) { //ignore lines with no data
      if (lineList(1) == "1" || lineList(1) == "2" || lineList(1) == "16" || lineList(1) == "18") {
        genreMap = genreMap + (lineList(1) -> "Action")
      }
      else if (lineList(1) == "6" || lineList(1) == "10" || lineList(1) == "11" || lineList(1) == "13") {
        genreMap = genreMap + (lineList(1) -> "Noir")
      }
      else if (lineList(1) == "3" || lineList(1) == "4" || lineList(1) == "5" || lineList(1) == "12") {
        genreMap = genreMap + (lineList(1) -> "Light")
      }
      else if (lineList(1) == "8" || lineList(1) == "14") {
        genreMap = genreMap + (lineList(1) -> "Serious")
      }
      else if (lineList(1) == "15" || lineList(1) == "9") {
        genreMap = genreMap + (lineList(1) -> "Fantasy")
      }
      else if (lineList(1) == "17" || lineList(1) == "7") {
        genreMap = genreMap + (lineList(1) -> "History")
      }
      else {
        genreMap = genreMap + (lineList(1) -> "unknown")
      }
    }
  }
  /**
   * second part reads in the movies from u.item and stores them in an ArrayBuffer of Movie entry, able to change to
   * another data Structure if need be.
   */
  def readMovies: ArrayBuffer[MovieEntry] = {
    val itemFile = "u.item"
    val itemText = Source.fromFile(itemFile,"windows-1250")
    var movieArray = new ArrayBuffer[MovieEntry]()

    for (line <- itemText.getLines()) {
      var index = ""
      var title = ""
      var year = ""
      var genres: Set[String] = Set()
      var lineArray = line.split('|')
      if (lineArray.length > 1) { //ignore lines with no data
        if (lineArray(1) != "unknown") {
          index = lineArray(0)
          title = lineArray(1)
          val date = lineArray(2).split("-")
          year = date(2)
          var i = 0
          while (i < 19) {
            if (lineArray(5 + i) == "1") {
              genres += genreMap(i.toString)
            }
            i += 1
          }
          println(s"index: $index title: $title year: $year genres: $genres")
          movieArray :+= MovieEntry(index, title, year, genres)
        }
      }
      else{ //if it is unknown
        println(s"index: $index title: $title year: $year genres: $genres")
        movieArray :+= MovieEntry(index, lineArray(1), year, Set(genreMap("0")))
      }
    }
    movieArray
  }
  readMovies
}

/**
 * Other parts needed are with partner
 * We will need two more data structures one that holds the data collected from the users where we can collect each users
 * individual movie ratings. And another that looks up the ratings a user rated for each genre using the data structure
 * that I created.
 */
