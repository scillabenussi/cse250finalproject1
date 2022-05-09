

import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
case class UserGenreRating(user: String, genre: String, ratin: Double)

class GenreBox extends Cardbox[UserGenreRating]((x,y) => x.user.compareTo(y.user))

object cse250Final {
  val movies = MovieEntry
  val userTuple = UserReaders.readEntries
  val userdatabase = userTuple._1

  var genreRatings = new GenreBox
  for(user <- userdatabase.toList){
   // genreRatings.insert()
  }
}
