/** File cse250Final.scala by Scilla Benussi and Connor Lehr
 * This file classes and methods created in the file MovieBaseCL and UserBaseSB
 * to evaluate the averages and factors listed in the handout for the Final project
 */

import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
case class UserGenreRating(user: String, genre: String, rating: Double)

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
