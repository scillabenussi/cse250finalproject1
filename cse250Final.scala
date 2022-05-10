/** File cse250Final.scala by Scilla Benussi and Connor Lehr
 * This file classes and methods created in the file MovieBaseCL and UserBaseSB
 * to evaluate the averages and factors listed in the handout for the Final project
 */

import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
case class UserGenreRating(user: String, genre: String, ratingAvg: Double)

class GenreBox extends Cardbox[UserGenreRating]((x,y) => x.user.compareTo(y.user))

object cse250Final extends App {
  val userTuple = UserReaders.readEntries
  val movies: ArrayBuffer[MovieEntry] = MovieReader.readMovies
  val userdatabase = userTuple._1
  def U_g: GenreBox = {
    var genreBox = new GenreBox

    var users = userdatabase.begin


    var currentUser = ""
    var actionRating: List[Double] = List()
    var noirRating: List[Double] = List()
    var lightRating: List[Double] = List()
    var seriousRating: List[Double] = List()
    var fantasyRating: List[Double] = List()
    var historyRating: List[Double] = List()

    while (users.hasNext) {

      val user = users.next()
      val user_id = user.user_id
      val movie_ratings = user.rated_movies
      val currentMovie = movies(movie_ratings.movie_id.toInt - 1)

      if (user_id != currentUser) {
        if (currentUser != "") {
          if (!actionRating.isEmpty) {
            genreBox.insert(UserGenreRating(currentUser, "Action", actionRating.sum / actionRating.length))
          }
          if (!noirRating.isEmpty) {
            genreBox.insert(UserGenreRating(currentUser, "Noir", noirRating.sum / noirRating.length))
          }
          if (!lightRating.isEmpty) {
            genreBox.insert(UserGenreRating(currentUser, "Light", lightRating.sum / lightRating.length))
          }
          if (!seriousRating.isEmpty) {
            genreBox.insert(UserGenreRating(currentUser, "Serious", seriousRating.sum / seriousRating.length))
          }
          if (!fantasyRating.isEmpty) {
            genreBox.insert(UserGenreRating(user_id, "Fantasy", fantasyRating.sum / fantasyRating.length))
          }
          if (!historyRating.isEmpty) {
            genreBox.insert(UserGenreRating(user_id, "History", historyRating.sum / historyRating.length))
          }
        }
        currentUser = user_id
        actionRating = List()
        noirRating = List()
        lightRating = List()
        seriousRating = List()
        fantasyRating = List()
        historyRating = List()

      }
      for (genre <- currentMovie.genres) {
        if (genre == "Action") {
          actionRating :+= movie_ratings.rating
        } else if (genre == "Noir") {
          noirRating :+= movie_ratings.rating
        } else if (genre == "Light") {
          lightRating :+= movie_ratings.rating
        } else if (genre == "Serious") {
          seriousRating :+= movie_ratings.rating
        } else if (genre == "Fantasy") {
          fantasyRating :+= movie_ratings.rating
        } else if (genre == "History") {
          historyRating :+= movie_ratings.rating

        }
      }
    }

  genreBox
  }
  def R_g: Array[(String,Double)] ={
    val users = userdatabase.begin
    var actionRating: List[Double] = List()
    var noirRating: List[Double] = List()
    var lightRating: List[Double] = List()
    var seriousRating: List[Double] = List()
    var fantasyRating: List[Double] = List()
    var historyRating: List[Double] = List()
    while(users.hasNext){
      val user = users.next()
      val movie_ratings = user.rated_movies
      val currentMovie = movies(movie_ratings.movie_id.toInt - 1)
      if(currentMovie.genres.contains("Action")){
        actionRating :+= movie_ratings.rating
      }else if(currentMovie.genres.contains("Noir")){
        noirRating :+= movie_ratings.rating
      }else if(currentMovie.genres.contains("Light")){
        lightRating :+= movie_ratings.rating
      }else if(currentMovie.genres.contains("Serious")){
        seriousRating :+= movie_ratings.rating
      }else if(currentMovie.genres.contains("Fantasy")){
        fantasyRating :+= movie_ratings.rating
      }else if(currentMovie.genres.contains("History")){
        historyRating :+= movie_ratings.rating
      }
    }
    val returnArray: Array[(String,Double)] = Array(("Action",actionRating.sum/actionRating.length),("Nior",noirRating.sum/noirRating.length),("Light",lightRating.sum/lightRating.length),
      ("Serious",seriousRating.sum/seriousRating.length),("Fantasy",fantasyRating.sum/fantasyRating.length),("History",historyRating.sum/historyRating.length))
  returnArray

  }
}
