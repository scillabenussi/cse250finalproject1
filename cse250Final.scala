/** File cse250Final.scala by Scilla Benussi and Connor Lehr
 * This file classes and methods created in the file MovieBaseCL and UserBaseSB
 * to evaluate the averages and factors listed in the handout for the Final project
 */

import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
case class UserGenreRating(user: String, genre: String, ratingAvg: Double)
class GenreBox extends Cardbox[UserGenreRating]((x,y) => x.user.compareTo(y.user))
class MovieBox extends Cardbox[(MovieEntry,Double)]((x,y) => y._2.compareTo(x._2))

object cse250Final extends App {
  val userTuple = UserReaders.readEntries
  val movies: mutable.ArrayBuffer[MovieEntry] = MovieReader.readMovies
  val userdatabase = userTuple._1
  val ratingsDatabase = userTuple._2
  val r_m: mutable.Map[String, Double] = UserReaders.average(ratingsDatabase) //Map movie -> r_m
  val r_gMap: mutable.Map[String, Double] = R_g
  val userBoxGen: GenreBox = U_g
  val prefBox: GenreBox = pref_fact
  val moviesForUsers = mov_per_user_genre
  val outp = new PrintWriter(new FileWriter("output.txt",false));  //appends
  /*for(user <- moviesForUsers.keys) {
    var itr = moviesForUsers(user).begin
    while (itr.hasNext) {
      println(user + " -> " + itr.next()._2)
    }
  }*/
  val chosenUserId: String = "30"
  val chosenGenre: String = "Light"
  val chosenN: Int = 10
  val nMoviesForUser: ArrayBuffer[MovieEntry] = top_n(chosenUserId,chosenGenre,chosenN)
  /*for(movies <- nMoviesForUser){
    println("1 -> " + movies.title)
  }*/



  def U_g: GenreBox = {
    val genreBox = new GenreBox
    val users = userdatabase.begin
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
  def R_g: mutable.Map[String,Double] = {
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
    val returnMap: mutable.Map[String,Double] = mutable.Map(("Action"->actionRating.sum/actionRating.length),("Noir"->noirRating.sum/noirRating.length),("Light"->lightRating.sum/lightRating.length),
      ("Serious"->seriousRating.sum/seriousRating.length),("Fantasy"->fantasyRating.sum/fantasyRating.length),("History"->historyRating.sum/historyRating.length))
    returnMap

  }

  def pref_fact: GenreBox = {
    val prefFactBox: GenreBox = new GenreBox //Output GenreBox
    val userGenItr = userBoxGen.begin //Iterator to input GenreBox
    while(userGenItr.hasNext){
      val currentUser: UserGenreRating = userGenItr.next()
      val currGenre: String = currentUser.genre
      val newGenreRating: UserGenreRating = UserGenreRating(currentUser.user, currGenre, currentUser.ratingAvg/r_gMap(currGenre))
      prefFactBox.insert(newGenreRating)
    }
    prefFactBox
  }

  def mov_per_user_genre: mutable.Map[String,MovieBox] = {
    val userToMovies: mutable.Map[String, MovieBox] = mutable.Map()
    for (movieEntry <- movies) {
      if (r_m.contains(movieEntry.index)) {
        val rm: Double = r_m(movieEntry.index)
        for (currGenre <- movieEntry.genres) {
          val userWithGenre: List[UserGenreRating] = prefBox.toList.filter(_.genre == currGenre) //Review this for efficiency
          for (user <- userWithGenre) {
            val currUserID: String = user.user
            val rm_x_pug: Double = rm * user.ratingAvg
            if (userToMovies.contains(currUserID)) {
              userToMovies(currUserID).insert((movieEntry, rm_x_pug))
            } else {
              userToMovies += (currUserID -> new MovieBox())
            }
          }
        }
      }
    }
    userToMovies
  }

  def top_n(userID: String, genre: String, n : Int): ArrayBuffer[MovieEntry] = {
    var nMovies: ArrayBuffer[MovieEntry] = new ArrayBuffer[MovieEntry]()
    var count = 1
    if(userID.toInt >943){
      println("User Not Found")
      return nMovies
    }
    if(genre!="Action" && genre!= "Noir" && genre!= "Light" && genre!= "Serious" && genre!= "Fantasy" && genre!= "History"){
      println("Genre Not Found")
      return nMovies
    }
    val movieItr = moviesForUsers(userID).begin
    while (count <= n) {
      var currMovietoFactor: (MovieEntry, Double) = movieItr.next()
      while(!currMovietoFactor._1.genres.contains(genre) && movieItr.hasNext) {
        currMovietoFactor = movieItr.next()
      }
      if (movieItr.hasNext) {
        if(!nMovies.contains(currMovietoFactor._1)) {
          nMovies :+= currMovietoFactor._1
          count += 1
          outp.println("UserId: " + userID + " -> Movie Title: " + currMovietoFactor._1.title + ", genre(s): " + currMovietoFactor._1.genres + " -> rating: " + currMovietoFactor._2)
        }
      } else {
        println("Couldn't find enough movies for this genre (only " + count + "). Please consider reducing your n")
        return nMovies
      }
    }
    nMovies
  }
  outp.close()
}
