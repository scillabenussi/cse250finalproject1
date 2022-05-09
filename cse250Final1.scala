/** File cse250Final1.scala
 * This file will facilitate the collaboration
 */
import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object cse250Final1 extends App{
  val movies: ArrayBuffer[MovieEntry] = MovieReader.readMovies
  val userTuple = UserReaders.readEntries
  val userdatabase = userTuple._1

  var genreRatings = new GenreBox
  var users = userdatabase.begin

  var currentUser = ""
  var actionRating: List[Double] = List()
  var noirRating: List[Double] = List()
  var lightRating: List[Double] = List()
  var seriousRating: List[Double] = List()
  var fantasyRating: List[Double] = List()
  var historyRating: List[Double] = List()

  while(users.hasNext){
    val user = users.next()
    val user_id = user.user_id
    val movie_ratings = user.rated_movies
    if(user_id != currentUser){
      actionRating = List()
      noirRating = List()
      lightRating = List()
      seriousRating = List()
      fantasyRating = List()
      historyRating = List()
      val currentMovieGenre = movies(movie_ratings.movie_id.toInt-1)
    }
  }

  def pref_fact(genreBox: GenreBox): GenreBox = {
    var prefBox: GenreBox = new GenreBox
    //description
    prefBox
  }
}
