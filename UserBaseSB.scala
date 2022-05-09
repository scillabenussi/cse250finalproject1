/** File UserBaseSB.scala created by Scilla Benussi for CSE250, Spring 2022
 *  Final Project, due date: May 13th, Midnight stretchy
 */
import scala.collection.mutable
import scala.io.Source

/**
 * @param user_id is the id of the user in the u.data file
 * @param rated_movies is Movie_ratings object
 */

case class User(user_id: String, rated_movies: Movie_ratings)

/**
 * @param movie_id is the id of the movie in the u.data file
 * @param rating is the id of the user in the u.data file
 */

case class Movie_ratings(movie_id: String, rating: Double)
//We'll use ISR classes for the whole users+their ratings database
class UserBox extends Cardbox[User]((x,y) => x.user_id.compareTo(y.user_id)) {}

object UserReaders {
  //readEntries method reads the u.data file and creates the userbox, a Cardbox of all the user_ids, with all their related movie_ratings
  def readEntries: (UserBox, mutable.Map[String, Array[Double]]) = {
    val userFile = "u.data" //Use fixed version of the file
    val src = Source.fromFile(userFile) //Read from u.data
    var userbox = new UserBox()
    var movie_to_ratings: mutable.Map[String, Array[Double]] = mutable.Map()
    var user_id = "" //will save the user id
    var movie = "" //will save the movie id
    var rating: Double = 0.0 //Needs to be a double
    var count = 0 //counts the ratings to see if it's 100000 at the end (code works as it should)
    for (line <- src.getLines()) {
      //Each line has the format <user_id> <movie_id> <rating> <unix timestamp>
      val lineArray = line.split("\\s+").filter(_.nonEmpty)
      user_id = lineArray(0)  //<user_id>
      movie = lineArray(1).toString //<movie_id>
      rating = lineArray(2).toDouble //<rating>
      if(movie_to_ratings.contains(movie)){
        movie_to_ratings(movie) :+= rating
      } else{
        var new_arr: Array[Double] = Array()
        new_arr :+= rating
        movie_to_ratings = movie_to_ratings + (movie -> new_arr)
      }
      val movie_rat = new Movie_ratings(movie,rating) //creates the Movie_ratings object
      val user = new User(user_id, movie_rat) //creates the User object
      userbox.insert(user) //Inserts in the chosen Carbox
      count+=1
    }
    println("I've inserted " + count+ " ratings") //Just to double check the 100000 ratings
    (userbox, movie_to_ratings) //return statement
  }

  //Maps each movie to its r_m = average star rating by users who rated that movie
  def average(map: mutable.Map[String, Array[Double]]): mutable.Map[String, Double] = {
    var av_movie_map: mutable.Map[String, Double] = mutable.Map()
    //Iterates the whole Map of movie -> array of ratings
    for ((key,arr) <- map){
      val average = arr.sum/arr.length //using functions of the class array, calculates the average of the ratings
      av_movie_map = av_movie_map + (key -> average) //Adds a new (key->value) pair to the map, which is (movie->r_m)
    }
    av_movie_map //Returns the map
  }
}

/** Executable object to test the code */
object UserBaseSB extends App {
  val tuple = UserReaders.readEntries
  var userdatabase = tuple._1
  var movie_rating = tuple._2
  /*for((keys,ratings)<-movie_rating){
    println(ratings.toList)
  }*/
  val r_m_map = UserReaders.average(movie_rating)
  /*for((key,av)<-r_m_map){
    println(av)
  }*/
}