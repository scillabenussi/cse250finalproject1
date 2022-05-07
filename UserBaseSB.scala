/** File UserBaseSB.scala created by Scilla Benussi for CSE250, Spring 2022
 *  Final Project, due date: May 13th, Midnight stretchy
 */
import java.io.FileWriter
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.io.Source

case class User(user_id: String, rated_movies: Movie_ratings)
case class Movie_ratings(movie_id: String, rating: Double)
//We'll use ISR classes for the whole users+their ratings database
class UserBox extends Cardbox[User]((x,y) => x.user_id.compareTo(y.user_id)) {}

object UserReaders{
  //readEntries method reads the u.data file and creates the userbox, a Cardbox of all the user_ids, with all their related movie_ratings
  def readEntries: (UserBox, Map[String, Array[Double]]) = {
    val userFile = "u.data" //Use fixed version of the file
    val src = Source.fromFile(userFile) //Read from u.data
    var userbox = new UserBox()
    var movie_to_ratings: Map[String, Array[Double]] = Map()
    var user_id = "" //will save the user id
    var movie = "" //will save the movie id
    var rating: Double = 0.0 //Needs to be a double
    var count = 0 //counts the ratings
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

  def average(map: Map[String, Array[Double]]): Map[String, Double] = {
    var av_movie_map: Map[String, Double] = Map()
    for ((key,arr) <- map){
      val average = arr.sum/arr.length
      av_movie_map = av_movie_map + (key -> average)
    }
    av_movie_map
  }
}

object UserBaseSB extends App{
  val tuple = UserReaders.readEntries
  var userdatabase = tuple._1
  var movie_rating = tuple._2
  /*for((keys,ratings)<-movie_rating){
    println(ratings.toList)
  }*/
  val r_m_map = UserReaders.average(movie_rating)
  for((key,av)<-r_m_map){
    println(av)
  }
}