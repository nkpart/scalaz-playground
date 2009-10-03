import org.specs._

object ArrowsAndKleislis extends Specification {
  "be interesting" in {
    import scalaz.http.scapps.Scapps._
    import scalaz.Scalaz._
    import scalaz.Arrow._
    import scalaz.Monad._
  
    val arrow = KleisliArrow[Option]
  
    def divBy_?(x : Int) = (y : Int) => if (y % x == 0) Some(y) else None
    def keep(x : Int) = (y : Int) => {
      Some( ("Keeping " + x, y) )
    }
  
    val b = keep(10) >=> arrow.second[Int, Int, String](divBy_?(3) >=> divBy_?(6))
    val a = divBy_?(5) >=> keep(4) >=> arrow.second[Int, (String, Int), String](b)
      
    println(a(30))     
    println(a(10))
    println(a(1))
  
    def consume(s : String) = (xs : List[String]) => if (xs.head == s) Some(xs.tail) else None
    def read = (xs : List[String]) => Some(xs.head, xs.tail)
  
    def foo(payload : (String, (String, List[String]))) = payload
  
    val a1b_ = consume("a") >=> consume("1") >=> consume("b")
    val a_b_ = consume("a") >=> read >=> 
      arrow.second[List[String], (String, List[String]), String](consume("b") >=> read) |> foo _
  
    println(a1b_("a" :: "1" :: "b" :: "2" :: Nil))
    println(a_b_("a" :: "1" :: "b" :: "2" :: Nil))
    
    true must beTrue
  }
  
}