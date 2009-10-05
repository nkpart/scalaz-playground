import org.specs._
    
import scalaz.Scalaz._
//import scalaz.Apply._
import scalaz.Applicative._
import scalaz._
    

object ValidationsAndForms extends Specification {
  case class Person(name : String, age : Int)
  object Person {
    val make = Function.curried(apply _)
  }
  
  val validData = Map("name" -> "Nick", "age" -> "26")
  val emptyData = Map[String, String]()
  val badAgeData = Map("name" -> "Nick", "age" -> "26ys")
  
  val allBad = Map("age" -> "yes please")

  "Given some validated values" in {
    type Errors = List[(String, String)]
    
    val name : Validation[Errors, String] = Success("Nick")
    val emptyName = Failure(List(("name", "Required!")))
        
    val age = Success[List[(String, String)],Int](26)
    val badAge = Failure(List(("age", "must be integer")))

    val cons = Person.make
    
    val y = (cons <|: name) <*>: age
    val x = age <*> (name |> cons)
    val z : Validation[List[(String,String)], Person] = cons </> name <*> age
    
    y must be_==(Success(Person("Nick", 26)))
    x must be_==(y)
    println(y)
  }
  
  "be interesting" in {    
    def required[T](s : String)(f : (String => Option[T])) = 
      f(s) toSuccess { List((s, "Required!")) }
      
    def asInt(s : String)(value : String) = {
      (() => value.toInt).throws match {
        case Right(v) => Success(v)
        case Left(e) => Failure(List((s, "must be integer")))
      }
    }    
    
    val name = required("name")(validData get _)   
    val emptyName = required("name")(emptyData get _)
    
    name must be_==(Success("Nick"))
    emptyName must be_==(Failure(List(("name", "Required!"))))
    
    // Adding types for clarity
    val age : Validation[List[(String, String)], String] = required("age")(validData get _) 
    val intAge : Validation[List[(String, String)], Int] = age >>= asInt("age")
    
    val badAge = required("age")(badAgeData get _) >>= asInt("age")
    
    age must be_==(Success("26"))
    intAge must be_==(Success(26))
    
    badAge must be_==(Failure(List(("age", "must be integer"))))

//    val f = Function.curried(Person.apply _).liftM[PartialApply1Of2[Validation,(String, String)]#Apply]
    
    val x = name liftA (intAge, Function.curried(Person.apply _))
    println(x)
  }
}