package tuco

object Tuco
  extends SessionIOFunctions
     with ServerIOFunctions {

   val FC = tuco.free.connection
   implicit val AsyncC = FC.AsyncConnectionIO

}
