import scala.io.Source

object Main
{
    def main(args: Array[String]): Unit = 
    {
        var fileName: String = "data.txt"
        var logistic: Logistic = new Logistic(fileName)
        var theta0: Double = -1.0
        var theta1: Double = -1.0
        var result: Vector = logistic.newton(theta0, theta1)
        //println(result)
        logistic.printResult(result)
    }
}
