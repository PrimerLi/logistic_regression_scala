import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.io._
import scala.math._
import scala.util.control.Breaks._

class Logistic
{
    private var fileName: String = ""
    private var hours: ListBuffer[Double] = new ListBuffer[Double]()
    private var pass: ListBuffer[Double] = new ListBuffer[Double]()
    private var length: Int = 0
    private var range: Range = Range(0, 0)
    private def f(a: Double): Boolean = 
    {
        return true
    }
    def this(fileName: String)
    {
        this()
        this.fileName = fileName
        val bufferedSource = Source.fromFile(fileName)
        for (line <- bufferedSource.getLines)
        {
            var tempArray:Array[String] = line.split("\t")
            hours.append(tempArray(0).toDouble)
            pass.append(tempArray(1).toDouble)
        }
        bufferedSource.close
        length = hours.count(f)
        range = Range(0, length)
    }

    def printData(): Unit = 
    {
        var writer = new PrintWriter(new File("test.txt"))
        for (i <- range)
        {
            writer.write(hours(i).toString + "  " + pass(i).toString + "\n")
        }
        writer.close
    }

    def minusLog(theta0: Double, theta1: Double): Double = 
    {
        var sum: Double = 0
        for (i <- range)
        {
            var alpha: Double = theta0 + theta1*hours(i)
            sum = sum + pass(i)*log(1 + exp(alpha)) + (1 - pass(i))*log(1 + exp(-alpha))
        }
        return sum
    }

    def gradient(theta0: Double, theta1: Double): Vector = 
    {
        var result:Vector = new Vector(2)
        for (i <- range)
        {
            var X:Vector = new Vector(2)
            X.setElement(0, 1)
            X.setElement(1, hours(i))
            var alpha:Double = theta0 + theta1 * hours(i)
            result = result.add(X.scale(pass(i)/(1 + exp(-alpha)) - (1 - pass(i))/(1 + exp(alpha))))
        }
        return result
    }

    def hessian(theta0: Double, theta1: Double): Matrix = 
    {
        var result: Matrix = new Matrix(2,2)
        for (i <- range)
        {
            var matrix: Matrix = new Matrix(2,2)
            matrix.setElement(0, 0, 1.0)
            matrix.setElement(0, 1, hours(i))
            matrix.setElement(1, 0, hours(i))
            matrix.setElement(1, 1, hours(i)*hours(i))
            var alpha: Double = theta0 + theta1*hours(i)
            result = result.add(matrix.scale(0.25/(cosh(0.5*alpha)*cosh(0.5*alpha))))
        }
        return result
    }

    def printGradient(theta0: Double, theta1: Double): Unit = 
    {
        println(gradient(theta0, theta1))
    }

    def printHessian(theta0: Double, theta1: Double): Unit = 
    {
        println(hessian(theta0, theta1))
    }

    def newton(theta0: Double, theta1: Double, rate: Double = 0.01): Vector = 
    {
        var result = new Vector(2)
        result.setElement(0, theta0)
        result.setElement(1, theta1)
        var count: Int = 0
        var iterationMax: Int = 10000
        val eps = 1.0e-12
        breakable
        {
            var old: Vector = result
            while(true)
            {
                count = count + 1
                if (count > iterationMax) break
                old = result
                var hessianMatrix:Matrix = this.hessian(result.getElement(0), result.getElement(1))
                var hessianInverse:Matrix = new Matrix(2,2)
                var det: Double = hessianMatrix.getElement(0, 0)*hessianMatrix.getElement(1,1) - hessianMatrix.getElement(0, 1)*hessianMatrix.getElement(1, 0)
                if (true)
                {
                    hessianInverse.setElement(0, 0, rate)
                    hessianInverse.setElement(1, 1, rate)
                }
                else
                {
                    hessianInverse.setElement(0, 0, hessianMatrix.getElement(1, 1)/det)
                    hessianInverse.setElement(1, 1, hessianMatrix.getElement(0, 0)/det)
                    hessianInverse.setElement(0, 1, -hessianMatrix.getElement(0, 1)/det)
                    hessianInverse.setElement(1, 0, -hessianMatrix.getElement(1, 0)/det)
                }
                result = result.subtract(hessianInverse.product(this.gradient(result.getElement(0), result.getElement(1))))
                var error = result.subtract(old).norm()
                //println("count = " + count + ", error = " + error)
                //println(result)
                if (error < eps) break
            }
        }
        return result
    }

    def printResult(result: Vector):Unit = 
    {
        def logisticFunction(result: Vector, x: Double): Double = 
        {
            var alpha: Double = result.getElement(0) + result.getElement(1)*x
            return 1.0/(1.0 + exp(alpha))
        }
        var writer = new PrintWriter(new File("result.txt"))
        for (i <- range)
        {
            writer.write(hours(i).toString + "  " + logisticFunction(result, hours(i)) + "\n")
        }
        writer.close
        writer = new PrintWriter(new File("theta.txt"))
        writer.write("theta_0 = " + result.getElement(0) + "\n")
        writer.write("theta_1 = " + result.getElement(1) + "\n")
        writer.close
    }
}
