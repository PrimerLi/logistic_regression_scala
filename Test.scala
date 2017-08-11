import scala.collection.mutable.ListBuffer
import scala.math._
import scala.util.Random

object Test
{
    def main(args: Array[String]): Unit = 
    {
        var random: Random = new Random()
        var matrix: ListBuffer[ListBuffer[Double]] = new ListBuffer[ListBuffer[Double]]()
        var dimension: Int = 4
        var row: Int = dimension
        var col: Int = dimension
        for (i <- 0 to row - 1)
        {
            var list: ListBuffer[Double] = new ListBuffer[Double]()
            for (j <- 0 to col - 1)
            {
                list.append(random.nextDouble)
            }
            matrix.append(list)
        }
        var myMatrix:Matrix = new Matrix(matrix)
        myMatrix = myMatrix.product(myMatrix.transpose())
        println(myMatrix)
        /*var Q: Matrix = new Matrix(dimension, dimension)
        var R: Matrix = new Matrix(dimension, dimension)
        var result = myMatrix.QRDecomposition()
        Q = result._1
        R = result._2
        println("Q * Q^T: ")
        println(Q.product(Q.transpose()))
        println("R: ")
        println(R)
        println("Q*R: ")
        println(Q.product(R))
        println("Error: ")
        println((myMatrix.subtract(Q.product(R))).norm())*/
        /*var inverse = myMatrix.inverse()
        println("Inverse: ")
        println(inverse)
        println("Inverse * matrix: ")
        println(inverse.product(myMatrix))*/
        var (e, o) = myMatrix.eigensystem()
        println("o^T * myMatrix * o = ")
        println(o.transpose().product(myMatrix).product(o))
        println("eigenvalues = ")
        println(e)
        println("Trace = " + myMatrix.trace())
        println(e.sum())
        println(e.product())

    }
}
