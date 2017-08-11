import scala.collection.mutable.ListBuffer
import scala.math._

class Vector
{
    private var len: Int = 0
    private var array: ListBuffer[Double] = new ListBuffer[Double]()
    private def f(a: Double): Boolean = 
    {
        return true
    }
    
    def this(len: Int)
    {
        this()
        this.len = len
        for (i <- 0 to len - 1)
        {
            this.array.append(0)
        }
    }

    def this(a: ListBuffer[Double])
    {
        this()
        this.len = a.count(f)
        for (i <- 0 to len - 1)
        {
            this.array.append(a(i))
        }
    }

    override def toString(): String = 
    {
        //var result: String = "Length = " + this.len.toString + "\n"
        var result: String = ""
        for (i <- 0 to len-1)
        {
            result = result + this.array(i).toString + "  "
        }
        //result = result + "\n"
        return result
    }

    def getLength(): Int = 
    {
        return this.len;
    }

    def setLength(len: Int): Unit = 
    {
        this.len = len
        array.clear()
        for (i <- 0 to len - 1)
        {
            array.append(0)
        }
    }

    def scale(factor: Double): Vector = 
    {
        var result: Vector = new Vector(this.len)
        for (i <- 0 to len-1)
        {
            result.array(i) = factor*this.array(i)
        }
        return result
    }

    def innerProduct(vector: Vector): Double = 
    {
        if (vector.len != this.len)
        {
            println("Vector dimensions are incompatible. ")
            System.exit(-1)
        }
        var result: Double = 0.0
        for (i <- 0 to this.len - 1)
        {
            result = result + vector.array(i)*this.array(i)
        }
        return result
    }

    def norm(): Double = 
    {
        return sqrt(this.innerProduct(this))
    }

    def normSquared(): Double = 
    {
        return this.innerProduct(this)
    }

    def normalize(): Vector = 
    {
        var normOfVector = this.norm()
        if (normOfVector < 1.0e-32)
        {
            println("Your vector is not normalizable. ")
            System.exit(-1)
        }
        for (i <- 0 to len-1)
        {
            array(i) = array(i)/normOfVector
        }
        return this
    }

    def assign(vector: Vector): Vector = 
    {
        this.array.clear()
        this.len = vector.len
        for (i <- 0 to this.len - 1)
        {
            this.array.append(vector.array(i))
        }
        return this
    }

    def add(vector: Vector): Vector = 
    {
        if (this.len != vector.len)
        {
            println("Vector dimensions are incompatible. ")
            System.exit(-1)
        }
        var result: Vector = new Vector(this.len)
        for (i <- 0 to this.len - 1)
        {
            result.array(i) = this.array(i) + vector.array(i)
        }
        return result
    }

    def subtract(vector: Vector): Vector = 
    {
        if (this.len != vector.len)
        {
            println("Vector dimensions are incompatible. ")
            System.exit(-1)
        }
        var result: Vector = new Vector(this.len)
        for (i <- 0 to this.len - 1)
        {
            result.array(i) = this.array(i) - vector.array(i)
        }
        return result
    }

    def getElement(index: Int): Double = 
    {
        if (index < 0 || index >= len)
        {
            println("Index out of bounds. ")
            System.exit(-1)
        }
        return this.array(index)
    }

    def setElement(index: Int, value: Double): Unit = 
    {
        if (index < 0 || index >= len)
        {
            println("Index out of bounds. ")
            System.exit(-1)
        }
        this.array(index) = value
    }

    def product(that: Matrix): Vector = 
    {
        if (this.len !=  that.getRow())
        {
            println("Matrix dimensions are incompatible. ")
            System.exit(-1)
        }
        var result: Vector = new Vector(that.getCol())
        for (j <- 0 to that.getCol() - 1)
        {
            var sum: Double = 0.0
            for (i <- 0 to this.len - 1)
            {
                sum = sum + this.array(i)*that.getElement(i, j)
            }
            result.array(j) = sum
        }
        return result
    }

    def sum(): Double = 
    {
        var s: Double = 0.0
        for (i <- 0 to len - 1)
        {
            s = s + this.array(i)
        }
        return s
    }

    def product(): Double = 
    {
        var p: Double = 1.0
        for (i <- 0 to len-1)
        {
            p = p * this.array(i)
        }
        return p
    }
}
