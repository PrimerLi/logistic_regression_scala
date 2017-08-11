object VectorTest
{
    def main(args: Array[String]): Unit = 
    {
        var len = 3
        var a: Vector = new Vector(len)
        a.setElement(0, 51)
        a.setElement(1, 60)
        a.setElement(2, 69)
        println(a)
        a.normalize()
        println(a)
        var b = new Vector(len)
        b.setElement(0, 96)
        b.setElement(1, 114)
        b.setElement(2, 132)
        b = b.subtract(a.scale(b.innerProduct(a)))
        b.normalize()
        println(b)
        var c = new Vector(len)
        c.setElement(0, 141)
        c.setElement(1, 168)
        c.setElement(2, 195)
        c = c.subtract((a.scale(c.innerProduct(a))).add(b.scale(c.innerProduct(b))))
        c.normalize()
        println(c)
        println(a.innerProduct(a))
        println(b.innerProduct(b))
        println(c.innerProduct(c))
        println(a.innerProduct(b))
        println(a.innerProduct(c))
        println(b.innerProduct(c))
    }
}
