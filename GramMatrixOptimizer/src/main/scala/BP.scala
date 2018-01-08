import scala.collection.mutable.Map
import scala.util.matching.Regex

case class BP(var entity: collection.mutable.Map[(Int, Int), Double]) {
  this.reduce

  def this() = this(collection.mutable.Map[(Int, Int), Double]())

  def this(x: Double) = this(collection.mutable.Map((0,0)->x))

  def this(x: Int, y: Int, v: Double) = this(collection.mutable.Map((x, y)->v))
  
  def isNull: Boolean = entity.isEmpty

  def make(expr: String): BP = {
    val term = new util.matching.Regex("""([+-])?(\d+\.?\d*)?\*?([xy])?(\^\d+)?\*?([xy])?(\^\d+)?(.*)""")
    var exp1 = 0
    var exp2 = 0
    var cef = 0.0
    entity = collection.mutable.Map[(Int,Int),Double]()
    var s = expr
    while(s != "") {
        s match {
            case term(sgn, c, v1, e1, v2, e2, remain) =>
                if(c == null) cef = 1.0 else cef = c.toDouble
                if(sgn == "-") cef *= -1
                if(e1 == null) exp1 = 1 else exp1 = e1.tail.toInt
                if(e2 == null) exp2 = 1 else exp2 = e2.tail.toInt
                s = remain
                if(v1 == "x" && v2 == "y") entity += (exp1, exp2) -> cef
                else if(v1 == "y" && v2 == "x") entity += (exp2, exp1) -> cef
                else if(v1 == "x" && v2 == null) entity += (exp1, 0) -> cef
                else if(v1 == "y" && v2 == null) entity += (0, exp1) -> cef
                else if(v1 == null && v2 == null && c != null) entity += (0, 0) -> cef
                else {
                    entity = collection.mutable.Map[(Int,Int),Double]()
                    s = ""
                }
            case _ =>
                entity = collection.mutable.Map[(Int,Int),Double]()
                s = ""
        }
    }
    this
  }

  def reduce: BP = {
    entity = entity.filter(_._2 != 0.0)
    if(entity.isEmpty) entity = collection.mutable.Map((0,0)->0.0)
    this
  }
  
  def +(that: BP): BP = {
    var temp = collection.mutable.Map[(Int, Int), Double]()
    entity.foreach(temp+=_)
    that.entity.foreach(tpl =>
      if(temp.isDefinedAt(tpl._1))
        temp(tpl._1) += tpl._2
      else
        temp += tpl
    )
    (new BP(temp)).reduce
  }

  def +(x: Double): BP = {
    (this + new BP(x)).reduce
  }

  def -(that: BP): BP = {
    var temp = collection.mutable.Map[(Int, Int), Double]()
    entity.foreach(temp+=_)
    that.entity.foreach(tpl =>
      if(temp.isDefinedAt(tpl._1))
        temp(tpl._1) -= tpl._2
      else
        temp += tpl._1 -> -tpl._2
    )
    (new BP(temp)).reduce
  }
  def -(x: Double): BP = {
    (this - new BP(x)).reduce
  } 


  def *(that: BP): BP = {
    var answer = new BP(0)
    for(x <- entity; y <- that.entity)
      answer += new BP(collection.mutable.Map((x._1._1 + y._1._1, x._1._2 + y._1._2) -> x._2 * y._2))
    answer.reduce
  }

  def *(x: Double): BP = {
    (this * new BP(x)).reduce
  }

  def +=(that: BP) = {
    that.entity.foreach(tpl =>
      if(entity.isDefinedAt(tpl._1))
        entity(tpl._1) += tpl._2
      else
        entity += tpl
    )
    this.reduce
  }

  def -=(that: BP) = {
    that.entity.foreach(tpl =>
      if(entity.isDefinedAt(tpl._1))
        entity(tpl._1) -= tpl._2
      else
        entity += tpl._1 -> -tpl._2
    )
    this.reduce
  }

  def *=(that: BP) = {
    var temp = new BP(0)
    for(x <- entity; y <- that.entity)
      temp += new BP(collection.mutable.Map((x._1._1 + y._1._1, x._1._2 + y._1._2) -> x._2 * y._2))
    entity = temp.entity
    this.reduce
  }
  
  def substitute(x: Double, y: Double): Double = {
    entity.foldLeft(0.0){case (res, ((i,j), c)) => res + c * math.pow(x,i) * math.pow(y,j)}
  }

  override def toString(): String = {
    var s: String = ""
    var terms: List[(Int, Int, Double)] = Nil
    entity.map(tpl => terms ::= (tpl._1._1, tpl._1._2, tpl._2))
    terms = terms.sortWith((x,y)=> x._1<y._1 || x._1==y._1 && x._2<y._2).reverse
    terms.foreach{tpl =>
      if(s == "" && tpl._3 > 0)
        s += tpl._3
      else if(tpl._3 > 0)
        s += "+" + tpl._3
      else
        s += "-" + -tpl._3
      tpl._1 match{
        case 0 =>
        case 1 => s += "x"
        case i => s += "x^" + i
      }
      tpl._2 match{
        case 0 =>
        case 1 => s += "y"
        case j => s += "y^" + j
      }
    }
    s
  }
}
