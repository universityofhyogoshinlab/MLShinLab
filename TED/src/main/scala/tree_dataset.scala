import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.util.matching.Regex

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import com.Ostermiller.util.MD5

case class MyHash() {
  val md5 = new MD5

  def digest(s: String): String = {
    md5.update(s.getBytes)
    javax.xml.bind.DatatypeConverter.printBase64Binary(md5.getHash())
  }
}

case class TreeNode(l: String, p: Option[TreeNode], c: ArrayBuffer[TreeNode]) {
  var label = l
  var parent = p
  val children = c
  var refID = "DUMMY"
  var stringID = "DUMMY"
  var nodeN = -1

  // String Identifier
  override def toString: String = {
    if(stringID == "DUMMY") {
      stringID = label + children.map(_.toString).mkString("(", ",", ")")
    }
    stringID
  }


  // Reference Identifgier
  def ref: String = {
    if(refID == "DUMMY") {
      val myHash = new MyHash
      refID = myHash.digest(toString)
    }
    refID
  }

  // 木のサイズ
  def size: Int = {
    if(nodeN < 0) {
      nodeN = children.foldLeft(1){(x,y) => x + y.size}
    }
    nodeN
  }
}

class TreeDataset(file_name: String, weight: Double) {

  val labels = ArrayBuffer[String]()
  val labelP = """([^:]*):(.*)""".r

  var ln = 1
  val f = Source.fromFile(file_name)
  val trees: Array[Array[TreeNode]] =
    for (line <- f.getLines.toArray; val labelP(label, tree) = line) yield {
      val pack = TreeParser.parse(line)
      if(pack.size == 0) {
        println("Some error in line " + ln)
        System.exit(1)
      } 
      labels += label
      ln += 1
      pack
    }

  val tn = trees.size

  val pairs =
    {for(i <- 0 to tn-2; j <- i+1 until tn) yield {
      ((i, trees(i)), (j, trees(j)))}}.toArray

  def pack(array: Array[((Int, Array[TreeNode]), (Int, Array[TreeNode]))],
    n: Int):
      Array[Array[((Int, Array[TreeNode]), (Int, Array[TreeNode]))]] = {
    var t = array
    val temp =
      ArrayBuffer[Array[((Int, Array[TreeNode]), (Int, Array[TreeNode]))]]()
    while(t.size > 0) {
      val pair = t.splitAt(n)
      temp += pair._1
      t = pair._2
    }
    temp.toArray
  }

  def computeDM(shape: String, gap: Boolean, float: Boolean, exact: Boolean,
    jobSize: Int): collection.immutable.Map[(Int, Int), Double] = {

    val sc = new SparkContext("local[*]", "tree_distance")

    
    val list = pack(pairs, jobSize)

    val ted = new TED()

    val matrix = list.flatMap{pairList =>
      pairList.map{case (x, y) => 
        (x._1, y._1) ->
        ted.eval(x._2(0), y._2(0), shape, gap, float, exact, weight)}
    }

//    val ted = new TED(shape, gap, float, exact)
    
//    val list = sc.parallelize(pack(pairs, jobSize))

    // val matrix = list.flatMap{pairList =>
    //   val ted = new TED()
    //   pairList.map{case (x, y) => 
    //     (x._1, y._1) ->
    //     ted.eval(x._2(0), y._2(0), shape, gap, float, exact, weight)}
    // }.cache().collect()

    //   pairList.map{case (x, y) => (x._1, y._1) -> ted.eval(x._2(0), y._2(0))}
    // }.cache().collect()
    
    sc.stop()

    matrix.toMap
  }
}

//class TED(shape: String, gap: Boolean, float: Boolean, exact: Boolean) {

//case class TED(weight: Double) {

case class TED() {

  val weight = 0.5

  val d_db = Map[(String, String), Double]()
  val e_db = Map[(String, String), Double]()
  val infty: Double = 100000000.0 // 無限大を意味する。

  /*******************
   距離関数の定義
   *******************/

  // ラベル間の距離関数
//  def c(x: TreeNode, y: TreeNode): Double = if(x.label == y.label) 0 else weight

  def calcRef(array: Seq[TreeNode]): String = {
    val myHash = new MyHash
    myHash.digest(array.map(_.ref).mkString("::"))
  }

  val dummy: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = (tn_x, tn_y, c) => 0

  val tai: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    def d( x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) + d(t.children, u.children) + d(x.tail, y.tail)
          val v1 = d(t.children ++ x.tail, y) + 1
          val v2 = d(x, u.children ++ y.tail) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }

    d(ArrayBuffer(tn_x), ArrayBuffer(tn_y))
  }

  val fttt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) match {
            case 0 => d(t.children, u.children) + d(x.tail, y.tail)
            case 1 => infty
          }
          val v1 = d(t.children ++ x.tail, y) + 1
          val v2 = d(x, u.children ++ y.tail) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }

    d(ArrayBuffer(tn_x), ArrayBuffer(tn_y))
  }

  val fftf: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) + e(t.children, u.children) + d(x.tail, y.tail)
          val v1 = d(t.children ++ x.tail, y) + 1
          val v2 = d(x, u.children ++ y.tail) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }

    def e(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(e_db.isDefinedAt((x_ref, y_ref))) return e_db((x_ref, y_ref))
      if(e_db.isDefinedAt((y_ref, x_ref))) return e_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) + e(t.children, u.children) + e(x.tail, y.tail)
          val v1 = e(x.tail, y) + t.size
          val v2 = e(x, y.tail) + u.size

          val v = List(v0, v1, v2).min
          e_db((x_ref, y_ref)) = v
          return v
      }
    }

    d(ArrayBuffer(tn_x), ArrayBuffer(tn_y))
  }

  val fftt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) match {
            case 0 => e(t.children, u.children) + d(x.tail, y.tail)
            case 1 => infty
          }
          val v1 = d(t.children ++ x.tail, y) + 1
          val v2 = d(x, u.children ++ y.tail) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }

    def e(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(e_db.isDefinedAt((x_ref, y_ref))) return e_db((x_ref, y_ref))
      if(e_db.isDefinedAt((y_ref, x_ref))) return e_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) match {
            case 0 => e(t.children, u.children) + e(x.tail, y.tail)
            case 1 => infty
          }
          val v1 = e(x.tail, y) + t.size
          val v2 = e(x, y.tail) + u.size

          val v = List(v0, v1, v2).min
          e_db((x_ref, y_ref)) = v
          return v
      }
    }

    d(ArrayBuffer(tn_x), ArrayBuffer(tn_y))
  }

  val tttf: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    var v = 0.0

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      v = e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      v = e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = c(tn_x, tn_y) + d(tn_x.children, tn_y.children)
      val v1 = tn_x.children.
        map(t => tttf(t, tn_y, c) + tn_x.size - t.size)
      val v2 = tn_y.children.
        map(t => tttf(tn_x, t, c) + tn_y.size - t.size)
      val v3 = tn_x.size + tn_y.size

      v = (ArrayBuffer(v0, v3) ++ v1 ++ v2).min
      e_db((tn_x.ref, tn_y.ref)) = v
    }

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) + d(t.children, u.children) + d(x.tail, y.tail)
          val v1 = d(t.children ++ x.tail, y) + 1
          val v2 = d(x, u.children ++ y.tail) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }
    v
  }

  val tttt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    var v = 0.0

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      v = e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      v = e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0: Double = c(tn_x, tn_y) match {
        case 0 => d(tn_x.children, tn_y.children)
        case _ => infty
      }
      val v1 = tn_x.children.
        map(t => tttt(t, tn_y, c) + tn_x.size - t.size) :+ (tn_x.size + tn_y.size).toDouble
      val v2 = tn_y.children.
        map(t => tttt(tn_x, t, c) + tn_y.size - t.size) :+ (tn_x.size + tn_y.size).toDouble

      v = ((v0 +: v1) ++ v2).min
      e_db((tn_x.ref, tn_y.ref)) = v
    }

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) match {
            case 0 => d(t.children, u.children) + d(x.tail, y.tail)
            case _ => infty
          }
          val v1 = d(t.children ++ x.tail, y) + 1
          val v2 = d(x, u.children ++ y.tail) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }
    v
  }

  val tftf: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    var v = 0.0

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      v = e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      v = e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = c(tn_x, tn_y) + d(tn_x.children, tn_y.children)
      val v1 = tn_x.children.
        map(t => tftf(t, tn_y, c) + tn_x.size - t.size) :+ (tn_x.size + tn_y.size).toDouble
      val v2 = tn_y.children.
        map(t => tftf(tn_x, t, c) + tn_y.size - t.size) :+ (tn_x.size + tn_y.size).toDouble

      v = ((v0 +: v1) ++ v2).min
      e_db((tn_x.ref, tn_y.ref)) = v
    }

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) + d(t.children, u.children) + d(x.tail, y.tail)
          val v1 = d(x.tail, y) + t.size
          val v2 = d(x, y.tail) + u.size

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }

    v
  }

  val tftt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    var v = 0.0

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      v = e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      v = e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = c(tn_x, tn_y) match {
        case 0 => d(tn_x.children, tn_y.children)
        case _ => infty
      }
      val v1 = tn_x.children.
        map(t => tftt(t, tn_y, c) + tn_x.size - t.size) :+ (tn_x.size + tn_y.size).toDouble
      val v2 = tn_y.children.
        map(t => tftt(tn_x, t, c) + tn_y.size - t.size) :+ (tn_x.size + tn_y.size).toDouble

      v = ((v0 +: v1) ++ v2).min
      e_db((tn_x.ref, tn_y.ref)) = v
    }

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {
      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_ + _.size)
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_ + _.size)
        case _ =>
          val t = x.head
          val u = y.head
          val v0 = c(t, u) match {
            case 0 => d(t.children, u.children) + d(x.tail, y.tail)
            case _ => infty
          }
          val v1 = d(x.tail, y) + t.size
          val v2 = d(x, y.tail) + u.size

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }

    v
  }

  val constrained: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>
    // dは森を引数にとるように拡張したもの

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {

      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      // x, yはList[TreeNode]、即ち、森とする。
      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_+_.size)
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_+_.size)
        case (ArrayBuffer(t), ArrayBuffer(u)) => // 入力が両方とも木の時
          val v0 = c(t, u) + d(t.children, u.children)
          val v1 = d(ArrayBuffer(t), u.children) + 1
          val v2 = d(t.children, ArrayBuffer(u)) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
        case _ =>
          val v0 = e(x, y) // x, yの木の間に一対一対応
          val v1 = if(y.size == 1) {
            ArrayBuffer(d(x, y(0).children) + 1)
          } else {
            y.map{t => d(x, ArrayBuffer(t)) +
              (d(ArrayBuffer(), y) - d(ArrayBuffer(), ArrayBuffer(t)))}
          }
          // マッピングはtに集中
          val v2 = if(x.size == 1){
            ArrayBuffer(d(x(0).children, y) + 1)
          } else {
            x.map{t => d(ArrayBuffer(t), y) +
              (d(x, ArrayBuffer()) - d(ArrayBuffer(t), ArrayBuffer()))}
          }
          // マッピングはtに集中
          val v = (ArrayBuffer(v0) ++ v1 ++ v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }
    
    def e(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {

      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(e_db.isDefinedAt((x_ref, y_ref))) return e_db((x_ref, y_ref))
      if(e_db.isDefinedAt((y_ref, x_ref))) return e_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return d(x, y)
        case (_, ArrayBuffer()) => return d(x, y)
        case (ArrayBuffer(), _) => return d(x, y)
        case _ =>
          val v0 = d(ArrayBuffer(x(0)), ArrayBuffer(y(0))) + e(x.tail, y.tail)
          val v1 = e(x, y.tail) + d(ArrayBuffer(), ArrayBuffer(y(0)))
          val v2 = e(x.tail, y) + d(ArrayBuffer(x(0)), ArrayBuffer())

          val v = List(v0, v1, v2).min
          e_db((x_ref, y_ref)) = v
          return v
      }
    }

    d(ArrayBuffer(tn_x), ArrayBuffer(tn_y))
  }

  val sttt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>
    // dは森を引数にとるように拡張したもの

    def d(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {

      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(d_db.isDefinedAt((x_ref, y_ref))) return d_db((x_ref, y_ref))
      if(d_db.isDefinedAt((y_ref, x_ref))) return d_db((y_ref, x_ref))

      // x, yはList[TreeNode]、即ち、森とする。
      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (_, ArrayBuffer()) => return x.foldLeft(0)(_+_.size)
        case (ArrayBuffer(), _) => return y.foldLeft(0)(_+_.size)
        case (ArrayBuffer(t), ArrayBuffer(u)) => // 入力が両方とも木の時
          val v0 = c(t, u) match {
            case 0 => d(t.children, u.children)
            case _ => infty
          }
          val v1 = d(ArrayBuffer(t), u.children) + 1
          val v2 = d(t.children, ArrayBuffer(u)) + 1

          val v = List(v0, v1, v2).min
          d_db((x_ref, y_ref)) = v
          return v
        case _ =>
          val v0 = e(x, y) // x, yの木の間に一対一対応
          val v1 = if(y.size == 1) {
            ArrayBuffer(d(x, y(0).children) + 1)
          } else {
            y.map{t => d(x, ArrayBuffer(t)) +
              (d(ArrayBuffer(), y) - d(ArrayBuffer(), ArrayBuffer(t)))}
          }
          // マッピングはtに集中
          val v2 = if(x.size == 1){
            ArrayBuffer(d(x(0).children, y) + 1)
          } else {
            x.map{t => d(ArrayBuffer(t), y) +
              (d(x, ArrayBuffer()) - d(ArrayBuffer(t), ArrayBuffer()))}
          }
          // マッピングはtに集中
          val v = (ArrayBuffer(v0) ++ v1 ++ v2).min
          d_db((x_ref, y_ref)) = v
          return v
      }
    }
    
    def e(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {

      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(e_db.isDefinedAt((x_ref, y_ref))) return e_db((x_ref, y_ref))
      if(e_db.isDefinedAt((y_ref, x_ref))) return e_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return d(x, y)
        case (_, ArrayBuffer()) => return d(x, y)
        case (ArrayBuffer(), _) => return d(x, y)
        case _ =>
          val v0 = d(ArrayBuffer(x(0)), ArrayBuffer(y(0))) + e(x.tail, y.tail)
          val v1 = e(x, y.tail) + d(ArrayBuffer(), ArrayBuffer(y(0)))
          val v2 = e(x.tail, y) + d(ArrayBuffer(x(0)), ArrayBuffer())

          val v = List(v0, v1, v2).min
          e_db((x_ref, y_ref)) = v
          return v
      }
    }

    d(ArrayBuffer(tn_x), ArrayBuffer(tn_y))
  }

  val lustar: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = { (tn_x, tn_y, c) =>
    
    def d(x: TreeNode, y: TreeNode): Double = {

      if(d_db.isDefinedAt((x.ref, y.ref))) return d_db((x.ref, y.ref))
      if(d_db.isDefinedAt((y.ref, x.ref))) return d_db((y.ref, x.ref))

      val v0 = e(x.children, y.children) + c(x, y)
      val v1 = y.children.map{w => d(x, w) + y.size - w.size}
      val v2 = x.children.map{w => d(w, y) + x.size - w.size}

      val v = (ArrayBuffer(v0) ++ v1 ++ v2).min
      d_db((x.ref, y.ref)) = v
      return v
    }

    def e(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {

      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(e_db.isDefinedAt((x_ref, y_ref))) return e_db((x_ref, y_ref))
      if(e_db.isDefinedAt((y_ref, x_ref))) return e_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (_, ArrayBuffer()) => return x.foldLeft(0){(s,t) => s + t.size}
        case (ArrayBuffer(), _) => return y.foldLeft(0){(s,t) => s + t.size}
        case _ =>
          val v0 = e(x.tail, y) + x(0).size
          val v1 = e(x, y.tail) + y(0).size
          val v2 = d(x(0), y(0)) + e(x.tail, y.tail)

          val v = List(v0, v1, v2).min
          e_db((x_ref, y_ref)) = v
          return v 
      }
    }
    
    d(tn_x, tn_y)
  }

  val mast: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = { (tn_x, tn_y, c) =>

    def d(x: TreeNode, y: TreeNode): Double = {

      if(d_db.isDefinedAt((x.ref, y.ref))) return d_db((x.ref, y.ref))
      if(d_db.isDefinedAt((y.ref, x.ref))) return d_db((y.ref, x.ref))

      val v0 = c(x, y) match {
        case 0 => e(x.children, y.children)
        case _ => infty
      }
      val v1 = y.children.map{w => d(x, w) + y.size - w.size}
      val v2 = x.children.map{w => d(w, y) + x.size - w.size}

      val v = (ArrayBuffer(v0) ++ v1 ++ v2).min
      d_db((x.ref, y.ref)) = v
      return v
    }

    def e(x: ArrayBuffer[TreeNode], y: ArrayBuffer[TreeNode]): Double = {

      val x_ref = calcRef(x)
      val y_ref = calcRef(y)

      if(e_db.isDefinedAt((x_ref, y_ref))) return e_db((x_ref, y_ref))
      if(e_db.isDefinedAt((y_ref, x_ref))) return e_db((y_ref, x_ref))

      (x, y) match {
        case (ArrayBuffer(), ArrayBuffer()) => return 0
        case (_, ArrayBuffer()) => return x.foldLeft(0){(s,t) => s + t.size}
        case (ArrayBuffer(), _) => return y.foldLeft(0){(s,t) => s + t.size}
        case _ =>
          val v0 = e(x.tail, y) + x(0).size
          val v1 = e(x, y.tail) + y(0).size
          val v2 = d(x(0), y(0)) + e(x.tail, y.tail)

          val v = List(v0, v1, v2).min
          e_db((x_ref, y_ref)) = v
          return v 
      }
    }

    d(tn_x, tn_y)
  }

  val pttf: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = c(tn_x, tn_y) + tn_x.size + tn_y.size - 2 +
        (0.0 +: tn_x.children.flatMap(t => tn_y.children.map(u =>
          pttf(t, u, c) - t.size - u.size))).min
      val v1 = tn_x.children.
        map(t => pttf(t, tn_y, c) + tn_x.size - t.size) :+ (tn_x.size + tn_y.size).toDouble
      val v2 = tn_y.children.
        map(t => pttf(tn_x, t, c) + tn_y.size - t.size) :+ (tn_x.size + tn_y.size).toDouble

      val v = ((v0 +: v1) ++ v2).min
      e_db((tn_x.ref, tn_y.ref)) = v
      v
    }
  }

  val pttt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = c(tn_x, tn_y) match {
        case 0 => tn_x.size + tn_y.size - 2 +
            (0.0 +: tn_x.children.flatMap(t => tn_y.children.map (u =>
              pttt(t, u, c) - t.size - u.size))).min
        case _ => infty
      }
      val v1 = tn_x.children.
        map(t => pttt(t, tn_y, c) + tn_x.size - t.size) :+ (tn_x.size + tn_y.size).toDouble
      val v2 = tn_y.children.
        map(t => pttt(tn_x, t, c) + tn_y.size - t.size) :+ (tn_x.size + tn_y.size).toDouble

      val v = ((v0 +: v1) ++ v2).min
      e_db((tn_x.ref, tn_y.ref)) = v
      v
    }
  }

  val pftf: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    var v = 0.0

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      v = e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      v = e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = d(tn_x, tn_y)
      val v1 = tn_x.children.map(t => pftf(t, tn_y, c) + tn_x.size - t.size)
      val v2 = tn_y.children.map(t => pftf(tn_x, t, c) + tn_y.size - t.size)
      val v3 = tn_x.size + tn_y.size.toDouble

      v = ((v0 +: v1) ++ v2 :+ v3).min
      e_db((tn_x.ref, tn_y.ref)) = v
    }

    // ルートから始まる連続パス
    def d(x: TreeNode, y: TreeNode): Double = {
      c(x, y) + x.size + y.size - 2 +
      (0.0 +: x.children.flatMap(t => y.children.map(u =>
        d(t, u) - t.size - u.size))).min
    }
    v
  }

  val pftt: (TreeNode, TreeNode, (TreeNode, TreeNode) => Double) => Double = {(tn_x, tn_y, c) =>

    var v = 0.0

    if(e_db.isDefinedAt((tn_x.ref, tn_y.ref))) {
      v = e_db((tn_x.ref, tn_y.ref))
    } else if(e_db.isDefinedAt((tn_y.ref, tn_x.ref))) {
      v = e_db((tn_y.ref, tn_x.ref))
    } else {
      val v0 = c(tn_x, tn_y) match {
        case 0 => d(tn_x, tn_y)
        case _ => infty
      }
      val v1 = tn_x.children.map(t => pftt(t, tn_y, c) + tn_x.size - t.size)
      val v2 = tn_y.children.map(t => pftt(tn_x, t, c) + tn_y.size - t.size)
      val v3 = tn_x.size + tn_y.size.toDouble

      v = ((v0 +: v1) ++ v2 :+ v3).min
      e_db((tn_x.ref, tn_y.ref)) = v
    }

    // ルートから始まる連続パス
    def d(x: TreeNode, y: TreeNode): Double = {
      c(x, y) match {
        case 0 => x.size + y.size - 2 +
            (0.0 +: x.children.flatMap(t => y.children.map(u =>
              d(t, u) - t.size - u.size))).min
        case _ => infty
      }
    }

    v
  }

  def eval (x: TreeNode, y: TreeNode, 
    shape: String, gap: Boolean, float: Boolean, exact: Boolean,
    weight: Double): Double = {
    val c:  (TreeNode, TreeNode) => Double =
      (x, y) => if(x.label == y.label) 0.0 else weight
    (shape, gap, float, exact) match {
      case ("f", true, true, false) => tai(x, y, c)
      case ("f", true, true, true) => fttt(x, y, c)
      case ("f", false, true, false) => fftf(x, y, c)
      case ("f", false, true, true) => fftt(x, y, c)
      case ("t", true, true, false) => tttf(x, y, c)
      case ("t", true, true, true) => tttt(x, y, c)
      case ("t", false, true, false) => tftf(x, y, c)
      case ("t", false, true, true) => tftt(x, y, c)
      case ("s", true, true, false) => constrained(x, y, c)
      case ("s", true, true, true) => sttt(x, y, c)
      case ("a", true, true, false) => lustar(x, y, c)
      case ("a", true, true, true) => mast(x, y, c)
      case ("p", true, true, false) => pttf(x, y, c)
      case ("p", true, true, true) => pttt(x, y, c)
      case ("p", false, true, false) => pftf(x, y, c)
      case ("p", false, true, true) => pftt(x, y, c)
      case _ =>
        print("shape = " + shape + "; gap = " + gap)
        println("; float = " + float + "; exact = " + exact)
        println("Sorry.  This combination of parameters is not supported.")
        dummy(x, y, c)
    }
  }
}
 
object TreeParser {
  def parse(src: String): Array[TreeNode] = {
    val p = new Regex("""([^,\s\(\)]*)\s*([,\s\(\)]?)[,\s]*(.*)""")
    val i_root = TreeNode("IMAGINARY", None, ArrayBuffer[TreeNode]())
    var crr_n = i_root
    var delim = "("
    var remain = src
    val temp = ArrayBuffer[TreeNode]()

    while (remain != ""){
      val p(label,d,r) = remain
      delim = d
      remain = r

      if(label == "") {
        if(delim == ")") {
          crr_n.parent match {
            case Some(p) => crr_n = p
            case None => return Array[TreeNode]()
          }
        } else return Array[TreeNode]()
      } else {
        val new_n = TreeNode(label, Some(crr_n), ArrayBuffer[TreeNode]())
        temp += new_n
        crr_n.children += new_n
        delim match {
          case ")" =>
            crr_n.parent match {
              case Some(p) => crr_n = p
              case None => return Array[TreeNode]()
            }
          case "(" =>
            crr_n = new_n
          case _ =>
            // do nothing
        }
      }
    }
    if(crr_n == i_root) {
      i_root.children(0).parent = None
      if(i_root.children(0) != temp(0)) println("DEBUG: Ooops!")
      temp.toArray
    } else {
      Array[TreeNode]()
    }
  }
}

// object TEST {
//   def main(args: Array[String]) {
//     val shape = "p"
//     val gap = false
//     val float = true
//     val exact = true

//     val ted = new TED(shape, gap, float, exact)

//     val testSeq = Array(
//       ("a", "a"),
//       ("a", "b"),
//       ("a", "a(b,c)"),
//       ("b", "a(b,c)"),
//       ("d", "a(b,c)"),
//       ("a(b,c)", "a(d,e)"),
//       ("a(b,c,d)", "a(b,e,c)"),
//       ("a(e(a(b(c(a,b),a(c,c)))))", "e(f(b(a(c(a,c),c))))")
//     )

//     println("TEST for shape=" + shape + ", gop=" + gap +", float=" + float + ", exact=" + exact)

//     testSeq.foreach {case (x, y) =>
//       val t = TreeParser.parse(x)
//       val u = TreeParser.parse(y)
//       println(x + " , " + y + " -> " + ted.eval(t.get, u.get))
//     }
//   }
// }
