import scala.io.Source
import java.io.PrintWriter
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.util.matching.Regex
import java.text.DecimalFormat

import scopt.OptionParser

case class MainOption(
  parseFile: String = "",
  writeFile: String = "",
  jobSize: Int = 10,
  shape: String = "f",
  gap: String = "t",
  float: String = "t",
  exact: String = "f",
  weight: String = "1.0"
)

object Main {

  val f2 = new DecimalFormat("0.00")

  def main(args: Array[String]) {
    var pFile = ""
    var wFile = ""
    var jobSize = 10
    var shape = "f"
    var gap = true
    var float = true
    var exact = false
    var weight = 1.0

    val parser = new OptionParser[MainOption]("Extract patterns") {
      opt[String]('i', "input") required() valueName("<path>") action {(x, o) => o.copy(parseFile = x)} text("The path to an input file.  Mandatory.")

      opt[String]('o', "output") valueName("<path>") action { (x, o) =>
        o.copy(writeFile = x)
      } text("The path to an output file.  If left out, the programy determines a file name.")

      opt[String]('m', "memoization") valueName("Integer") action { (x, o) =>
        o.copy(jobSize = x.toInt)
      } text("Specify the number of distances computed sharing a single memoization cache.  Optional.  Default = 10.")

      opt[String]('s', "shape") valueName("[f(orest),t(ree),p(ath),a(greement),s(eparable)") action{ (x, o) => o.copy(shape = x)} text("Specify the shape of substructures. Can be abbreviated by the initial letters:  e.g. \"f\" instead of \"forest\".  Optional.  Default = f(alse).")

      opt[String]('g', "gap") valueName("[t,f]") action { (x, o) =>
        o.copy(gap = x)
      } text("Specify whether substructures allow gaps between vertices.  Optional.  Default = t(rue).")

      opt[String]('f', "float") valueName("[t,f]") action { (x, o) =>
        o.copy(float = x)
      } text("Specify whether substructures can float.  Optional.  Default = t(rue).")

      opt[String]('e', "exact") valueName("[t,f]") action { (x, o) =>
        o.copy(exact = x)
      } text("Specify whether the exact match of labeles is required for mappings.  Optional.  Default = f(alse).")

      opt[String]('w', "weight") valueName("Double") action { (x, o) =>
        o.copy(weight = x)
      } text("Specify the cost for replacement of labels.  Optional.  Default = 1.00.")
    }

    parser.parse(args, MainOption()) map { option =>
      // 引数解析に成功した場合
      pFile = option.parseFile
      jobSize = option.jobSize
      shape = option.shape match{
        case "forest" => "f"
        case "tree" => "t"
        case "path" => "p"
        case "agreement" => "a"
        case "separable" => "s"
        case _ => option.shape
      }
      gap = option.gap match{case "t" => true; case "f" => false}
      float = option.float match{case "t" => true; case "f" => false }
      exact = option.exact match{case "t" => true; case "f" => false }
      weight = if(exact) 1.0 else option.weight.toDouble
      wFile =
        if(option.writeFile != "") option.writeFile
        else pFile+"-"+shape+option.gap+option.float+option.exact+"-"+f2.format(weight)+".dm"
    } getOrElse {
      // 引数解析に失敗した場合
      // sys.exit(1)
      // println(parser.usage)
      return
    }

    println("Parsing " + pFile + " and computing a distance matrix.")

    val tds = new TreeDataset(pFile, weight)

    println("Finished.  Found " + tds.tn + " trees.")

    val matrix = tds.computeDM(shape, gap, float, exact, jobSize) //temp

    println("Writing to " + wFile)

    val out = new PrintWriter(wFile)

    out.write("*** DATASET " + pFile + "\n")
    out.write("*** LABELS\n")
    for(i <- 0 until tds.tn)
      out.write(i + ":" + tds.labels(i) + "\n")

    out.write("*** DISTANCES: SHAPE=" + shape +
      " GAP=" + (if(gap) "t" else "f") +
      " FLOAT=" + (if(float) "t" else "f") +
      " EXACT_MATCH=" + (if(exact) "t" else "f") +
      " WEIGHT=" + f2.format(weight) + "\n")

    for(i <- 0 until tds.tn) {
      out.write((0 until tds.tn).map{j =>
          j + ":" +
          (if(i == j) 0
          else if(i < j) matrix((i, j))
          else matrix((j, i)))
        }.mkString(i + ": ", " ", "\n"))
    }
    out.close()
  }
}

// object Test {
//   def main(argv: Array[String]) {
//     val t1 = "d (e(a, b), c)"
    
//     TreeParser.parse(t1) match {
//       case Some(n: TreeNode) => println(n.toString)
//       case None => println("Something is wrong")
//     }

//     val t2 = "d(a   e (b,c))"
    
//     TreeParser.parse(t2) match {
//       case Some(n: TreeNode) => println(n.toString)
//       case None => println("Something is wrong")
//     }

//     val a = TreeParser.parse(t1).get
//     val b = TreeParser.parse(t2).get


//     println("DEBUG: " + a.parent + ":" + a.label + ":" + a.children.mkString(","))


//     println("DEBUG: " + a.toString + "=>" + a.ref)
//     println("DEBUG: " + a.toString + "=>" + a.ref)
//     println("DEBUG: " + b.toString + "=>" + b.ref)
//     println("DEBUG: " + b.toString + "=>" + b.ref)

//     println("DEBUG: " + a.children.map(_.ref).mkString(","))

//     println
//     println("********************")
//     println

//     val ted = new TED("s", true, true, false)

//     println("Tai = " + ted.mast(a, b))
//     println("Tai = " + ted.mast(a, b))
//     println("Tai = " + ted.mast(b, a))
//     println("Tai = " + ted.mast(b, b))
//   }
// }
