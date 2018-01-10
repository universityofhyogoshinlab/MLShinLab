import libsvm._
import scala.io.Source
import scala.util.matching.Regex
import java.io.PrintWriter

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

object Main {

  var labels: Array[Double] = null
  var samples: Array[(Int, Array[(Int, BP)])] = null
  var diag: Map[Int, Double] = null

  val sc = new SparkContext("local[*]", "gram_matrix_optimizer")

  def readFormula(path: String): Boolean = {
    /*
     Read a .kernel file whose path is specified by 'path' and 
     fill 'labels' and 'samples' with contents.
     In .kernel files, data are indexed by contiguous integers starting from 0, 
     while libSVM requires that the initial index be 1.
     */

    val lines = sc.textFile(path)

    labels = lines.map{line =>
      val labelP = new Regex("""(\d+):(\S.*)""")
      line match {
        case labelP(i,l) => (i.toInt, l.toDouble)
        case _ => (-1, 0.0)
      }
    }.filter(_._1 >= 0).collect().sortWith(_._1 < _._1).map(_._2).toArray

    samples = lines.map{line =>
      val sampleP = new Regex("""(\d+):\s+(.+)""")
      line match {
        case sampleP(i, s) =>
          (i.toInt+1, s.split(" ").map(_.split(":")).map(array =>
            (array(0).toInt+1, new BP().make(array(1)))))
        case _ => (0, Array[(Int, BP)]())
      }
    }.filter(each => each._1 > 0).collect().toArray

    true
  }
  
  def substitute(x: Double, y: Double, norm: Boolean): Array[Array[libsvm.svm_node]] = {
    /*
     Substitute the values of 'x' and 'y' for the variables x and y in the formulas in 'samples'.
     If 'norm' is true, the kernel values obtained by the substitutions are normalized.
     */
    val temp = samples.map{tpl =>
      val i = tpl._1
      val start_node = new libsvm.svm_node
      start_node.index = 0
      start_node.value = tpl._1
      val null_node = new libsvm.svm_node
      null_node.index = -1
      start_node +: tpl._2.map{each =>
        val node = new libsvm.svm_node
        node.index = each._1
        node.value = each._2.substitute(x, y)
        node
      } :+ null_node
    }
    if(norm) {
      diag = temp.map{array =>
        val i = array(0).value.toInt
        (i, array.find(_.index == i).get.value)
      }.toMap
      temp.map{array =>
        val f = diag(array(0).value.toInt)
        array.map{node =>
          if(node.index > 0) node.value = node.value/math.sqrt(f * diag(node.index))
          node
        }
      }
    } else {
      temp
    }
  }

  def div(r: (Double, Double, Int), i: Int): Double = r._1*(1.0-i.toDouble/r._3)+r._2*i.toDouble/r._3

  var kernelFile = "./in.kernel"
  var logFile = "./log.txt"
  var resultFile = "./out.txt"
  var range_a = (0.0, 1.0, 4) // For alpha: (minimum, maximum, # of division)
  var range_b = (0.0, 1.0, 4) // For beta: (minimum, maximum, # of division)
  var range_c = (-3.0, 3.0, 4) // For log C: (minimum, maximum, # of division)
  var cv = 5 // # of folds in cross validation
  var norm = false // If true, the kernel values are normalized.

  def readConfig(configFile: String) {
    val attributeP = new Regex("""\s*([^:\s]+)\s*:\s*(.*)""")
    val lines = Source.fromFile(configFile).getLines
    for(l <- lines) {
      l match {
        case attributeP(attr, value) =>
          attr.toLowerCase match {
            case "kernel" => kernelFile = value
            case "log" => logFile = value
            case "result" => resultFile = value
            case "alpha_min" => range_a = (value.toDouble, range_a._2, range_a._3)
            case "alpha_max" => range_a = (range_a._1, value.toDouble, range_a._3)
            case "alpha_div" => range_a = (range_a._1, range_a._2, value.toInt)
            case "beta_min" => range_b = (value.toDouble, range_b._2, range_b._3)
            case "beta_max" => range_b = (range_b._1, value.toDouble, range_b._3)
            case "beta_div" => range_b = (range_b._1, range_b._2, value.toInt)
            case "logc_min" => range_c = (value.toDouble, range_c._2, range_c._3)
            case "logc_max" => range_c = (range_c._1, value.toDouble, range_c._3)
            case "logc_div" => range_c = (range_c._1, range_c._2, value.toInt)
            case "cv" => cv = value.toInt
            case "norm" => norm = true
            case _ => // Do nothing
          }
        case _ => //Do nothing
      }
    }
  }


  
  def main(args: Array[String]) {

    readConfig(args(0))

    println(".kernel file = " + kernelFile)
    println("Log file" + logFile)
    println("Result file" + resultFile)
    println("Alpha (min, max, div) =  (" + range_a._1 +","+ range_a._2 +","+ range_a._3 +")")
    println("Beta (min, max, div) = (" + range_b._1 +","+ range_b._2 +","+ range_b._3 +")")
    println("log C (min, max, div) = (" + range_c._1 +","+ range_c._2 +","+ range_c._3 +")")
    println("Folds in Cross Validation = " + cv)
    if(norm) println("Kernel values will be normalized.")

    readFormula(kernelFile) // Read the specified .kernel file.

    val test = (for(a <- 1 to range_a._3;
      b <- 0 to range_b._3 if div(range_a, a) >= div(range_b, b);
      c <- 0 to range_c._3) yield (div(range_a, a), div(range_b, b), div(range_c, c))).toArray.distinct

    svm.svm_set_print_string_function(new libsvm.svm_print_interface() {
      override def print(s: String) {
        if (s != ".")
          System.out.print(s);
          System.out.flush();
      }
    })

    val scores: Array[((Double, Double, Double), Int, Int, Int, Int)] =
      sc.parallelize(test).map{case (a:Double, b:Double, c:Double) =>
        var param = new svm_parameter()
        param.svm_type=svm_parameter.C_SVC
        param.kernel_type=svm_parameter.PRECOMPUTED
        param.C=math.pow(10,c)
        var prob = new svm_problem()
        prob.x = substitute(a,b,norm)
        prob.y = labels
        prob.l = labels.size

        var target = Array.fill(labels.size)(0.0)
        svm.svm_cross_validation(prob, param, cv, target)
        var tp = 0
        var fp = 0
        var fn = 0
        var tn = 0
        labels.zip(target).foreach{pair =>
          pair match {
            case (1, 1) => tp += 1
            case (-1, 1) => fp += 1
            case (1, -1) => fn += 1
            case (-1, -1) => tn += 1
          }
        }
        ((a,b,c), tp, fp, fn, tn)
      }.collect()

    sc.stop()

    var out = new PrintWriter(logFile)
    out.write("alpha, beta, log C, tp, fp, fn, tn\n")
    out.write(scores.map{
      case ((a: Double, b:Double, c:Double), tp:Int, fp:Int, fn:Int, tn:Int) =>
        a + "," + b + "," + c + "," + tp + "," + fp + "," + fn + "," + tn}.mkString("\n")
    )
    out.close

    val minError = scores.map(tpl => tpl._3 + tpl._4).min
    val optParams = scores.filter(tpl => tpl._3 + tpl._4 == minError)
    val ((a: Double, b: Double, c: Double), tp: Int, fp: Int, fn: Int, tn: Int) = optParams(0)

    println("* System has chosen an optimal set of parameters as follows:")
    println("*** Optimal X = " + a)
    println("*** Optimal Y = " + b)
    println("*** Optimal log C = " + c)
    println("* Confuson Matrix:")
    println("*** True Positive = " + tp)
    println("*** False Positive = " + fp)
    println("*** False Negative = " + fn)
    println("*** True Negative = " + tn)
    println("* Accuracy = " + (tp.toDouble + tn)/(tp + fp + fn + tn))

    out = new PrintWriter(resultFile)
    out.write("*** Optimal X = " + a +"\n")
    out.write("*** Optimal Y = " + b +"\n")
    out.write("*** Optimal log C = " + c +"\n")
    out.write("*** Normalize = " + (if(norm) "t" else "f") +"\n")
    out.write("*** True Positive = " + tp +"\n")
    out.write("*** False Positive = " + fp +"\n")
    out.write("*** False Negative = " + fn +"\n")
    out.write("*** True Negative = " + tn +"\n")
    out.write("*** Problem definition: \n")
    out.write("*** Labels: \n")
    out.write(labels.mkString("",",","\n"))
    out.write("*** Samples: \n")
    out.write(
      substitute(a,b,norm).map(_.map(n => n.index + ":" + n.value).mkString(" ")).mkString("\n"))
    out.write("\n")
    if(norm) {
      out.write("*** Diagonal Values: \n")
      out.write(diag.toArray.sortWith(_._1 < _._1).map(_._2).mkString(","))
    }
    out.close
  }
}
