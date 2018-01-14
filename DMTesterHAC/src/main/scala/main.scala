import smile._
import smile.math.Math._
import smile.clustering._
import smile.plot._
import smile.validation._

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.util.matching.Regex
import scala.util.Random
import scala.collection.JavaConversions._

import java.io.{File, OutputStreamWriter, FileOutputStream}
import java.text.DecimalFormat
import java.util.Date

import scopt.OptionParser


case class MainOption(
  configFileName: String = "",
  resultFileName: String = ""
)


object Main {

  val d2 = new DecimalFormat("#.##")

  def main(args: Array[String]) {
    var configFileName = ""
    var resultFileName = ""

    // 距離を定義する.dmファイル名のリスト
    // 各.dmファイルは一つの距離（ターゲット）を含む
    var fileList: Array[String] = Array()

    // ファイル名->ターゲット名
    var targetList: Map[String, String] = Map()

    // データセットの名称
    var dataset: String = ""
    // クラスの列挙
    val classes: ArrayBuffer[String] = ArrayBuffer()
    // クラスの数
    var classN: Int = 2
    // クラスのエントロピー
    var classH: Double = 0.0
    // クラス毎のインスタンスの列挙 ArrayからArrayBufferに変更
    val instances: Map[String, ArrayBuffer[Int]] = Map()
    // 最大クラスタ数、k = 2..k_max
    var k_max: Int = 2
    // HACのリンクメソッド
    var link: String = "complete"

    // 距離を格納するMapのインスタンス
    // var d_db: Map[(Int, Int), Double] = Map()
    // インスタンスのクラスの列挙
    var cls: Array[Int] = Array()

    var ariForTex = ArrayBuffer[Double]()
    var nmiForTex = ArrayBuffer[Double]()
    var ariForPy = ArrayBuffer[Double]()
    var nmiForPy = ArrayBuffer[Double]()

    val parser = new OptionParser[MainOption]("Distance-based analysis.") {
      opt[String]('c', "config") required() valueName("<path>") action {
        (x, o) => o.copy(configFileName = x)
      } text("Read experiment configuration from this file. Required.")

      opt[String]('w', "write") valueName("<path>") action {
        (x, o) => o.copy(resultFileName = x)
      } text("Write results of analysis to this file.")
    }

    parser.parse(args, MainOption()) map { option =>
      // 引数解析に成功した場合
      configFileName = option.configFileName
      resultFileName = option.resultFileName
    } getOrElse {
      // 引数解析に失敗した場合
      println(parser.usage)
      return
    }

    if(configFileName == "") {
      println("No configuration file specified.")
      println("Abort the process.")
      println(parser.usage)
      return
    }

    readConfigFile

    if(fileList.length == 0) {
      println("No file is specified.\n The process aborted.")
      return
    }
    println("Read from " + fileList.mkString(","))

    if(resultFileName == "") {
      println("No output file name specified.")
      resultFileName = ("%tF-%<tH:%<tM:%<tS" format new Date) + ".result"
    }
    println("The resulsts will be output to " + resultFileName +".")

    lazy val out = new OutputStreamWriter(new FileOutputStream(resultFileName), "utf-8")

    // dataset, classes, instances, clsの値を設定する
    if(!extractInfo(fileList(0))) {
      println("Process aborted.")
      return
    }

    println("Dataset is: " + dataset)
    println("Class labels are: " + classes.mkString(","))
    println("Numbers of instances per class label: ")
    println(instances.map(x => x._1 + " => " + x._2.size).mkString("\n"))

    if(!checkFiles(fileList)) {
      println("Process aborted.")
      return
    }

    println("Targets are: " + targetList.
      map(x => x._2 + " (" + x._1 + ")").mkString(",")
    )

    printlnBoth("%DATASET = " + dataset + "%")

    for(f_name <- fileList) {
      printlnBoth("%TARGET = " + targetList(f_name) + "%")

      val clusterTree = hclust(readData(f_name), link)
      val ariArray = ArrayBuffer[Double]()
      val nmiArray = ArrayBuffer[Double]()

      for(k <- 2 to k_max) {
        printlnBoth("k = " + k + "***")
        val clusters = clusterTree.partition(k)
        printlnBoth("Clustering = " + clusters.mkString(","))

        // Adjusted Rand Indexを計算する
        val ari = adjustedRandIndex(cls, clusters)
        ariArray += ari
        printlnBoth("Ajusted Rand Index = " + ari)

        // Normalized Mutual Informationを計算する
        val nmi = NMI(clusters)
        nmiArray += nmi
        printlnBoth("Normalized Mutual Info = " + nmi)
      }
      printlnBoth("%MEASURE ARI = " + ariArray.max + "%")
      printlnBoth("%MEASURE NMI = " + nmiArray.max + "%")

      ariForTex ++= ariArray
      nmiForTex ++= nmiArray

      ariForPy ++= ariArray
      nmiForPy ++= nmiArray
    }

    printlnBoth("\n*****************")
    printlnBoth("TeX Program ")
    printlnBoth("*****************")
    printlnBoth("\n---- Cut here ----\n")

    printlnBoth("""\begin{table}""")
    printBoth("""\caption{Adjusted Rand Index by HAC with the """)
    printlnBoth(link + """ link}""")
    printlnBoth(List.fill(k_max-1){"l"}.mkString("""\begin{tabular}{l|""","","}"))
    printlnBoth("""\toprule""")
    printlnBoth((2 to k_max).mkString("Target & "," & ","""\\"""))
    printlnBoth("""\midrule""")
    for(f <- fileList) {
      printBoth(targetList(f))
      printlnBoth(ariForTex.take(k_max-1).map(x => d2.format(x)).mkString(" & "," & ", """\\"""))
      ariForTex = ariForTex.drop(k_max-1)
    }
    printlnBoth("""\bottomrule""")
    printlnBoth("""\end{tabular}""")
    printlnBoth("""\end{table}""")

    printlnBoth("""\begin{table}""")
    printBoth("""\caption{Normalized Mutual Information by HAC with the """)
    printlnBoth(link + """ link}""")
    printlnBoth(List.fill(k_max-1){"l"}.mkString("""\begin{tabular}{l|""","","}"))
    printlnBoth("""\toprule""")
    printlnBoth((2 to k_max).mkString("Target & "," & ","""\\"""))
    printlnBoth("""\midrule""")
    for(f <- fileList) {
      printBoth(targetList(f))
      printlnBoth(nmiForTex.take(k_max-1).map(x => d2.format(x)).mkString(" & "," & ", """\\"""))
      nmiForTex = nmiForTex.drop(k_max-1)
    }
    printlnBoth("""\bottomrule""")
    printlnBoth("""\end{tabular}""")
    printlnBoth("""\end{table}""")

    printlnBoth("\n*****************")
    printlnBoth("Python Program ")
    printlnBoth("*****************")
    printlnBoth("\n---- Cut here ----\n")
    printlnBoth("# HAC with " + link + " link.")

    printlnBoth("import numpy as np")
    printlnBoth("import matplotlib.pyplot as plt")
    printlnBoth("k = np.arange(2,"+(k_max+1)+",1)")
    printlnBoth("%pylab inline --no-import-all")

    printlnBoth("fig=plt.figure()")

    printlnBoth("ariAx=fig.add_subplot(121)")
    printlnBoth("ariAx.set_ylim(0.0,1.0)")
    printlnBoth("ariAx.set_yticks(np.arange(0.0,1.0,0.05))")
    printlnBoth("ariAx.set_title(\"Adjusted Rand Index\")")
    printlnBoth("ariAx.grid()")

    printlnBoth("nmiAx=fig.add_subplot(122,sharey=ariAx)")
    printlnBoth("nmiAx.set_ylim(0.0,1.0)")
    printlnBoth("nmiAx.set_yticks(np.arange(0.0,1.0,0.05))")
    printlnBoth("nmiAx.set_title(\"Normalized Mutual Information\")")
    printlnBoth("nmiAx.grid()")

    for(f <- fileList) {
      val t = targetList(f)
      printlnBoth("ari_"+t+"=["+ariForPy.take(k_max-1).mkString(",")+"]")
      printlnBoth("ariAx.plot(k,ari_"+t+",label=\'"+t+"\')")
      ariForPy = ariForPy.drop(k_max-1)
    }

    for(f <- fileList) {
      val t = targetList(f)
      printlnBoth("nmi_"+t+"=["+nmiForPy.take(k_max-1).mkString(",")+"]")
      printlnBoth("nmiAx.plot(k,nmi_"+t+",label=\'"+t+"\')")
      nmiForPy = nmiForPy.drop(k_max-1)
    }

    printlnBoth("plt.legend(bbox_to_anchor=(1.05,1),loc=\'upper left\')")
    printlnBoth("plt.tight_layout()")

    out.close()


    def readConfigFile {
      /*
       * 設定ファイルの例
       * Files% leukemia-tai.dm, leukemia-lu.dm leukemia-2deg.dm
       *  K_MAX% 10
       */
      val fileP = """FILES%\s*(.*)""".r
      val kP = """K_MAX%\s*(\d+).*""".r
      val lP = """LINK%\s*([a-zA-Z]+).*""".r

      for(l <- Source.fromFile(configFileName).getLines) {
        l.toUpperCase match {
          case fileP(x) =>
            fileList ++= x.split("""[,\s]""")
          case kP(x) =>
            k_max = x.toInt
          case lP(x) =>
            link = x.toUpperCase match {
              case "SINGLE" => "single"
              case "UPGMA" => "upgma"
              case "WPGMA" => "wpgma"
              case "UPGMC" => "upgmc"
              case "WPGMC" => "wpgmc"
              case "WARD" => "ward"
              case _ => "complete"
            }
            println("### DEBUG ###" + link)
          case _ => // Just ignore.
        }
      }
    }

    def extractInfo(file_name: String): Boolean = {
      val datasetP = """\*\*\* DATASET\s+(.*)""".r
      val lineP = """(\d+):(.+)""".r

      val temp = ArrayBuffer[(Int, String)]()

      val lines = Source.fromFile(file_name).getLines.takeWhile(
        l => !l.startsWith("*** DISTANCES"))
      for(line <- lines) {
        line match {
          case datasetP(x) =>
            dataset = x
          case lineP(idSymb, label) =>
              val id = idSymb.toInt
              temp += Pair(id, label)
              if(!instances.isDefinedAt(label)) {
                instances(label) = ArrayBuffer[Int]()
                classes += label
              }
              instances(label) += id
          case _ => //Do nothing.
        }
      }
      if(dataset.isEmpty) {
        println("ERROR: No dataset is specified.")
        return false
      }
      if(classes.size < 2) {
        println("ERROR: Too few classes")
        return false
      }
      classN = classes.size
      val cls2int = classes.zipWithIndex.toMap
      cls = temp.sortWith(_._1 < _._1).map(x => cls2int(x._2)).toArray
      classH = entropy(cls)
      return true
    }

    def checkFiles(fileList: Array[String]): Boolean = {
      val datasetP = """\*\*\* DATASET\s+(.*)""".r
      val distanceP = """\*\*\* DISTANCES:\s*(.*)""".r

      for(fn <- fileList) {
        println("Checking " + fn + ".")

        var foundDataset = false
        var foundTarget = false
        var li = Source.fromFile(fn).getLines

        while(li.hasNext && !(foundDataset && foundTarget)) {
          val line = li.next
          line match {
            case datasetP(x) =>
              if(dataset != x) {
                println("ERROR: Inconsistent dataset name in " + fn)
                return false
              }
              foundDataset = true
            case distanceP(x) =>
              val m = x.split("""\s+""").map{x =>
                val y = x.split("=")
                (y(0), y(1))}.toMap
              val target = m("SHAPE")+m("GAP")+m("FLOAT")+m("EXACT_MATCH")+
                (if(m("EXACT_MATCH")=="f")
                  (m("WEIGHT").toDouble*100).toInt.toString else "")
              if(targetList.values.contains(target)) {
                println("ERROR: Duplicated target in " + fn)
                return false
              }
              targetList += fn -> target
              foundTarget = true
            case _ => // Just ignore.
          }
        }
        if(!foundDataset) {
          println("ERROR: No dataset specified in " + fn)
          return false
        }
        if(!foundTarget) {
          println("ERROR: No target specified in " + fn)
          return false
        }
      }
      return true
    }

    def readData(file_name: String): Array[Array[Double]] = {
      val lines = Source.fromFile(file_name).getLines
      val lineP = """(\d+):\s+(.+)""".r
      val matrix = ArrayBuffer[Array[Double]]()

      for(line <- lines) {
        line match {
          case lineP(idSymb, values) =>
            val id = idSymb.toInt
            matrix += values.split("""\s+""").map(_.split(":")).map{
              x => (x(0).toInt, x(1).toDouble)}.
              sortWith(_._1 < _._1).take(id+1).map(_._2)
          case _ => //Do nothing.
        }
      }
      return matrix.sortWith(_.size < _.size).toArray
    }

    // 標準出力と出力ファイルに同時に出力（改行あり）
    def printlnBoth(s: String) {
      println(s)
      out.write(s + "\n")
    }

    // 標準出力と出力ファイルに同時に出力（改行なし）
    def printBoth(s: String) {
      print(s)
      out.write(s)
    }

    def entropy(x: Array[Int]): Double = {
      val n = x.size.toDouble
      val c = collection.mutable.Map[Int, Int]()
      for(y <- x) {
        if(c.isDefinedAt(y)) {
          c(y) += 1
        } else {
          c(y) = 1
        }
      }
      c.map(_._2).foldLeft(0.0){(h, i) => h - i/n * scala.math.log(i/n)}
    }

    def NMI(x: Array[Int]): Double = {
      val h = entropy(x.zip(cls).map{case (y: Int, z: Int) => y*classN+z})
      2 - 2 * h / (classH + entropy(x))
    }
  }
}
