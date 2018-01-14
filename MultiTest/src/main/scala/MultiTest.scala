/**
 * Created by IntelliJ IDEA.
 * User: Firenze
 * Date: 11/05/20
 * Time: 13:56
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable.{ListBuffer, Map, Set}
import java.io.File
import scala.io.Source
import scala.math
import scala.util.matching.Regex
import java.text.DecimalFormat
import org.apache.commons.math3.distribution._
import org.apache.commons.math3.stat.inference._
import java.io.{OutputStreamWriter, FileOutputStream}


object MultiTest {
  def main(args: Array[String]) {
    println("*** Welcome to the beautiful world of multiple comparison!")
    /*
     * The main function performs the multiple tests of
     * 1. Wilcoxon Signed Rank Test, 
     * 2. Friedman Test, and 
     * 3. Hommel Test,
     * reading data from a file.
     *
     * Usage.
     * java -jar MultiFile.jar -f data.csv
     *
     * data.csv 
     */
    var src: Map[String, List[Double]] = Map()
    if(args.length == 0 || args(0) == "-h") {
      println("Usage:  java -jar MultiTest.jar foo.csv")
      return
    } 
    val file_name = args(0)
    // var argIt = args.iterator
    // var file_name: String = null
    // while(argIt.hasNext) {
    //   argIt.next match {
    //     case "-f" =>
    //       file_name = argIt.next
    //   }
    // }
    // if(file_name != null) {
    var lines = try {
      println("*** Reading " + file_name)
      Source.fromFile(file_name).getLines.toList
    } catch {
      case e: Throwable =>
        println("Error: Can't open '" + file_name + "'.")
        return
    }
    var ln: Int = 0
    for(l <- lines) {
      val tpl = l.split(",").toList
      src += tpl(0) -> (tpl.tail.map(_.toDouble))
    }
    // }
    println("***** Have read " + src.size + " lines.")
    for(l <- src) println("***** "+l._1+" has "+ l._2.size +" elements")
    val wfh = new WFH(src)
    wfh.test
    print("*** Rank\n")
    for(e <- wfh.rank_t) {
      print(e._1 + " -> " + e._2.mkString("(",", ",").average = ")
        + wfh.rank_av(e._1) + "\n")
    }
    print("**** Friedman Test\n")
    print("Chi-Square = " + wfh.FT_chi + "\n")
    print("F-Value = " + wfh.FT_f + "\n")
    print("P-Value = " + wfh.FT_p + "\n")
    print("**** Wilcoxon Signed Rank Test\n")
    for(tpl <- wfh.WT_p) {
      print(tpl._1 + "->" + tpl._2 + "\n")
    }
    // print("**** Hommel Test z Values\n")
    // for(tpl <- wfh.HT_z) {
    //   print(tpl._1 + "->" + tpl._2 + "\n")
    // }
    print("**** Scores for Hommel and Shaffer tests\n")
    for(tpl <- wfh.score) {
      print(tpl._1 + "->" + tpl._2 + "\n")
    }
    print("**** Hommel Test p Values\n")
    for(t <- wfh.HT_p) {
      print("CONTROL = " + t._1 + "\n")
      print(t._2.map(x => x._1 + "->" + x._2).mkString("* "," ","\n"))
    }
    print("**** Shaffer Test p Values\n")
    for(t <- wfh.ST_p) {
      print(t._1 + " -> " + t._2 + "\n")
    }
    print("**** Benjamini-Hochberg Test q values\n")
    for(t <- wfh.BH_q) {
      print("CONTROL = " + t._1 + "\n")
      print(t._2.map(x => x._1 + "->" + x._2).mkString("* "," ","\n"))
    }
    /*
    val testset = List((3.71,3,10), (2.59,8,16), (2.38,10,19), (2.416,3,39))
    for(x <- testset) {  
      val fd = new FDistribution(x._2, x._3)
      val p = 1 - fd.cumulativeProbability(x._1)
      print("p("+x._1+","+x._2+","+x._3+") = " + p + "\n")
    } */
  }
}

/*
 * WFH is a class to perform
 * 1. Wilcoxon Signed Rank Test,
 * 2. Friedman Test,
 * 3. Hommel Test.
 * 4. Benjamini-Hochberg FDR test
 *
 * Constructing an instance
 *   val wft = new WFT(src)
 * 
 * Performing test
 *   if(wft.isEffective) wft.test()
 * 
 * Accessing the results
 * The results of the test are stored in public variables of an instance.
 * Examples.
 *   wft.FT_p:  The p value of Friedman Test.
 *   wft.WT_p((i, j)):  The p value of Wilcoxon Signed Rank Test
 *                      for the pair of the algorithm i and j.
 *   wft.HT_p(i)(j):  The p value of Hommel Test for the alg j
 *                    when the alg i is the control.
 *                    The best and worst algorithms are used as a control.
 *   wft.BH_q(i)(j):  The q value of Benjamini-Hochberg Test for the alg. j
 *                    when the alg i is the control.
 */

class WFH(val src: Map[String, List[Double]]) {
  /*
   * src specifies the mapping from the algorithms to investigate
   * to vectors of observed scores when applying the algorithms to datasets.
   *
   * For an element i -> (s_i1, ..., s_il),
   * s_ij is the score observed when aplying the algorithm i to the dataset j.
   */
  val l = src.toList.apply(0)._2.length
  // val l = src.first._2.length
  /*
   *  Number of the datasets to investigate
   */
  val isEffective = (l > 0 && src.dropWhile(_._2.length == l).isEmpty)
  /*
   * Check whether all the vectors have the same dimension.
   */
  val s = src.size
  /*
   Number of the algorithms to investigate
   */
  var rank_t: Map[String, List[Double]] = src.map(_._1 -> List())
  /*
   * For an element i -> (r_i1, ..., r_il),
   * r_ij is the rank of s_ij among (s_1j, ..., s_sj).
   */
  var rank_av: Map[String, Double] = Map()
  /*
   * For an element i -> R_i,
   * R_i is the average across (r_i1, ..., r_il).
   */
  var FT_chi: Double = 0.0
  /*
   * The chi-square value of Friedman Test.
   */
  var FT_f: Double = 0.0 
  /*
   * The F value of Friedman Test.
   */
  var FT_p: Double = 0.0
  /*
   * The p value of Friedman Test.
   */
  var WT_p: Map[(String, String), Double] = Map()
  /*
   * For (i, j) -> p_ij,
   * p_ij is the p value of Wilcoxon Signed Rnak Test
   * for the algorithms i and j.
   */
  var HT_z: Map[(String, String), Double] = Map()
  /*
   * For an entry (i, j) -> z_ij,
   * z_ij = |R_i - R_j| / SE for the algorithms i and j.
   * SE is the standard error.
   */
  var HT_s: Map[(String, String), Double] = Map()
  /*
   * Specifies scores to be used in Hommel Test.
   * 
   * For an entry (i, j) -> s_ij, 
   * s_ij is the two-side p value of z_ij in
   * the normal distribution with mean 0 and standard deviation 1.
   */
  var HT_p: Map[String, Map[String, Double]] = Map()
  /*
   * For an entry i -> (j -> p_ij),
   * p_ij is the p value of Hommel test of the algorithm j
   * when the algorithm i is the control.
   */
  var score: Map[(String, String), Double] = Map()
  /*
   * Scores to be used in Hommel and Shaffer tests
   */
  var ST_p: Map[(String, String), Double] = Map()
  /*
   * For an entry (i, j) -> p_ij,
   * p_ij is the p value of Shaffer test between
   * algorithms i and j.  
   */
  var BH_q: Map[String, Map[String, Double]] = Map()
  /*
   *  For an entry i -> (j -> q_ij)
   *  q_ij is the q value of Benjamini-Hochberg Test
   *  when the algorithm i is the control.
   *  Computed from the p values of Wilcoxon test.
   */

  val nd = new NormalDistribution()
  val fd = new FDistribution(s-1, (s-1)*(l-1))
  val wt = new WilcoxonSignedRankTest()
  /*
   * Instanciate objects of statistic classes of commons-math3.
   */
  
  def test: Int ={
    if(!this.isEffective) return 0

    compute_rank

    compute_average
    
    println("***** Performing Friedman Test")

    do_Friedman_test

    println("***** Performing Wilcoxon Test")

    do_Wilcoxon_test

    println("***** Performing Hommel Test")

    do_Hommel_test

    println("***** Performing Shaffer Test")

    do_Shaffer_test

    println("***** Performing Benjamini-Hochberg Test")

    do_Benjamini_Hochberg_test

    return 1
  }

  
  def compute_rank = {
    var d = src.toList
    for(i <- (0 to l-1).reverse) { 
      d = d.sortBy(_._2(i)).reverse
      var b = 0
      var t = 0
      var rb = 1.0
      var rt = 0.0
      while(b < s) {
	while(t < s && d(b)._2(i) == d(t)._2(i)) {
	  rt += 1
	  t += 1
	}
	while(b < s && b < t) {
	  rank_t(d(b)._1) = ((rb + rt)/2)::rank_t(d(b)._1)
	  b += 1
	}
	rb = rt + 1
      }
    }
  }

  def compute_average = {
    rank_av = rank_t.map(x => x._1 -> x._2.reduceLeft(_ + _)/l)
  }
  
  def do_Friedman_test = {
    val ss = rank_av.foldLeft(0.0)((x,y) => x + math.pow(y._2,2.0))
    FT_chi = (ss - s*math.pow(s+1,2.0)/4.0) * 12.0 * l / s / (s+1)
    FT_f = (l-1)*FT_chi/(l*(s-1)-FT_chi)
    FT_p = 1 - fd.cumulativeProbability(FT_f)
  }

  def do_Wilcoxon_test = {
    var x = src.iterator
    while(x.hasNext) {
      val xx = x.next
      val y = src.iterator.dropWhile(_._1 != xx._1)
      y.next
      while(y.hasNext) {
	val yy = y.next
	do_Wilcoxon_test_for(xx, yy)
	calculate_scores_for(xx, yy)
      }
    }
  }

  def do_Wilcoxon_test_for(xx: (String, List[Double]),
			   yy: (String, List[Double])) = {
    val p =
      if(xx._2.length > 30) {
        wt.wilcoxonSignedRankTest(xx._2.toArray, yy._2.toArray, false)
      }else{
        wt.wilcoxonSignedRankTest(xx._2.toArray, yy._2.toArray, true)
      }
    WT_p += (xx._1, yy._1) -> p
    WT_p += (yy._1, xx._1) -> p
  }


  def calculate_scores_for(
			   xx: (String, List[Double]),
			   yy: (String, List[Double])) = {
	val z = (rank_av(xx._1)-rank_av(yy._1)).abs/math.sqrt(s*(s+1)/6.0/l)
	val q = 2 * (1 - nd.cumulativeProbability(z))
	score += (xx._1, yy._1) -> q
  }

  def do_Benjamini_Hochberg_test = {
    val ctrls: List[String] = src.map(_._1).toList
    for(ctrl <- ctrls) {
      BH_q += ctrl -> Map()
      val sorted = WT_p.toList.filter(_._1._1 == ctrl).
        map(x => (x._1._2, x._2)).sortWith(_._2 > _._2)
      var cur_min = 1.0 // Current minimum q-value
      var m = sorted.length // Number of null hypotheses
      for(i <- 0 until m) {
        cur_min = math.min(sorted(i)._2 * m / (m - i), cur_min)
        BH_q(ctrl) += sorted(i)._1 -> cur_min
      }
    }
  }

  def do_Hommel_test = {
    val n = s-1
    var ctrls: List[String] = src.map(_._1).toList
    // ctrls ::= rank_av.minBy(_._2)._1
    // ctrls ::= rank_av.maxBy(_._2)._1
    for(ctrl <- ctrls) {
      HT_p += ctrl -> Map()
      var ss = score.filter(x => x._1._1 == ctrl || x._1._2 == ctrl).toList
      ss = ss.sortBy(_._2)
      val qq: Map[(Int, Int), Double] = Map()
      for(i <- 1 to n) {
	for(j <- (n-i+1) to n) {
	  qq += (i, j) -> (j * ss(i - 1)._2 / (i + j - n))
	}
      }
      val q : Map[Int, Double] = Map()
      for(j <- 1 to n) {
	q += j -> qq.filter(_._1._2 == j).map(_._2).toList.min
      }
      for(tpl <- ss) { 
	var j: Int = 1
	while(j != n+1 && j * tpl._2 <= q(j)) j += 1
        j -= 1
        val trgt = if(tpl._1._1 == ctrl) tpl._1._2 else tpl._1._1
        HT_p(ctrl) += trgt -> j * tpl._2
      }
    }
  }

  def do_Shaffer_test = {
    val n = s*(s-1)/2

    var S: Map[Int, Set[Int]] = Map()
    for(i <- 0 to s) {
      if(i == 0 || i == 1) {
	S += i -> Set(0)
      } else {
	var temp: Set[Int] = Set()
	for(j <- 1 to i; x <- S(i - j)) temp += j*(j-1)/2 + x
	S += i -> temp
      }
    }
    
    val sorted_score = score.toList.sortBy(_._2)
    var current_max = 0.0
    for(i <- 0 to sorted_score.size - 1) { 
      current_max = current_max.max(
	 (S(s).filter(_ + i <= n).max) * sorted_score(i)._2
      )
      val pval = 1.00 min current_max
      ST_p += sorted_score(i)._1 -> pval
      ST_p += (sorted_score(i)._1._2, sorted_score(i)._1._1) -> pval
    }
  }
}
