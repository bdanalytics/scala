package kmeans

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point,GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  def checkParUpdate(classified: GenMap[Point, GenSeq[Point]],
                     means: GenSeq[Point],
                     expected: GenSeq[Point]) {
//    update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point]
    assert(update(classified.par, means.par).toVector === expected,
      s"update($classified par, $means par) should equal to $expected")
  }

//  test("'update' should work for 'classified' points with one Point as key and same Point as value and 'means' with the same Point") {
//
//    val p1 = new Point(1, 1, 0)
////    val p2 = new Point(1, -1, 0)
////    val p3 = new Point(-1, 1, 0)
////    val p4 = new Point(-1, -1, 0)
////    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
//    val points: GenSeq[Point] = IndexedSeq(p1)
////    val mean1 = new Point(1, 0, 0)
//    val mean1 = new Point(1, 1, 0)
////    val mean2 = new Point(-1, 0, 0)
////    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
//    val means: GenSeq[Point] = IndexedSeq(mean1)
//    val classified = classify(points.par, means.par)
//    val expected: GenSeq[Point] = IndexedSeq(mean1)
//
//    checkParUpdate(classified, means, expected)
//  }

  def checkParConverged(eta: Double,
                        oldMeans: GenSeq[Point],
                        newMeans: GenSeq[Point]) {
//    converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean
    assert(converged(eta)(oldMeans.par, newMeans.par) === true,
      s"converged($eta, $oldMeans par, $newMeans par) should equal to true")
  }

//    test("'converged' should work for 'oldMeans' == GenSeq((1,1,1), ..., (99,99,99)) and 'newMeans' == GenSeq((1,1,1), ..., (99, 99, 99.01))") {
//
//      val p1 = new Point(1, 1, 0)
//  //    val p2 = new Point(1, -1, 0)
//  //    val p3 = new Point(-1, 1, 0)
//  //    val p4 = new Point(-1, -1, 0)
//  //    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
//      val points: GenSeq[Point] = IndexedSeq(p1)
//  //    val mean1 = new Point(1, 0, 0)
//      val mean1 = new Point(1, 1, 0)
//  //    val mean2 = new Point(-1, 0, 0)
//  //    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
//      val means: GenSeq[Point] = IndexedSeq(mean1)
//      val classified = classify(points.par, means.par)
//      val expected: GenSeq[Point] = IndexedSeq(mean1)
//
//      checkParUpdate(classified, means, expected)
//    }

  def checkParKMeans(eta: Double,
                     points: GenSeq[Point],
                     means: GenSeq[Point],
                     expected: GenSeq[Point]) {
//    kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point]
    assert(kMeans(points, means.par, eta) === expected,
      s"kMeans($points par, $means par, $eta) should equal to true")
  }

//      test("'kMeans' should work for 'points' == GenSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0)) and 'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12.25") {
//
//        val p1 = new Point(0, 0, 1)
//        val p2 = new Point(0, 0, -1)
//        val p3 = new Point(0, 1, 0)
//        val p4 = new Point(0, 10, 0)
//        val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
//        val mean1 = new Point(0, -1, 0)
//        val mean2 = new Point(0, 2, 0)
//        val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
////        val classified = classify(points.par, means.par)
//        val expected: GenSeq[Point] = IndexedSeq(new Point(0.0, 0.0, 0.0), new Point(0.0, 5.5, 0.0))
//        val eta : Double = 12.25
//
//        checkParKMeans(eta, points, means, expected)
//      }

}


  
