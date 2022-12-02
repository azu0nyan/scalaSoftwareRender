import java.io.File
import java.util.Scanner
import scala.collection.mutable

object Obj {
  def apply(file: String): ObjData = {
    val s = new Scanner(new File(file))

    val v = mutable.Buffer[V3]()
    val vt = mutable.Buffer[V2]()
    val vn = mutable.Buffer[V3]()
    val f = mutable.Buffer[((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]()

    while (s.hasNextLine) {
      val l = s.nextLine()
      if (l.startsWith("#")) {
        // skip
      } else if (l.startsWith("v ")) {
        val splitted = l.split(" ")
        v += V3(splitted(1).toDouble, splitted(2).toDouble, splitted(3).toDouble)
      } else if (l.startsWith("f")) {
        val splitted = l.split(" ") // f 6/4/1 3/5/3 7/6/5
        val s1 = splitted(1).split("/")
        val s2 = splitted(2).split("/")
        val s3 = splitted(3).split("/")
        f += ((
          (s1(0).toInt - 1, s1(1).toInt - 1, s1(2).toInt - 1),
          (s2(0).toInt - 1, s2(1).toInt - 1, s2(2).toInt - 1),
          (s3(0).toInt - 1, s3(1).toInt - 1, s3(2).toInt - 1),
          ))
      } else if (l.startsWith("vt")) {
        val splitted = l.split(" ")
        vt += V2(splitted(1).toDouble, splitted(2).toDouble)
      } else if (l.startsWith("vn")) {
        val splitted = l.split(" ")
        vn += V3(splitted(1).toDouble, splitted(2).toDouble, splitted(3).toDouble)
      }
    }

    ObjData(v.toIndexedSeq, vt.toIndexedSeq, vn.toIndexedSeq, f.toIndexedSeq)
  }

}

case class ObjData(
                v: IndexedSeq[V3],
                vt: IndexedSeq[V2],
                vn: IndexedSeq[V3],
                f: IndexedSeq[((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]
              )

