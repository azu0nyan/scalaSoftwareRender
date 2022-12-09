case class Matrix4x4(
                    m:IndexedSeq[IndexedSeq[Double]]
                    ) {

  def *(ot:Matrix4x4): Matrix4x4 = {
    var res = Array.ofDim[Double](4, 4)
    for(i <- 0 until 4;
        j <- 0 until 4;
        k <- 0 until 4){
        res(i)(j) += m(i)(k) * ot.m(k)(j)
    }
    Matrix4x4(res.map(_.toIndexedSeq).toIndexedSeq)
  }

  def *(v:V4):V4 = {
    var ot = Array.ofDim[Double](4)
    for(i <- 0 until 4;
        j <- 0 until 4){
      ot(i) += m(i)( j) * v(j)
    }
    V4(ot(0), ot(1), ot(2), ot(3))
  }

  def apply(x:Int) :Double =
    m(x / 4)(x % 4)

}
