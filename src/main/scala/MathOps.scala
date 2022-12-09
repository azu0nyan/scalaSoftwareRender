object MathOps {
  def toBaeycentric(a: V2, b: V2, c: V2, p: V2): V3 = {
    val abx = b.x - a.x
    val acx = c.x - a.x
    val pax = a.x - p.x
    val aby = b.y - a.y
    val acy = c.y - a.y
    val pay = a.y - p.y

    val res = V3(abx, acx, pax) x V3(aby, acy, pay)

    V3(1 - res.x / res.z - res.y / res.z,
      res.x / res.z,
      res.y / res.z)

  }

  def clamp(min: Double, max: Double, p: Double): Double = {
    if (min <= p && p <= max) p
    else if (p < min) min
    else max
  }

  def id: Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 1, 0, 0),
      IndexedSeq(0, 0, 1, 0),
      IndexedSeq(0, 0, 0, 1),
    )
  )

  def projection(dist: Double): Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 1, 0, 0),
      IndexedSeq(0, 0, 1, 0),
      IndexedSeq(0, 0, -1 / dist, 1),
    )
  )

  def translation(trans: V3): Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(1, 0, 0, trans.x),
      IndexedSeq(0, 1, 0, trans.y),
      IndexedSeq(0, 0, 1, trans.z),
      IndexedSeq(0, 0, 0, 1),
    )
  )

  def scale(scale: V3): Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(scale.x, 0, 0, 0),
      IndexedSeq(0, scale.y, 0, 0),
      IndexedSeq(0, 0, scale.z, 0),
      IndexedSeq(0, 0, 0, 1),
    )
  )

  def rotateX(a: Double): Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, math.cos(a), -math.sin(a), 0),
      IndexedSeq(0, math.sin(a), math.cos(a), 0),
      IndexedSeq(0, 0, 0, 1),
    )
  )

  def rotateY(a: Double): Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(math.cos(a), 0, -math.sin(a), 0),
      IndexedSeq(0, 1, 0, 0),
      IndexedSeq(math.sin(a), 0, math.cos(a), 0),
      IndexedSeq(0, 0, 0, 1),
    )
  )

  def rotateZ(a: Double): Matrix4x4 = Matrix4x4(
    IndexedSeq(
      IndexedSeq(math.cos(a), -math.sin(a), 0, 0),
      IndexedSeq(math.sin(a), math.cos(a), 0, 0),
      IndexedSeq(0, 0, 1, 0),
      IndexedSeq(0, 0, 0, 1),
    )
  )

  def rotateXYZ(rot:V3): Matrix4x4 =
    rotateZ(rot.z) * rotateY(rot.y) * rotateX(rot.x)


  def lookat(eye: V3, center: V3, up: V3):Matrix4x4  = {
    val z = (eye - center).norm
    val xv = (up x z).norm
    val y = (z x xv).norm

    val minv = Array.ofDim[Double](4, 4)
    val tr = Array.ofDim[Double](4, 4)
    for(i<- 0 until 4) {
      minv(i)(i) = 1
      tr(i)(i) = 1
    } 
    for (i <- 0 until 3 )
    {
      minv(0)(i) = xv(i)
      minv(1)(i) = y(i)
      minv(2)(i) = z(i)
      tr(i)(3) = -eye(i)
    }
     Matrix4x4(minv.map(_.toIndexedSeq).toIndexedSeq) *
       Matrix4x4(tr.map(_.toIndexedSeq).toIndexedSeq);
  }

  def viewport (x: Int,  y: Int,  w:Int,  h:Int, depth: Double):Matrix4x4 =  {
    val m = Array.ofDim[Double](4, 4)
    for (i <- 0 until 4) {
      m(i)(i) = 1
    }
    m(0)(3) = x + w / 2;
    m(1)(3) = y + h / 2;
    m(2)(3) = depth / 2;

    m(0)(0) = w / 2;
    m(1)(1) = h / 2;
    m(2)(2) = depth / 2;
    Matrix4x4(m.map(_.toIndexedSeq).toIndexedSeq)
  }

  def inverse(m: Matrix4x4): Matrix4x4 = {
    val inv = Array.ofDim[Double](4, 4)
  

    inv(0)(0) = m(5) * m(10) * m(15) -
      m(5) * m(11) * m(14) -
      m(9) * m(6) * m(15) +
      m(9) * m(7) * m(14) +
      m(13) * m(6) * m(11) -
      m(13) * m(7) * m(10);

    inv(1)(0) = -m(4) * m(10) * m(15) +
      m(4) * m(11) * m(14) +
      m(8) * m(6) * m(15) -
      m(8) * m(7) * m(14) -
      m(12) * m(6) * m(11) +
      m(12) * m(7) * m(10);

    inv(2)(0) = m(4) * m(9) * m(15) -
      m(4) * m(11) * m(13) -
      m(8) * m(5) * m(15) +
      m(8) * m(7) * m(13) +
      m(12) * m(5) * m(11) -
      m(12) * m(7) * m(9);

    inv(3)(0) = -m(4) * m(9) * m(14) +
      m(4) * m(10) * m(13) +
      m(8) * m(5) * m(14) -
      m(8) * m(6) * m(13) -
      m(12) * m(5) * m(10) +
      m(12) * m(6) * m(9);

    inv(0)(1) = -m(1) * m(10) * m(15) +
      m(1) * m(11) * m(14) +
      m(9) * m(2) * m(15) -
      m(9) * m(3) * m(14) -
      m(13) * m(2) * m(11) +
      m(13) * m(3) * m(10);

    inv(1)(1) = m(0) * m(10) * m(15) -
      m(0) * m(11) * m(14) -
      m(8) * m(2) * m(15) +
      m(8) * m(3) * m(14) +
      m(12) * m(2) * m(11) -
      m(12) * m(3) * m(10);

    inv(2)(1) = -m(0) * m(9) * m(15) +
      m(0) * m(11) * m(13) +
      m(8) * m(1) * m(15) -
      m(8) * m(3) * m(13) -
      m(12) * m(1) * m(11) +
      m(12) * m(3) * m(9);

    inv(3)(1) = m(0) * m(9) * m(14) -
      m(0) * m(10) * m(13) -
      m(8) * m(1) * m(14) +
      m(8) * m(2) * m(13) +
      m(12) * m(1) * m(10) -
      m(12) * m(2) * m(9);

    inv(0)(2) = m(1) * m(6) * m(15) -
      m(1) * m(7) * m(14) -
      m(5) * m(2) * m(15) +
      m(5) * m(3) * m(14) +
      m(13) * m(2) * m(7) -
      m(13) * m(3) * m(6);

    inv(1)(2) = -m(0) * m(6) * m(15) +
      m(0) * m(7) * m(14) +
      m(4) * m(2) * m(15) -
      m(4) * m(3) * m(14) -
      m(12) * m(2) * m(7) +
      m(12) * m(3) * m(6);

    inv(2)(2) = m(0) * m(5) * m(15) -
      m(0) * m(7) * m(13) -
      m(4) * m(1) * m(15) +
      m(4) * m(3) * m(13) +
      m(12) * m(1) * m(7) -
      m(12) * m(3) * m(5);

    inv(3)(2) = -m(0) * m(5) * m(14) +
      m(0) * m(6) * m(13) +
      m(4) * m(1) * m(14) -
      m(4) * m(2) * m(13) -
      m(12) * m(1) * m(6) +
      m(12) * m(2) * m(5);

    inv(0)(3)= -m(1) * m(6) * m(11) +
      m(1) * m(7) * m(10) +
      m(5) * m(2) * m(11) -
      m(5) * m(3) * m(10) -
      m(9) * m(2) * m(7) +
      m(9) * m(3) * m(6);

    inv(1)(3) = m(0) * m(6) * m(11) -
      m(0) * m(7) * m(10) -
      m(4) * m(2) * m(11) +
      m(4) * m(3) * m(10) +
      m(8) * m(2) * m(7) -
      m(8) * m(3) * m(6);

    inv(2)(3) = -m(0) * m(5) * m(11) +
      m(0) * m(7) * m(9) +
      m(4) * m(1) * m(11) -
      m(4) * m(3) * m(9) -
      m(8) * m(1) * m(7) +
      m(8) * m(3) * m(5);

    inv(3)(3) = m(0) * m(5) * m(10) -
      m(0) * m(6) * m(9) -
      m(4) * m(1) * m(10) +
      m(4) * m(2) * m(9) +
      m(8) * m(1) * m(6) -
      m(8) * m(2) * m(5);

    var det = m(0) * inv(0)(0) + m(1) * inv(1)(0) + m(2) * inv(2)(0) + m(3) * inv(3)(0)

    Matrix4x4(inv.map(_.toIndexedSeq).toIndexedSeq)


  }

}
