object MathOps {
  def toBaeycentric(a:V2, b:V2, c:V2, p:V2): V3 ={
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

  def clamp(min: Double, max: Double, p: Double) : Double = {
    if(min <= p && p <= max) p
    else if(p < min) min
    else max
  }
}
