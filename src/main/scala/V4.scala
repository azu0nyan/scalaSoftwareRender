
case class V4(x: Double, y: Double, z: Double, w:Double) {

  def +(o: V4): V4 = V4(x + o.x, y + o.y, z + o.z, w + o.w)
  def -(o: V4): V4 = V4(x - o.x, y - o.y, z - o.z, w - o.w)
  def *(o: V4): V4 = V4(x * o.x, y * o.y, z * o.z, w * o.w )
  def /(o: V4): V4 = V4(x / o.x, y / o.y, z / o.z, w / o.w)

  def **(ot: V4): Double = x * ot.x + y * ot.y + z * ot.z + w * ot.w


  def length: Double = math.sqrt(x * x + y * y + z * z + w * w)

  def norm: V4 = this / V4(length, length, length, length)

  def fromHomo: V3 = if(w == 0) V3(x, y, z) else V3(x / w, y / w, z / w)

  def apply(i:Int): Double = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case 3 => w
  }

}

