
case class V3(x: Double, y: Double, z: Double) {

  def u: Double = y
  def v: Double = z
  def uv1: Double = x


  def +(o: V3): V3 = V3(x + o.x, y + o.y, z + o.z)
  def -(o: V3): V3 = V3(x - o.x, y - o.y, z - o.z)
  def *(o: V3): V3 = V3(x * o.x, y * o.y, z * o.z)
  def /(o: V3): V3 = V3(x / o.x, y / o.y, z / o.z)

  def **(ot: V3): Double = x * ot.x + y * ot.y + z * ot.z

  def x(ot: V3): V3 = V3(
    y * ot.z - z * ot.y,
    z * ot.x - x * ot.z,
    x * ot.y - y * ot.x)


  def dropZ: V2 = V2(x, y)

  def length: Double = math.sqrt(x * x + y * y + z * z)

  def norm: V3 = this / V3(length, length, length)

}

