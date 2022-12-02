import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Render {


  val objData: ObjData = Obj("uaz.obj")
  val texture: BufferedImage = ImageIO.read(new File("uaz_med_white_d.png"))

  def getColor(u: Double, v: Double) : Int = {
    val nu = ((u % 1) + 1 ) %1
    val nv = 1 - ((v % 1) + 1 ) %1
    texture.getRGB((nu * texture.getWidth).toInt, (nv * texture.getHeight).toInt)
  }

  val look = V3(0, 0, -1).norm
  val light = V3(1, 1, -1).norm
  val zBuffer: Array[Array[Double]] = Array.ofDim(Main.w, Main.h)


  def renderModel(objData: ObjData)(implicit bi: BufferedImage): Unit = {
    for (((i1, it1, in1), (i2, it2, in2), (i3, it3, in3)) <- objData.f) {
      val v1 = objData.v(i1) + V3(300, 300, 0)
      val v2 = objData.v(i2) + V3(300, 300, 0)
      val v3 = objData.v(i3) + V3(300, 300, 0)

      val vt1 = objData.vt(it1)
      val vt2 = objData.vt(it2)
      val vt3 = objData.vt(it3)

      renderTriangle(v1, v2, v3, vt1, vt2, vt3)
    }
  }

  def renderImage(implicit bi: BufferedImage): Unit = {
    for(x <- 0 until Main.w; y<- 0 until Main.h){
      zBuffer(x)(y) = Double.MinValue
    }

    renderModel(objData)

  }

  def renderTriangle(a: V3, b: V3, c: V3, at: V2, bt: V2, ct: V2)(implicit bi: BufferedImage): Unit = {

    val norm = ((b - a) x (c - a)).norm

    val side = (V3(-1, -1, -1) * look) ** norm

    if (side >= 0) {
      val minx = MathOps.clamp(0, bi.getWidth, Seq(a.x, b.x, c.x, bi.getWidth - 1).min).toInt
      val maxx = MathOps.clamp(0, bi.getWidth, Seq(a.x, b.x, c.x, 0).max).toInt
      val miny = MathOps.clamp(0, bi.getHeight, Seq(a.y, b.y, c.y).min).toInt
      val maxy = MathOps.clamp(0, bi.getHeight, Seq(a.y, b.y, c.y, 0).max).toInt


      val lP = ((V3(-1, -1, -1) * light) ** norm + 1) / 2

      for (x <- minx to maxx;
           y <- miny to maxy) {
        val bar = MathOps.toBaeycentric(a.dropZ, b.dropZ, c.dropZ, V2(x, y))

        val z = bar.x * a.z + bar.y * b.z + bar.z * c.z

        val uv = at * bar.x + bt * bar.y  + ct * bar.z

        def inTiangle(x: Int, y: Int): Boolean = bar.u + bar.v <= 1 && bar.u >= 0 && bar.v >= 0

        val col = new Color(getColor(uv.x, uv.y))
        val colL = new Color((col.getRed * lP).toInt, (col.getGreen * lP).toInt, (col.getBlue * lP).toInt)

        if (zBuffer(x)(y) < z && inTiangle(x, y)) {
          bi.setRGB(x, y, colL.getRGB)
          zBuffer(x)(y) = z
        }
      }
    }
  }


  def drawLine(f: V2, t: V2, color: Int)(implicit bi: BufferedImage): Unit = {
    val dist = t - f
    if (math.abs(dist.x) >= math.abs(dist.y)) {
      val diff = dist.y / dist.x
      var y = f.y
      for (x <- f.x.toInt to t.x.toInt by (if (f.x < t.x) 1 else -1)) {
        bi.setRGB(x, y.toInt, color)
        y += diff
      }
    } else {
      val diff = dist.x / dist.y
      var x = f.x
      for (y <- f.y.toInt to t.y.toInt by (if (f.y < t.y) 1 else -1)) {
        bi.setRGB(x.toInt, y, color)
        x += diff
      }
    }
  }


  //  def renderTriangle(a: V2, b: V2, c: V2, color: Int)(implicit bi: BufferedImage): Unit = {
  //    val minx = MathOps.clamp(0, bi.getWidth, Seq(a.x, b.x, c.x, bi.getWidth - 1).min).toInt
  //    val maxx = MathOps.clamp(0, bi.getWidth, Seq(a.x, b.x, c.x, 0).max).toInt
  //    val miny = MathOps.clamp(0, bi.getHeight, Seq(a.y, b.y, c.y).min).toInt
  //    val maxy = MathOps.clamp(0, bi.getHeight, Seq(a.y, b.y, c.y, 0).max).toInt
  //
  //
  //    for (x <- minx to maxx;
  //         y <- miny to maxy) {
  //      val bar = MathOps.toBaeycentric(a, b, c, V2(x, y))
  //      def inTiangle(x: Int, y: Int): Boolean = bar.u + bar.v <= 1 && bar.u >= 0 && bar.v >= 0
  //
  //
  //      if (inTiangle(x, y)) {
  //
  //        bi.setRGB(x, y, color)
  //      }
  //    }
  //  }

  //  def renderTestData()(implicit bi: BufferedImage): Unit = {
  //    for (i <- 0 until 32) {
  //      drawLine(V2(400, 400), V2(400, 400) + V2(100, 0)
  //        .rotate(Math.PI * 2 * i / 32), Color.RED.getRGB)
  //    }
  //
  //
  //    for (i <- 0 until 3) {
  //      val a = V2(500, 500)
  //      val b = a + V2(100, 0).rotate(Math.PI * 2 * i / 3)
  //      val c = a + V2(100, 0).rotate(Math.PI * 2 * (i + 1) / 3)
  //      renderTriangle(a, b, c, Color.RED.getRGB)
  //    }
  //
  //  }
}
