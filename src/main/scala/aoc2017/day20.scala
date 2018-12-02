package aoc2017

object day20 extends App {

  case class Vec(x: Long, y: Long, z: Long) {
    def manhattan = x.abs + y.abs + z.abs
    def +(v: Vec) = Vec(x + v.x, y + v.y, z + v.z)
  }

  case class Particle(i: Int, p: Vec, v: Vec, a: Vec) {
    def acceleration = a.manhattan
    def velocity = v.manhattan
    def position = p.manhattan
    def updated = Particle(i, p + v + a, v + a, a)
  }

  object Particle {
    def apply(s: String, i: Int): Particle = {
      val LineReg =
        "p=<(.*),(.*),(.*)>, v=<(.*),(.*),(.*)>, a=<(.*),(.*),(.*)>".r
      s match {
        case LineReg(px, py, pz, vx, vy, vz, ax, ay, az) =>
          Particle(
            i,
            Vec(px.toLong, py.toLong, pz.toLong),
            Vec(vx.toLong, vy.toLong, vz.toLong),
            Vec(ax.toLong, ay.toLong, az.toLong)
          )
      }
    }
  }

  def part1(input: String): Int = {
    val particles = input.split("\n").zipWithIndex.map {
      case (s, i) => Particle(s, i)
    }

    val minAcc = particles.map(_.acceleration).min

    val particlesMinAcc = particles.filter(_.acceleration == minAcc)

    val minVelo = particlesMinAcc.map(_.velocity).min

    val particlesMinVelo = particlesMinAcc.filter(_.velocity == minVelo)

    particlesMinVelo.minBy(_.position).i
  }

  def part2(input: String): Int = {
    val particles = input.split("\n").zipWithIndex.map {
      case (s, i) => Particle(s, i)
    }

    def loop(count: Int, particles: Set[Particle]): Int =
      if (count <= 0) particles.size
      else {
        val updated = particles.map(_.updated)
        loop(
          count - 1,
          updated.filter(p1 => updated.forall(p2 => p1 == p2 || p1.p != p2.p))
        )
      }
    loop(1000, particles.toSet)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
