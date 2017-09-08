import CtmModel._
import scodec.Codec
import scodec.codecs._
import shapeless.HNil

case class UvMap(name: String, material: String, data: Vector[Float])

object UvMap {
  def codec(vertexCount: Int) =
    (tag("TEXC") ~> ctmString :: ctmString :: vectorOfN(
      provide(vertexCount * 2),
      floatL)).as[UvMap]

  def listCodec(stats: MeshStats) =
    vectorOfN(provide(stats.uvmaps), UvMap.codec(stats.vertices))
}

case class Attribute(name: String, data: Vector[Float])

object Attribute {
  def codec(vertexCount: Int) =
    (tag("ATTR") ~> ctmString :: vectorOfN(provide(vertexCount * 4), floatL))
      .as[Attribute]
      .as[Attribute]

  def listCodec(stats: MeshStats) =
    vectorOfN(provide(stats.attributes), Attribute.codec(stats.vertices))
}

sealed trait Data

object Data {
  def codec(h: Header): Codec[Data] =
    (Raw.codec(h) :+: Mg1.codec(h))
      .as[Data]
      .discriminatedBy(provide(h.method))
      .auto
}

case class Raw(indices: Vector[Int],
               vertices: Vector[Float],
               normals: Option[Vector[Float]],
               uvs: Vector[UvMap],
               attributes: Vector[Attribute])
    extends Data

object Raw {
  def normalCodec(header: Header) = {
    conditional(
      header.hasNormals,
      tag("NORM") ~> vectorOfN(provide(header.stats.vertices * 3), floatL))
  }

  def vertexCodec(header: Header) = {
    tag("VERT") ~> vectorOfN(provide(header.stats.vertices * 3), floatL)
  }

  def indexCodec(header: Header) = {
    tag("INDX") ~> vectorOfN(provide(header.stats.triangles * 3), int32L)
  }

  implicit val discriminator: Discriminator[Data, Raw, String] =
    Discriminator("RAW\0")

  def codec(header: Header) =
    (indexCodec(header) ::
      vertexCodec(header) ::
      normalCodec(header) ::
      UvMap.listCodec(header.stats) ::
      Attribute.listCodec(header.stats)).as[Raw]
}

case class Mg1() extends Data

object Mg1 {
  def codec(header: Header): Codec[Mg1] = provide(Mg1())

  implicit val discriminator: Discriminator[Data, Mg1, String] =
    Discriminator("MG1\0")
}
