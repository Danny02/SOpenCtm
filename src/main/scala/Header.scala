import CtmModel._
import scodec.Codec
import scodec.codecs._

case class MeshStats(vertices: Int,
                     triangles: Int,
                     uvmaps: Int,
                     attributes: Int)

object MeshStats {
  implicit val codec = (("vertex count" | int32L) ::
    ("triangle count" | int32L) ::
    ("uvmap count" | int32L) ::
    ("attribute count" | int32L)).as[MeshStats]
}

case class Header(version: Int,
                  method: String,
                  stats: MeshStats,
                  hasNormals: Boolean,
                  comment: String)

object Header {
  private val flags: Codec[Boolean] =
    int32L.xmap(i => (i & 1) > 0, if (_) 1 else 0)

  implicit val codec: Codec[Header] = (tag("OCTM") ~>
    ("format version" | int32L) ::
    ("method tag" | fixedSizeBytes(4, ascii)) ::
    Codec[MeshStats] ::
    flags ::
    ctmString).as[Header]
}
