import scodec.Codec
import scodec.codecs._

case class CtmModel(header: Header, data: Data)

object CtmModel {
  def tag(value: String) = {
    require(value.length == 4)
    (s"$value tag" | constant(ascii.encode(value).require))
  }

  val ctmString = variableSizeBytes(int32L, ascii)

  implicit val codec: Codec[CtmModel] = Codec[Header].flatZipHList(Data.codec _).as[CtmModel]
}
