package cvb.socks

import java.net.Inet4Address
import java.net.Inet6Address

import scodec.bits.BitVector
import scodec.Codec
import scodec.Err
import scodec.codecs._

import scalaz._

object Socks5Codec {

  val version = 0x05
  val verCodec = constant(version)
  type Port = Int

  // actual protocol messages
  case class ClientHello(meths: List[AuthMethod])
  case class HelloResp(meth: AuthMethod)
  case class Request(cmd: CmdType, addr: Addr, port: Port)
  case class Response(reply: ReplyType, addr: Addr, port: Port)
  case class UdoHeader(fragment: Int, addr: Addr, port: Port, data: BitVector)

  val ClientHelloCodec =
    (verCodec :: listOfN(uint8, AuthMethodCodec)).dropUnits.as[ClientHello]

  val HelloRespCodec = (verCodec :: AuthMethodCodec).dropUnits.as[HelloResp]

  val RequestCodec =
    (verCodec :: CmdTypeCodec :: constant(0x00) :: AddrCodec :: PortCodec)
      .dropUnits.as[Request]

  val ResponseCodec =
    (verCodec :: ReplyTypeCodec :: constant(0x00) :: AddrCodec :: PortCodec)
      .dropUnits.as[Response]

  val PortCodec = uint16

  object ReplyTypeCodec extends Codec[ReplyType]

  object CmdTypeCodec extends Codec[CmdType] {

  }

  object AddrCodec extends Codec[Addr] {

  }

  object AuthMethodCodec extends Codec[AuthMethod] {
    def encode(m: AuthMethod) = constant(m.code).encode(m)
    def decode(v: BitVector)  =
      uint8.decode(v).map { case (b, i) => (b, int2auth(i)) }

    def int2auth(i: Int): AuthMethod = i match {
      case NoAuth.code       => NoAuth
      case GSSAPI.code       => GSSAPI
      case Passwd.code       => Passwd
      case NoAcceptable.code => NoAcceptable
      // FIXME: Use proper decoder for IANAAssigned, defined in it's class
      case _                 => IANAAssigned(i)
    }
  }

  sealed abstract class AuthMethod {
    val code: Int
  }

  // X'00' NO AUTHENTICATION REQUIRED
  case object NoAuth extends AuthMethod       { val code = 0x00 }

  //  X'01' GSSAPI
  case object GSSAPI extends AuthMethod       { val code = 0x01 }

  // X'02' USERNAME/PASSWORD
  case object Passwd extends AuthMethod       { val code = 0x02 }

  // X'03' to X'7F' IANA ASSIGNED
  case class IANAAssigned(code: Int) extends AuthMethod {
    def int2codec(v: Int) = v match {
      case _ if v >= 0x03 && v <= 0x7f => \/-(IANAAssigned(v))
      case _ => -\/(Err("IANAAssigned should be in [0x03, 0x7F]"))
    }
    def codec2int (m: IANAAssigned) = \/-(m.code)

    val codec = uint8.exmap(int2codec, codec2int)
  }

  // X'FF' NO ACCEPTABLE METHODS
  case object NoAcceptable extends AuthMethod  { val code = 0xFF }

  sealed abstract class CmdType       { val code: Int }
  // CONNECT X'01'
  case object Connect extends CmdType { val code = 0x01 }
  // BIND X'02'
  case object Bind extends CmdType    { val code = 0x02 }
  // UDP ASSOCIATE X'03'
  case object UDP extends CmdType     { val code = 0x03 }


  sealed abstract class Addr { val code: Int }
  // IP V4 address: X'01'
  case class V4(addr: Inet4Address) { }
  // DOMAINNAME: X'03'
  case class DomainName(addr: String)
  // IP V6 address: X'04'
  case class V6(addr: Inet6Address)



  sealed abstract class ReplyType {
    val code: Int
  }
  // X'00' succeeded
  case object Succeeded extends ReplyType { val code = 0x00 }
  // X'01' general SOCKS server failure
  case object ServerFailure extends ReplyType  { val code = 0x01 }
  // X'02' connection not allowed by ruleset
  case object NotAllowed extends ReplyType  { val code = 0x02 }
  // X'03' Network unreachable
  case object NetUnreachable extends ReplyType  { val code = 0x03 }
  // X'04' Host unreachable
  case object HostUnreachable extends ReplyType  { val code = 0x04 }
  // X'05' Connection refused
  case object ConnRefused extends ReplyType  { val code = 0x05 }
  // X'06' TTL expired
  case object TTLExpired extends ReplyType  { val code = 0x06 }
  // X'07' Command not supported
  case object CommandNotSupported extends ReplyType  { val code = 0x07 }
  // X'08' Address type not supported
  case object AddrTypeNotSupported extends ReplyType  { val code = 0x08 }
  // X'09' to X'FF' unassigned
  case class Unknown(code: Int) extends ReplyType

}
