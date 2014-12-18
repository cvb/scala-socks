package cvb.socks

import java.net.InetAddress
import java.net.Inet4Address
import java.net.Inet6Address

import scodec.bits.{ BitVector, ByteVector }
import scodec.{ Codec, Err,  DecodingContext }
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

  val PortCodec: Codec[Int] = uint16

  val V4Codec = bytes(4).xmap[V4](
    v => V4(InetAddress.getByAddress(v.toArray).asInstanceOf[Inet4Address]),
    v => ByteVector(v.addr.getAddress())
  )

  val V6Codec = fail(Err("V6 not implemented"))

  val AddrCodec = discriminated[Addr].by(uint8)
    .typecase(0x01, V4Codec).typecase(0x03, DomainNameCodec)

  val RequestCodec =
    (verCodec :: CmdTypeCodec :: constant(0x00) :: AddrCodec :: PortCodec)
      .dropUnits.as[Request]

  val ResponseCodec =
    (verCodec :: ReplyTypeCodec :: constant(0x00) :: AddrCodec :: PortCodec)
      .dropUnits.as[Response]

  object DomainNameCodec extends Codec[DomainName] {
    def encode(s: DomainName) = s match {
      case DomainName(s1) => (uint8 ~ utf8).encode(s1.length ~ s1)
    }
    def decode(v: BitVector) = (for {
      len  <- DecodingContext(uint8.decode(_))
      name <- DecodingContext(fixedSizeBytes(len, utf8).decode(_))
    } yield DomainName(name)).run(v)
  }

  object ReplyTypeCodec extends Codec[ReplyType] {
    def encode(m: ReplyType) = constant(m.code).encode(m)
    def decode(v: BitVector) =
      uint8.decode(v).map { case (b, i) => (b, int2replyType(i)) }

    def int2replyType(i: Int) = i match {
      case Succeeded.code            => Succeeded
      case ServerFailure.code        => ServerFailure
      case NotAllowed.code           => NotAllowed
      case NetUnreachable.code       => NetUnreachable
      case HostUnreachable.code      => HostUnreachable
      case ConnRefused.code          => ConnRefused
      case TTLExpired.code           => TTLExpired
      case CommandNotSupported.code  => CommandNotSupported
      case AddrTypeNotSupported.code => AddrTypeNotSupported
      case code                      => Unknown(code)
    }

  }

  object CmdTypeCodec extends Codec[CmdType] {
    def encode(m: CmdType) = constant(m.code).encode(m)
    def decode(v: BitVector) =
      uint8.decode(v).map { case (b, i) => (b, int2cmdType(i)) }

    def int2cmdType(i: Int) = i match {
      case Connect.code => Connect
      case Bind.code    => Bind
      case UDP.code     => UDP
        // Fixme: use \/ and catch all variants here
    }

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


  sealed abstract class Addr
  // IP V4 address: X'01'
  case class V4(addr: Inet4Address) extends Addr
  // DOMAINNAME: X'03'
  case class DomainName(addr: String) extends Addr
  // IP V6 address: X'04'
  case class V6(addr: Inet6Address) extends Addr



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
