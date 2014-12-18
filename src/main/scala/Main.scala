package cvb.socks

import akka.event.Logging
import akka.actor.{ Actor, ActorRef, Props, ActorSystem, ActorLogging }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress

import scodec.bits.BitVector
import cvb.socks.Socks5Codec._

class Listener extends Actor with ActorLogging {
  import context.system
  import Tcp._

  val manager = IO(Tcp)

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 9333))

  def receive = {
    case b @ Bound(localAddress) =>
      log.info("successfully bounded")

    case CommandFailed(_: Bind) => context stop self

    case c @ Connected(remote, local) =>
      val handler = context.actorOf(Props[SimpleSocks])
      val connection = sender()
      connection ! Register(handler)
  }
}

class SimpleSocks extends Actor with ActorLogging {
  import Tcp._
  import context._

  def receive = {
    case Received(data) => {
      log.info(s"get data $data")
      val h = ClientHelloCodec.decode(BitVector(data))
      log.info(s"data was: $h")
      HelloRespCodec.encode(HelloResp(NoAuth)).map { s =>
        sender() ! Write(ByteString(s.toByteBuffer))
        become(requestHandler(sender()))
      }
    }

    case PeerClosed     => context stop self
  }

  def requestHandler(client: ActorRef): Receive = {
    case Received(data) => {
      val h = RequestCodec.decode(BitVector(data)).map {
        case (_, Request(Socks5Codec.Connect, addr, port)) =>
          log.info(s"Request got connect to: $addr, $port")
          val remote = context.actorOf(SimpleRemote.props(addr, port))
          become({
            case "connect failed" =>
              log.info(s"Failed to connect: $addr, $port")
              ResponseCodec.encode(
                Response(ServerFailure, addr, port)).map { r =>
                client ! Write(ByteString(r.toByteBuffer))
            }
            case Connected(_, _) =>
              log.info(s"Connected: $addr, $port")
              ResponseCodec.encode(Response(Succeeded, addr, port)).map { r =>
                client ! Write(ByteString(r.toByteBuffer))
              }
              become(forwarding(client, remote))
          })
          // RequestCodec.encode(
        case (_, _) =>
          ResponseCodec.encode(
            Response(CommandNotSupported, DomainName("noname"), 111)).map { r =>
            sender() ! Write(ByteString(r.toByteBuffer))
          }
      }
    }

    case PeerClosed     => context stop self
  }

  def forwarding(client: ActorRef, remote: ActorRef): Receive = {
    case Received(data)   =>
      log.info(s"Data from client $data")
      remote ! data
    case data: ByteString =>
      log.info(s"Data from remote $data")
      client ! Write(data)
    case PeerClosed       => context stop self
      log.info(s"Peer closed, forwarding stopped")
    case "connection closed" =>
      log.info(s"Remote closed, forwarding stopped")
      context stop self
  }

}

object SimpleRemote {
  def props(remote: Addr, port: Int) =
    Props(classOf[SimpleRemote], remote, port)
}

class SimpleRemote(host: Addr, port: Int) extends Actor with ActorLogging {

  import java.net.InetSocketAddress
  import Tcp._
  import context.system

  val addr = (host match {
    case V4(a)         => new InetSocketAddress(a, port)
    case DomainName(n) => new InetSocketAddress(n, port)
    case V6(a)         => new InetSocketAddress(a, port)
  })

  IO(Tcp) ! Connect(addr)

  def receive = {
    case CommandFailed(_: Connect) =>
      context.parent ! "connect failed"
      context stop self

    case c @ Connected(remote, local) =>
      context.parent ! c
      val connection = sender()
      connection ! Register(self)
      context become {
        case data: ByteString =>
          connection ! Write(data)
        case CommandFailed(w: Write) =>
          // O/S buffer was full
          context.parent ! "write failed"
        case Received(data) =>
          context.parent ! data
        case "close" =>
          connection ! Close
        case _: ConnectionClosed =>
          context.parent ! "connection closed"
          context stop self
      }
  }


}

object Main {
  def main(args: Array[String]) {
    akka.Main.main(Array(classOf[Listener].getName))
  }
}
