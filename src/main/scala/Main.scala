package cvb.socks

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import akka.actor.ActorLogging
import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress

// object Greeter {
//   case object Greet
//   case object Done
// }

// class HelloWorld extends Actor {

//   val log = Logging(context.system, this)
//   override def preStart(): Unit = {
//     log.debug("starting...")
//     // create the greeter actor
//     val greeter = context.actorOf(Props[Greeter], "greeter")
//     // tell it to perform the greeting
//     greeter ! Greeter.Greet
//   }

//   def receive = {
//     // when the greeter is done, stop this actor and with it the application
//     case Greeter.Done =>
//       log.info("greeter done")
//       context.stop(self)
//   }
// }

// class Greeter extends Actor {
//   def receive = {
//     case Greeter.Greet =>
//       println("Hello World!")
//       sender() ! Greeter.Done
//   }
// }



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
      val handler = context.actorOf(Props[SimpleEcho])
      val connection = sender()
      connection ! Register(handler)
      // context stop self
  }
}

class SimpleEcho extends Actor with ActorLogging {
  import Tcp._

  def receive = {
    case Received(data) =>
      log.info(data.utf8String + "q weqwe")
      sender() ! Write(ByteString.fromString(data.utf8String + "!"))
    case PeerClosed     => context stop self
  }
}

object Main {
  def main(args: Array[String]) {
    akka.Main.main(Array(classOf[Listener].getName))
  }
}
