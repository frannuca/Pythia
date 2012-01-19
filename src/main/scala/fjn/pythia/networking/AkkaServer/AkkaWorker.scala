package fjn.pythia.networking.AkkaServer

import org.slf4j.{_}
import akka.actor.Actor.{_}
import akka.actor.Actor

/**
 * Created by fjn group of one
 * User: fran
 * Date: 10/28/11
 * Time: 5:19 PM
 */


object Int {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.NumberFormatException => None
  }
}

class AkkaWorker extends Actor {

  def receive = {
    case xmlStr: String => throw new Exception("Not implemented")
    case dataByte: Array[Byte] => throw new Exception("Not implemented")
    case _ => throw new Exception("Not implemented")
  }
}

object AkkaWorker {


  def logger = LoggerFactory.getLogger(classOf[AkkaWorker])

  val defaultPort = 6960
  def main(args: Array[String]) = {
          initWorker
  }


  val initWorker = {
    val port =
        System.getProperty("akkaPort") match {
          case Int(x) => {
            x
          }
          case _ => {
            logger.warn("httpPort option not presented in command line. Defaulting http port")
            defaultPort
          }
        }

    remote.start("localhost", port) //Start the server
    remote.register("AkkaWorker", actorOf[AkkaWorker]) //Register the actor with the specified service id
  }

}