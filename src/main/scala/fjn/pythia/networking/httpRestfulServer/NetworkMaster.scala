package fjn.pythia.networking.httpRestfulServer

/**
 * User: fran
 * Date: 10/27/11
 * Time: 6:31 PM
 */


import java.util.Properties

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.annotation.Resource;


import javax.xml.ws.handler.MessageContext;


import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.beans.XMLEncoder;
import java.beans.XMLDecoder
import javax.xml.ws.http.{HTTPException, HTTPBinding}
import javax.xml.ws._

import org.slf4j.{_}


object Int {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.NumberFormatException => None
  }
}


@WebServiceProvider
@ServiceMode(value = javax.xml.ws.Service.Mode.MESSAGE)
@BindingType(value = HTTPBinding.HTTP_BINDING)
class NetworkMaster extends Provider[Source] {

  @Resource
  val ws_ctx: WebServiceContext = null;

  // This method handles incoming requests and generates the response.
  def invoke(request: Source): Source = {
    if (ws_ctx == null) throw new RuntimeException("DI failed on ws_ctx.");
    // Grab the message context and extract the request verb.
    val msg_ctx = ws_ctx.getMessageContext();
    val http_verb = msg_ctx.get(MessageContext.HTTP_REQUEST_METHOD).asInstanceOf[String] trim() toUpperCase;


    // Act on the verb. To begin, only GET requests accepted.
    if (http_verb.equals("GET")) throw new HTTPException(405)//doGet(msg_ctx)
    else if (http_verb.equals("PUT")) throw new HTTPException(405)//doPut(msg_ctx)
    else if (http_verb.compareTo("POST") == 0) throw new HTTPException(405)//doPost(msg_ctx)
    else if (http_verb.compareTo("DELETE") == 0) throw new HTTPException(405)// doDelete(msg_ctx)
    else throw new HTTPException(405); // method not allowed
  }


}

object NetworkMaster {

  def logger = LoggerFactory.getLogger(classOf[NetworkMaster])


  val defaultPort = 8960
  val ServiceName = "/master"
  val workerReg = new scala.collection.mutable.HashMap[String, String]()

  var endPoint: Endpoint = null

  def StartService(): Boolean = {

    try {

      if( endPoint!=null && endPoint.isPublished() )
      {
         logger.warn("Trying to start a service that already is published")
         return false;

      }
      logger.info("Starting NetworkMaster")
      val port =
        System.getProperty("httpPort") match {
          case Int(x) => {
            x
          }
          case _ => {
            logger.warn("httpPort option not presented in command line. Defaulting http port")
            defaultPort
          }
        }
      logger.info("HTTP port=" + port)


      val url = "http://localhost:" + port + ServiceName;
      logger.info("Publishing  restfully on port " + port);
      endPoint = Endpoint.publish(url, new NetworkMaster());

      return true;
    }
    catch {
      case e: HTTPException => logger.error(e.getMessage); false
      case e2: Exception => logger.error(e2.getMessage); false
      case _ => logger.error("Unknown exception"); false
    }

  }

  def StopService: Unit = {

    if (endPoint==null || !endPoint.isPublished())
    {
      logger.warn("trying to close an endpoint that is not initilized of published")
    }
    logger.info("Service is stopping ...")
    endPoint.stop()
    logger.info("Service stopped ...")
  }


}