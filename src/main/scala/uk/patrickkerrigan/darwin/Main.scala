package uk.patrickkerrigan.darwin

import java.io.ByteArrayInputStream
import java.net.InetSocketAddress
import java.util.zip.GZIPInputStream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.tvc.stomp.{Credentials, QueueName, StompSource}

import scala.io.Source

object Main extends App {

  implicit val as = ActorSystem()
  implicit val mat = ActorMaterializer()
  implicit val ec = mat.executionContext

  def gzDecode(b: ByteString): String =
    Source.fromInputStream(new GZIPInputStream(new ByteArrayInputStream(b.toArray))).mkString

  val stompSource = (for {
    queue <- sys.env.get("NATIONAL_RAIL_QUEUE_NAME").toRight("No queue name")
    username <- sys.env.get("NATIONAL_RAIL_USERNAME").toRight("No username")
    password <- sys.env.get("NATIONAL_RAIL_PASSWORD").toRight("No password")
  } yield StompSource(
    queue = QueueName(queue),
    host = InetSocketAddress.createUnresolved("datafeeds.nationalrail.co.uk", 61613),
    credentials = Some(Credentials(login = username, passcode = password))
  )).fold(e => throw new Exception(e), identity)

  stompSource
    .mapConcat(b => b.body.map(gzDecode).toList)
    .runForeach(println(_))
}
