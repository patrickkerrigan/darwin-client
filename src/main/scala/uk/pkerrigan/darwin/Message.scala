package uk.pkerrigan.darwin

import scala.xml.{Node, XML}

sealed trait Time

case class EstimatedTime(time: String) extends Time

case class ActualTime(time: String) extends Time

sealed trait Location {
  def tiploc: String
}

case class Station(tiploc: String, arrivalTime: Time, departureTime: Time) extends Location

case class Waypoint(tiploc: String, passTime: Time) extends Location

case class Origin(tiploc: String, departureTime: Time) extends Location

case class Destination(tiploc: String, arrivalTime: Time) extends Location

sealed trait Message {
  def origin: String
}

case class ScheduleUpdate(origin: String, runId: String, locations: List[Location]) extends Message

case class InvalidMessage(why: String)

object Message {
  def parseMessage(message: String): Either[InvalidMessage, Message] =
    (XML.loadString(message) \ "uR")
      .headOption
      .toRight(InvalidMessage("No update response"))
      .flatMap(this.parseUpdateResponse)


  private def parseUpdateResponse(updateResponse: Node): Either[InvalidMessage, Message] =
    for {
      child <- updateResponse.child.headOption.toRight(InvalidMessage("No update response body"))
      origin <- updateResponse.attribute("updateOrigin").toRight(InvalidMessage("No update origin"))
      result <- this.parseMessageType(origin.text)(child)
    } yield result

  private def parseMessageType(origin: String)(messageContent: Node): Either[InvalidMessage, Message] =
    messageContent.label match {
      case "TS" => this.parseSchedule(origin)(messageContent)
      case _ => Left(InvalidMessage("Unsupported message type"))
    }

  private def parseSchedule(origin: String)(messageContent: Node): Either[InvalidMessage, ScheduleUpdate] =
    for {
      rid <- messageContent.attribute("rid").toRight(InvalidMessage("No running ID"))
      locations <- this.getLocations(messageContent)
    } yield ScheduleUpdate(origin, rid.text, locations)

  private def getLocations(message: Node): Either[InvalidMessage, List[Location]] =
    (message \ "Location").map(this.parseLocation).foldLeft[Either[InvalidMessage, List[Location]]](Right(List.empty)) {
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
      case (Right(prev), Right(cur)) => Right(prev :+ cur)
    }

  private def parseLocation(location: Node): Either[InvalidMessage, Location] = {
    val time = this.getTime(location) _
    location.attribute("tpl").toRight(InvalidMessage("No tiploc"))
      .flatMap(tiploc => {
        (time("arr"), time("dep"), time("pass")) match {
          case (Some(arr), Some(dep), None) => Right(Station(tiploc.text, arr, dep))
          case (None, Some(dep), None) => Right(Origin(tiploc.text, dep))
          case (Some(arr), None, None) => Right(Destination(tiploc.text, arr))
          case (None, None, Some(pass)) => Right(Waypoint(tiploc.text, pass))
          case _ => Left(InvalidMessage("Unknown location type"))
        }
      })
  }

  private def getTime(messageContent: Node)(name: String): Option[Time] =
    (messageContent \ name)
      .headOption
      .flatMap(x => {
        (x.attribute("at"), x.attribute("et")) match {
          case (Some(time), None) => Some(ActualTime(time.head.text))
          case (None, Some(time)) => Some(EstimatedTime(time.head.text))
          case _ => None
        }
      })
}
