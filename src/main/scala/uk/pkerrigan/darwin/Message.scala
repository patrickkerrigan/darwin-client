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

sealed trait Message {
  def origin: String
}
case class ScheduleUpdate(origin: String, runId: String, locations: List[Location]) extends Message

object Message {
  def parseMessage(message: String): Option[Message] =
    (XML.loadString(message) \ "uR")
      .headOption
      .flatMap(this.parseUpdateResponse)

  private def parseUpdateResponse(updateResponse: Node): Option[Message] =
    updateResponse.child
      .headOption
      .flatMap(this.parseMessageType(updateResponse.attribute("updateOrigin").head.text)(_))

  private def parseMessageType(origin: String)(messageContent: Node): Option[Message] =
    messageContent.label match {
      case "TS" => this.parseSchedule(origin)(messageContent)
      case _ => None
    }

  private def parseSchedule(origin: String)(messageContent: Node): Option[ScheduleUpdate] = {
    val locations = (messageContent \ "Location")
      .map(this.parseLocation)
      .toList
    Some(ScheduleUpdate(origin, messageContent.attribute("rid").head.text, locations))
  }

  private def parseLocation(location: Node): Location = {
    val time = this.getTime(location) _
    (time("arr"), time("dep"), time("pass")) match {
      case (Some(arr), Some(dep), None) => Station(location.attribute("tpl").head.text, arr, dep)
      case (None, None, Some(pass)) => Waypoint(location.attribute("tpl").head.text, pass)
      case _ => throw new Exception("Unknown location type")
    }
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
