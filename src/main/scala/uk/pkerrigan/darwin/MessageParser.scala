package uk.pkerrigan.darwin

import scala.xml.{Node, XML}

/**
  * Parse XML messages produced by Darwin PushPort
  */
object MessageParser {
  def fromString(message: String): Either[InvalidMessage, Message] =
    (XML.loadString(message) \ "uR")
      .headOption
      .toRight(InvalidMessage("No update response"))
      .flatMap(this.parseUpdateResponse)

  private def parseUpdateResponse(updateResponse: Node): Either[InvalidMessage, Message] =
    for {
      child <- updateResponse.child.headOption.toRight(InvalidMessage("No update response body"))
      originAttribute <- updateResponse.attribute("updateOrigin").toRight(InvalidMessage("No update origin"))
      origin <- this.parseOrigin(originAttribute.text)
      result <- this.parseMessageType(origin)(child)
    } yield result


  private def parseOrigin(text: String): Either[InvalidMessage, MessageOrigin] =
    text match {
      case "TD" => Right(TrainDescriber())
      case "CIS" => Right(Cis())
      case "Darwin" => Right(Darwin())
      case "Trust" => Right(Trust())
      case _ => Left(InvalidMessage(s"Unknown origin $text"))
    }

  private def parseMessageType(origin: MessageOrigin)(messageContent: Node): Either[InvalidMessage, Message] =
    messageContent.label match {
      case "TS" => this.parseTrainStatus(origin)(messageContent)
      case _ => Left(InvalidMessage(s"Unsupported message type ${messageContent.label}"))
    }

  private def parseTrainStatus(origin: MessageOrigin)(messageContent: Node): Either[InvalidMessage, TrainStatus] =
    for {
      rid <- messageContent.attribute("rid").toRight(InvalidMessage("No running ID"))
      locations <- this.getLocations(messageContent)
    } yield TrainStatus(origin, rid.text, locations)

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
          case (None, None, Some(pass)) => Right(PassingPoint(tiploc.text, pass))
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
