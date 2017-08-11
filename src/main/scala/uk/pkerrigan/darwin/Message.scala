package uk.pkerrigan.darwin

sealed trait MessageOrigin

case class TrainDescriber() extends MessageOrigin
case class Cis() extends MessageOrigin
case class Darwin() extends MessageOrigin
case class Trust() extends MessageOrigin

sealed trait Time

case class EstimatedTime(time: String) extends Time

case class ActualTime(time: String) extends Time

sealed trait Location {
  def tiploc: String
}

case class Station(tiploc: String, arrivalTime: Time, departureTime: Time) extends Location

case class PassingPoint(tiploc: String, passTime: Time) extends Location

case class Origin(tiploc: String, departureTime: Time) extends Location

case class Destination(tiploc: String, arrivalTime: Time) extends Location

sealed trait Message {
  def origin: MessageOrigin
}

case class TrainStatus(origin: MessageOrigin, runId: String, locations: List[Location]) extends Message

case class InvalidMessage(why: String)
