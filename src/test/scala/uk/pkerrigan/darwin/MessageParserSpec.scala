package uk.pkerrigan.darwin

import org.scalatest.FlatSpec
import org.scalatest.EitherValues._
import org.scalatest.Matchers._

class MessageParserSpec extends FlatSpec {

  "A message with no update response" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><notAuR></notAuR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A message with no body" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A message with no origin" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR><TS rid=\"test\"></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A message with an unknown origin" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"UK\"><TS rid=\"test\"></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A message of an unsupported type" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><unknown></unknown></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A train status message with no running id" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A train status message with TD origin" should "result in a TrainStatus from TrainDescriber" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"></TS></uR></pport>")
      .right.value shouldEqual TrainStatus(TrainDescriber(), "test", List())
  }

  "A train status message with CIS origin" should "result in a TrainStatus from Cis" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"CIS\"><TS rid=\"test\"></TS></uR></pport>")
      .right.value shouldEqual TrainStatus(Cis(), "test", List())
  }

  "A train status message with Darwin origin" should "result in a TrainStatus from Darwin" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"Darwin\"><TS rid=\"test\"></TS></uR></pport>")
      .right.value shouldEqual TrainStatus(Darwin(), "test", List())
  }

  "A train status message with Trust origin" should "result in a TrainStatus from Trust" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"Trust\"><TS rid=\"test\"></TS></uR></pport>")
      .right.value shouldEqual TrainStatus(Trust(), "test", List())
  }

  "A train status message with a location having no tiploc" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location><pass at=\"10:55\" /></Location></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A train status message with a location having no times" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST\"></Location></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A train status message with a location having nonsensical times" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST\"><pass at=\"10:55\" /><dep at=\"11:55\" /></Location></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A train status message with a location having unknown time types" should "result in an InvalidMessage" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST\"><pass ut=\"10:55\" /></Location></TS></uR></pport>")
      .left.value shouldBe an[InvalidMessage]
  }

  "A train status message with valid contents" should "result in a TrainStatus" in {
    MessageParser.fromString("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST1\"><dep at=\"10:55\" /></Location><Location tpl=\"TEST2\"><pass et=\"11:55\" /></Location><Location tpl=\"TEST3\"><arr at=\"12:00\" /><dep et=\"12:01\" /></Location><Location tpl=\"TEST4\"><arr et=\"12:30\" /></Location></TS></uR></pport>")
      .right.value shouldEqual TrainStatus(
      TrainDescriber(), "test", List(
        Origin("TEST1", ActualTime("10:55")),
        PassingPoint("TEST2", EstimatedTime("11:55")),
        Station("TEST3", ActualTime("12:00"), EstimatedTime("12:01")),
        Destination("TEST4", EstimatedTime("12:30"))
      )
    )
  }

}
