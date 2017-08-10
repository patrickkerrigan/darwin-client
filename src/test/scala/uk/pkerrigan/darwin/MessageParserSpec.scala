package uk.pkerrigan.darwin

import org.scalatest.FlatSpec
import org.scalatest.EitherValues._

class MessageParserSpec extends FlatSpec {

  "A message with no update response" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><notAuR></notAuR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A message with no body" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A message with no origin" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR><TS rid=\"test\"></TS></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A message of an unsupported type" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><unknown></unknown></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A train status message with no running id" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS></TS></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A train status message with no content" should "result in an empty TrainStatus" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"></TS></uR></pport>")
        .right.value == TrainStatus("TD", "test", List())
    }
  }

  "A train status message with a location having no tiploc" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location><pass at=\"10:55\" /></Location></TS></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A train status message with a location having no times" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST\"></Location></TS></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A train status message with a location having nonsensical times" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST\"><pass at=\"10:55\" /><dep at=\"11:55\" /></Location></TS></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A train status message with a location having unknown time types" should "result in an InvalidMessage" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST\"><pass ut=\"10:55\" /></Location></TS></uR></pport>")
        .left.value.isInstanceOf[InvalidMessage]
    }
  }

  "A train status message with valid contents" should "result in a TrainStatus" in {
    assert {
      Message.parseMessage("<?xml version=\"1.0\" ?><pport><uR updateOrigin=\"TD\"><TS rid=\"test\"><Location tpl=\"TEST1\"><dep at=\"10:55\" /></Location><Location tpl=\"TEST2\"><pass et=\"11:55\" /></Location><Location tpl=\"TEST3\"><arr at=\"12:00\" /><dep et=\"12:01\" /></Location><Location tpl=\"TEST4\"><arr et=\"12:30\" /></Location></TS></uR></pport>")
        .right.value == TrainStatus(
        "TD", "test", List(
          Origin("TEST1", ActualTime("10:55")),
          PassingPoint("TEST2", EstimatedTime("11:55")),
          Station("TEST3", ActualTime("12:00"), EstimatedTime("12:01")),
          Destination("TEST4", EstimatedTime("12:30"))
        )
      )
    }
  }

}
