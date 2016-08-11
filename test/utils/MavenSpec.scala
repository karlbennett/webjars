package utils

import akka.util.Timeout
import play.api.test._

import scala.concurrent.duration._

class MavenSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val maven = application.injector.instanceOf[Maven]

  "artifactId" should {
    "convert a name to a name" in {
      await(maven.artifactId("foo")) must beEqualTo ("foo")
    }
    "convert a github url to a name" in {
      await(maven.artifactId("mochajs/mocha")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a git:// url to a name" in {
      await(maven.artifactId("git://github.com/mochajs/mocha.git")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a https:// url to a name" in {
      await(maven.artifactId("https://github.com/mochajs/mocha.git")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a scoped name" in {
      await(maven.artifactId("@reactivex/rxjs")) must beEqualTo ("reactivex__rxjs")
    }
    "not go case insensitive for github repos" in {
      await(maven.artifactId("MochaJS/Mocha")) must beEqualTo ("github-com-MochaJS-Mocha")
    }
    "not go case insensitive for non-github repos" in {
      await(maven.artifactId("Foo")) must beEqualTo ("Foo")
    }
  }

}
