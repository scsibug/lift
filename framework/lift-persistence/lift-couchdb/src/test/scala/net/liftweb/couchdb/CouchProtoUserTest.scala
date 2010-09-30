package net.liftweb.couchdb

import net.liftweb.util.FieldError
import net.liftweb.common.{ Empty, Full }
import dispatch.StatusCode
import net.liftweb.json.JsonAST.JObject
import dispatch.Http

import org.scalacheck.Gen
import net.liftweb.testkit.GenPlus

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

@RunWith(classOf[JUnitSuiteRunner])
class CouchProtoUserTest extends Specification with JUnit with ScalaCheck {

  doBeforeSpec {
    val database = new Database("test_users")
    val http = new Http()
    database.createIfNotCreated(http)
    database.storeDesign(UserT, http)
    CouchDB.defaultDatabase = database
  }

  "CouchProtoUser" should {
     "not find unkown user" in {
        UserT.findByEmail("toto@titi.com") must_== Empty
        UserT.findByUsername("zzz") must_== Empty
     }
    "allow basic user lifecycle" in {
      val testData = genUserT
      val user : UserT = testData.sample.get
      //testData must pass { user: UserT =>
        user.id.valueBox must_== Empty
        user.userIdAsString must_== ""

        UserT.validateSignup(user) must_== Nil
        UserT.saveUser(user)
        user.id.valueBox must_!= Empty
        user.userIdAsString must_!= ""



        // there is no cache of couchdb object so there is several object for same id (and same backend object) 
        //user must matchUserT(user)
        //UserT.findByEmail(user.email.value).get must_== user
        UserT.findByEmail(user.email.value).get must matchUserT(user)
        UserT.findByUsername(user.userName).get must matchUserT(user)
        
        //TODO add other lifecycle part login, logout,...
//        UserT.logUserIn(user)
//        UserT.loggedIn_? aka "user login" must beTrue
//        UserT.currentUser must_== user
      //}
    }
  }

  doAfterSpec {
     Http(CouchDB.defaultDatabase delete)
  }

  def genUserT: Gen[UserT] = {
    for {
      email <- GenPlus.genEmail
      firstName <- GenPlus.genNonEmptyString
      lastName <- GenPlus.genNonEmptyString
      passwd <- GenPlus.genPassword
    } yield {
      UserT.createUser.
        firstName(firstName).
        lastName(lastName).
        email(email).
        password(passwd)
    }
  }

  case class matchUserT(v: UserT) extends Matcher[UserT] {
    def apply(other: => UserT) = {
      ((beEqualTo(_: String)) ^^^ ((_: UserT).id.asString) and
//        (beEqualTo(_:String)) ^^^ ((_:UserT).firstName.asString) and
//        (beEqualTo(_:String)) ^^^ ((_:UserT).lastName.asString) and
      (beEqualTo(_: String)) ^^^ ((_: UserT).email.asString))(v)(other)
    }
  }

}

class UserT extends CouchMegaProtoUser[UserT] {
  override def typeName = "UserT"
  def meta = UserT

  override def toString() = allFields.map(_.asString).mkString("UserT[", ",", "]")
}

object UserT extends UserT with CouchMetaMegaProtoUser[UserT] {
}

