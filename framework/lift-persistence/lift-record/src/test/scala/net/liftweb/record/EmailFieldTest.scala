package net.liftweb.record

import _root_.net.liftweb.util.FieldError
import _root_.net.liftweb.common.Empty
import _root_.net.liftweb.record.field.EmailField
import _root_.net.liftweb.testkit.GenPlus

import org.junit.runner.RunWith
import org.specs.runner.{ JUnitSuiteRunner, JUnit }
import org.specs._

@RunWith(classOf[JUnitSuiteRunner])
class EmailFieldTest extends Specification with JUnit with ScalaCheck {
  private val _emailOwner = EmailOwner.createRecord
  private val _emailField = _emailOwner.email

  "Record with EmailField" should {
    "support several email format" in {

      
      _emailField.set("foo@bar.com")
      _emailField.validate aka "foo@bar.com" must_== Nil

      _emailField.set("FOO@bar.com")
      _emailField.validate aka "FOO@bar.com : with Upper case" must_== Nil

      _emailField.set("foo+filter@bar.com")
      _emailField.validate aka "FOO@bar.com : with '+' filter" must_== Nil

      GenPlus.genEmail must pass { email : String =>
       	_emailField.set(email)
      	_emailField.validate must_== Nil
      }
    }
    "reject email without '@'" in {
      _emailField.set("foo.bar.com")
      _emailField.validate must_!= Nil
    }
    "reject email without 'host'" in {
      _emailField.set("foo@")
      _emailField.validate must_!= Nil

      _emailField.set("foo@bar")
      _emailField.validate must_!= Nil
    }
    //TODO auto-generate entries with invalid char
    "reject email with ':'" in {
      _emailField.set("x:foo@bar.com")
      _emailField.validate must_!= Nil
    }
    "reject email with '/'" in {
      _emailField.set("x/foo@bar")
      _emailField.validate must_!= Nil
    }
  }
}

class EmailOwner extends Record[EmailOwner] {
  def meta = EmailOwner

  object email extends EmailField(this, 100)
}

object EmailOwner extends EmailOwner with MetaRecord[EmailOwner]
