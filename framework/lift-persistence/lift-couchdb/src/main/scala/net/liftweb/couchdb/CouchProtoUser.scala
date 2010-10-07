package net.liftweb.couchdb

import net.liftweb.record.user.{ MegaProtoUser, MetaMegaProtoUser }
import net.liftweb.record.field.{ StringField, EmailField, PasswordField, BooleanField, LocaleField, TimeZoneField, CountryField }
import net.liftweb.record.KeyField
import net.liftweb.common.{ Box, Full, Empty, Failure }
import net.liftweb.json.Implicits.{ int2jvalue, string2jvalue }
import net.liftweb.json.JsonAST.{ JField, JInt, JObject, JString, render }
import net.liftweb.json.JsonDSL.{ jobject2assoc, pair2Assoc, pair2jvalue }
import net.liftweb.util.FieldError
import _root_.net.liftweb.http.S

trait CouchMegaProtoUser[T <: CouchMegaProtoUser[T]] extends CouchRecord[T] with MegaProtoUser[T] { self: T =>
  def userIdAsString: String = id.valueBox.getOrElse("")
}

trait CouchMetaMegaProtoUser[ModelType <: CouchMegaProtoUser[ModelType]] extends CouchMetaRecord[ModelType] with MetaMegaProtoUser[ModelType] with DesignProvider { self: ModelType =>

  def design: JObject = {
    ("language" -> "javascript") //~
    ("views" -> (
    		("byShortName" -> ("map" -> ("function(doc) { if (doc.type == '" + typeName + """') {
                var name = "";
                if (doc.firstName && doc.lastName) { name = doc.firstName + " " +doc.lastName; }
                else if (doc.firstName) { name = doc.firstName; }
                else if (doc.lastName) { name = doc.lastName; }
                else if (doc.email) { name = doc.email; }
                if (name != "" ) { 
                  emit(doc.firstName+ " "+ doc.lastName, doc);
                }
               } }"""))) ~
    		("byEmail" -> ("map" -> ("function(doc) { if (doc.type == '" + typeName + "') { emit(doc.email, doc); } }")))
    ))
  }

  override def validateUser(user: ModelType) = super.validateUser(user) ++ valUniqueEmail(user)
  override def validateSignup(user: ModelType): List[FieldError] = validateUser(user)
  
  /**
   * If the backend support constraint (RDBMS) and could enforce unicity of email then override this method with return Nil
   */ 
  def valUniqueEmail(user: ModelType) : List[FieldError] = findByEmail(user.email.value) match {
    case Empty => Nil
    case Full(u2) => u2.userIdAsString == user.userIdAsString match {
    	case true => Nil
    	case false => List(FieldError(user.email, S.??("unique.email.address")))
    }
    case _ => List(FieldError(user.email, S.??("unique.email.address")))
  }
  
  def saveUser(user: ModelType) = save(user)

  def findById(id: String): Box[ModelType] = fetch(id)
  def findByUniqueId(uniqueId: String): Box[ModelType] = findById(uniqueId)
  def findByUsername(username: String): Box[ModelType] = singleOrFailure(queryView(designName, "byShortName", { x: View => x.key(JString(username)).limit(2) }))
  def findByEmail(email: String): Box[ModelType] = singleOrFailure(queryView(designName, "byEmail", { x: View => x.key(JString(email)).limit(2) }))

  //  def firstOf(s :Box[Seq[ModelType]]) : Box[ModelType] = s.flatMap(_.headOption.map(x => Full(x)).getOrElse(Empty))
  def firstOf(s: Box[Seq[ModelType]]): Box[ModelType] = for (i <- s; b <- i.headOption) yield b
  def singleOrFailure(s: Box[Seq[ModelType]]) = s.flatMap { s =>
    s.size match {
    	case 0 => Empty
    	case 1 => Full(s.head)
    	case _ => Failure("more than 1 entry found")
    }
  }
}
