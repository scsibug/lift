/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package record {
package user {
	
import net.liftweb.record.MetaRecord
import _root_.net.liftweb.http._
import js._
import JsCmds._
import _root_.scala.xml.{NodeSeq, Node, Text, Elem}
import _root_.scala.xml.transform._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Mailer._
import S._
import _root_.net.liftweb.builtin.user._
//import _root_.net.liftweb.record.KeyedRecord
import _root_.net.liftweb.record.Record
import net.liftweb.record.field.{ StringField, EmailField, PasswordField, BooleanField, LocaleField, TimeZoneField, CountryField }
import net.liftweb.record.OwnedField 

trait ProtoUser[T <: ProtoUser[T]] extends Record[T] with UserIdAsString {
  self: T =>

  // First Name
  object firstName extends StringField(this, 32) {
    override def displayName = owner.firstNameDisplayName
    override val fieldId = Some(Text("txtFirstName"))
  }

  def firstNameDisplayName = ??("first.name")

  // Last Name
  object lastName extends StringField(this, 32) {
    override def displayName = owner.lastNameDisplayName
    override val fieldId = Some(Text("txtLastName"))
  }

  def lastNameDisplayName = ??("last.name")

  // Email
  object email extends EmailField(this, 100) {
    //valUnique for email is delegated to MetaMegaProtoUser (method valUniqueEmail)
    //override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations
    override def displayName = owner.emailDisplayName
    override val fieldId = Some(Text("txtEmail"))
  }

  def emailDisplayName = ??("email.address")
  // Password
  object password extends PasswordField(this) {
    override def displayName = owner.passwordDisplayName
    def match_?(p : String) : Boolean = valueBox.map( _ == p).getOrElse(false)
  }

  def passwordDisplayName = ??("password")

  object superUser extends BooleanField(this) {
    override def defaultValue = false
  }

  def niceName: String = (firstName.is, lastName.is, email.is) match {
    case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"
    case (f, _, e) if f.length > 1 => f+" ("+e+")"
    case (_, l, e) if l.length > 1 => l+" ("+e+")"
    case (_, _, e) => e
  }

  def shortName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case _ => email.is
  }

  def niceNameWEmailLink = <a href={"mailto:"+email.is}>{niceName}</a>
}

/**
 * Each Record backend should provide its own adaptation of the MegaProtoUser
 * 
 * <pre><code>
 * trait XxxMegaProtoUser[T <: XxxMegaProtoUser[T]] extends XxxRecord[T] with MegaProtoUser[T] { self: T =>
 *   def userIdAsString: String = ...
 * }
 * <code></pre>
 * 
 * @see  net.liftweb.couchdb.CouchMegaProtoUser 
 * @todo provide a demo/sample code
 */
trait MegaProtoUser[T <: MegaProtoUser[T]] extends ProtoUser[T] with UserDetails {
  self: T =>

  object validated extends BooleanField[T](this) {
    override def defaultValue = false
    override val fieldId = Some(Text("txtValidated"))
  }

  object locale extends LocaleField[T](this) {
    override def displayName = owner.localeDisplayName
    override val fieldId = Some(Text("txtLocale"))
  }

  object timezone extends TimeZoneField[T](this) {
    override def displayName = owner.timezoneDisplayName
    override val fieldId = Some(Text("txtTimeZone"))
  }

  def timezoneDisplayName = ??("time.zone")

  def localeDisplayName = ??("locale")

  def userEmail = email.is

  def userName = shortName

  def userLastName = lastName.is

  def userFirstName = firstName.is

  def userUniqueId = userIdAsString

  override def userSuperUser_? = superUser.is
}

/**
 * Each Record backend should provide its own adaptation of the MetaMegaProtoUser and provide missing info
 * 
 * <pre><code>
 * trait XxxMetaMegaProtoUser[ModelType <: XxxMegaProtoUser[ModelType]] extends XxxMetaRecord[ModelType] with MetaMegaProtoUser[ModelType] { self: ModelType =>
 *   def resetUniqueId(user: ModelType) = ...
 *   override def validateUser(user: ModelType) = super.validateUser(user) ++ valUniqueEmail(user)
 *   override def validateSignup(user: ModelType): List[FieldError] = validateUser(user)
 *   /**
 *    * If the backend support constraint (RDBMS) and could enforce unicity of email then override this method with return Nil
 *    */ 
 *	 def valUniqueEmail(user: ModelType) : List[FieldError] = ...
 *   
 *   def saveUser(user: ModelType) = save(user)
 *
 *   def findById(id: String): Box[ModelType] = ...
 *   def findByUniqueId(uniqueId: String): Box[ModelType] = ...
 *   def findByUsername(username: String): Box[ModelType] = ...
 *   def findByEmail(email: String): Box[ModelType] = ...
 * }
 * <code></pre>
 *  
 * @see  net.liftweb.couchdb.CouchMetaMegaProtoUser 
 * @todo provide a demo/sample code
 */
trait MetaMegaProtoUser[ModelType <: MegaProtoUser[ModelType]] extends MetaRecord[ModelType]
        with UserRecordService[ModelType] with UserOperations[ModelType]
        with UserRecordSnippet[ModelType] {
  self: ModelType => 

  case class MenuItem(name: String, path: List[String],
                      loggedIn: Boolean) {
    lazy val endOfPath = path.last
    lazy val pathStr: String = path.mkString("/", "/", "")
    lazy val display = name match {
      case null | "" => false
      case _ => true
    }
  }

  /**
   * Return the URL of the "login" page
   */
  def loginPageURL = loginPath.mkString("/","/", "")

  def loginFirst = If(
    loggedIn_? _,
    () => {
      import net.liftweb.http.{RedirectWithState, RedirectState}
      val uri = S.uriAndQueryString
      RedirectWithState(
        loginPageURL,
        RedirectState( ()=>{loginRedirect.set(uri)})
      )
    }
  )

  def userMenu: List[Node] = {
    val li = loggedIn_?
    ItemList.
    filter(i => i.display && i.loggedIn == li).
    map(i => (<a href={i.pathStr}>{i.name}</a>))
  }

  lazy val ItemList: List[MenuItem] =
  List(MenuItem(S.??("sign.up"), signUpPath, false),
       MenuItem(S.??("log.in"), loginPath, false),
       MenuItem(S.??("lost.password"), lostPasswordPath, false),
       MenuItem("", passwordResetPath, false),
       MenuItem(S.??("change.password"), changePasswordPath, true),
       MenuItem(S.??("log.out"), logoutPath, true),
       MenuItem(S.??("edit.profile"), editPath, true),
       MenuItem("", validateUserPath, false))

  protected object curUserId extends SessionVar[Box[String]](Empty)

  def testLoggedIn(page: String): Boolean =
  ItemList.filter(_.endOfPath == page) match {
    case x :: xs if x.loggedIn == loggedIn_? => true
    case _ => false
  }

  def userOperations = this

  def userService = this
}

trait UserRecordService[ModelType <: MegaProtoUser[ModelType]] extends MetaRecord[ModelType]
        with UserService[ModelType] {
  self: ModelType =>
  def validateUser(user: ModelType) = user.validate

  def setUserAccountValidated(user: ModelType, validated: Boolean) = {
    user.validated(validated)
    saveUser(user)
    this
  }

  def setPassword(user: ModelType, passwords: List[String]) = setPassword(user, passwords.head)

  def setPassword(user: ModelType, password: String) = {
    user.password.setFromString(password)
    this
  }

  def matchPassword(user: ModelType, password: String) = user.password.match_?(password)

//  def resetUniqueId(user: ModelType) = {
//    user.uniqueId.reset()
//    this
//  }

  //def saveUser(user: ModelType)

  def authenticate(user: ModelType) = matchPassword(user, S.param("password").openOr("*"))

  def createUser = createRecord
}


trait UserRecordSnippet[ModelType <: MegaProtoUser[ModelType]] extends CommonUserSnippet[ModelType] with MetaRecord[ModelType]{
  self: ModelType =>

  def signupFields: List[OwnedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil

  override def fieldOrder/*: List[OwnedField[ModelType]]*/ = firstName :: lastName :: email :: locale :: timezone :: password :: Nil

  def validateSignup(user: ModelType): List[FieldError] = user.validate

  def signupForm(user: ModelType): NodeSeq = localForm(user, false)

  def editForm(user: ModelType) = localForm(user, true)

  protected def localForm(user: ModelType, ignorePassword: Boolean): NodeSeq = {
    signupFields.
    flatMap(fi => user.fieldByName(fi.name)).
    filter(f => !ignorePassword || (f match {
          case f: PasswordField[ModelType] => false
          case _ => true
        })).
    flatMap(f =>
      f.toForm.toList.map(form =>
        (<tr><td>{f.displayName}</td><td>{form}</td></tr>) ) )
  }
}

}
}
}
