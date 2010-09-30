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
package builtin {
package user {

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.Mailer._
import _root_.net.liftweb.http.js.JsCmds._
import xml._
import transform._


trait UserSnippet {
  def login(xhtml: NodeSeq): NodeSeq
  def signup(xhtml: NodeSeq): NodeSeq
  def edit(xhtml: NodeSeq): NodeSeq
  def changePassword(xhtml: NodeSeq): NodeSeq
  def lostPassword(xhtml: NodeSeq): NodeSeq
  def passwordReset(xhtml: NodeSeq): NodeSeq
}

trait CommonUserSnippet[ModelType <: UserDetails] extends UserSnippet {
  def userService: UserService[ModelType]
  def userOperations: UserOperations[ModelType]

  object loginRedirect extends SessionVar[Box[String]](Empty)

  def login(xhtml: NodeSeq): NodeSeq = {
    object email extends RequestVar[String]("")
    if (S.post_?) {
      S.param("username").
      flatMap(username => {email(username); userService.findByUsername(username)}) match {
        case Full(user) if user.userValidated_? &&
          userService.authenticate(user) =>
          S.notice(S.??("logged.in"))
          userService.logUserIn(user)
          val redir = loginRedirect.is match {
            case Full(url) =>
              loginRedirect(Empty)
              url
            case _ =>
              userOperations.homePage
          }
          S.redirectTo(redir)

        case Full(user) if !user.userValidated_? =>
          S.error(S.??("account.validation.error"))

        case _ => S.error(S.??("invalid.credentials"))
      }
    }
    bind("user", xhtml,
      "email" -> (FocusOnLoad(<input type="text" name="username" value={email.is} />)),
      "password" -> (<input type="password" name="password"/>),
      "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }

  def login: NodeSeq = login(loginXhtml)

  def loginXhtml = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{S.??("log.in")}</td></tr>
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
          <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
          <tr><td><a href={userOperations.lostPasswordPath.mkString("/", "/", "")}
                >{S.??("recover.password")}</a></td><td><user:submit /></td></tr></table>
     </form>)
  }

  def changePassword(xhtml: NodeSeq): NodeSeq = {
    val user = userService.currentUser.open_! // we can do this because the logged in test has happened
    var oldPassword = ""
    var newPassword: List[String] = Nil
    bind("user", xhtml,
         "old_pwd" -> SHtml.password("", s => oldPassword = s),
         "new_pwd" -> SHtml.password_*("", S.LFuncHolder(s => newPassword = s)),
         "submit" -> SHtml.submit(S.??("change"), () => changePasswordCallback(user, oldPassword, newPassword)))
  }

  def changePasswordCallback(user: ModelType, oldPassword: String, newPassword: List[String]): Unit = {
    if (!userService.matchPassword(user, oldPassword)) {
      S.error(S.??("wrong.old.password"))
    } else {
        userService.setPassword(user, newPassword)
        userService.validateUser(user) match {
          case Nil => userService.saveUser(user); S.notice(S.??("password.changed")); S.redirectTo(userOperations.homePage)
          case xs => S.error(xs)
        }
      }
  }

  def changePassword: NodeSeq = changePassword(changePasswordXhtml)

  def changePasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("change.password")}</td></tr>
          <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>
          <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>
          <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
     </form>)
  }

  def signup(xhtml: NodeSeq) = {
    val theUser: ModelType = userService.createUser

    def testSignup() {
      validateSignup(theUser) match {
        case Nil =>
          actionsAfterSignup(theUser)
          S.redirectTo(userOperations.homePage)

        case xs => S.error(xs) ; userOperations.signupFunc(Full(innerSignup _))
      }
    }

    def innerSignup = bind("user", signupXhtml(theUser),
                           "submit" -> SHtml.submit(S.??("sign.up"), testSignup _))

    innerSignup
  }

  def signup: NodeSeq = signup(NodeSeq.Empty)

  /**
   * Override this method to validate the user signup (eg by adding captcha verification)
   */
  def validateSignup(user: ModelType): List[FieldError]

  def signupXhtml(user: ModelType) = {
    (<form method="post" action={S.uri}><table><tr><td
              colspan="2">{ S.??("sign.up") }</td></tr>
          {signupForm(user)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
                                        </table></form>)
  }

  def signupForm(user: ModelType): NodeSeq

  def signupMailBody(user: ModelType, validationLink: String) = {
    (<html>
        <head>
          <title>{S.??("sign.up.confirmation")}</title>
        </head>
        <body>
          <p>{S.??("dear")} {user.userFirstName},
            <br/>
            <br/>
            {S.??("sign.up.validation.link")}
            <br/><a href={validationLink}>{validationLink}</a>
            <br/>
            <br/>
            {S.??("thank.you")}
          </p>
        </body>
     </html>)
  }

  def signupMailSubject = S.??("sign.up.confirmation")

  def sendValidationEmail(user: ModelType) {
    val resetLink = S.hostAndPath+"/" + userOperations.validateUserPath.mkString("/") +
    "/" + user.userUniqueId

    val msgXml = signupMailBody(user, resetLink)

    Mailer.sendMail(From(userOperations.emailFrom), Subject(signupMailSubject),
                    (To(user.userEmail) :: xmlToMailBodyType(msgXml) ::
                     (userOperations.bccEmail.toList.map(BCC(_)))) :_* )
  }

  /**
   * Override this method to do something else after the user signs up
   */
  protected def actionsAfterSignup(theUser: ModelType) {
    userService.setUserAccountValidated(theUser, userOperations.skipEmailValidation)
    if (!userOperations.skipEmailValidation) {
      sendValidationEmail(theUser)
      S.notice(S.??("sign.up.message"))
    } else {
      S.notice(S.??("welcome"))
      userService.logUserIn(theUser)
    }
  }

  def edit(xhtml: NodeSeq) = {
    val theUser: ModelType = userService.currentUser.open_! // we know we're logged in

    def testEdit() {
      userService.validateUser(theUser) match {
        case Nil =>
          userService.saveUser(theUser)
          S.notice(S.??("profile.updated"))
          S.redirectTo(userOperations.homePage)

        case xs => S.error(xs); userOperations.editFunc(Full(innerEdit _))
      }
    }

    def innerEdit = bind("user", editXhtml(theUser),
                         "submit" -> SHtml.submit(S.??("edit"), testEdit _))

    innerEdit
  }

  def edit(): NodeSeq = edit(NodeSeq.Empty)

  def editXhtml(user: ModelType) = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("edit")}</td></tr>
          {editForm(user)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
     </form>)
  }

  def editForm(user: ModelType)

  def validateUser(id: String): NodeSeq = userService.findByUniqueId(id) match {
    case Full(user) if !user.userValidated_? =>
      userService.setUserAccountValidated(user, true)
      S.notice(S.??("account.validated"))
      userService.logUserIn(user)
      S.redirectTo(userOperations.homePage)

    case _ => S.error(S.??("invalid.validation.link")); S.redirectTo(userOperations.homePage)
  }

  def lostPassword(xhtml: NodeSeq): NodeSeq = {
    bind("user", xhtml,
         "email" -> SHtml.text("", sendPasswordReset _),
         "submit" -> <input type="submit" value={S.??("send.it")} />)
  }

  def lostPassword(): NodeSeq = lostPassword(lostPasswordXhtml)

  def lostPasswordXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td
              colspan="2">{S.??("enter.email")}</td></tr>
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
     </form>)
  }

  def sendPasswordReset(email: String) {
    userService.findByEmail(email) match {
      case Full(user) if user.userValidated_? =>
        userService.resetUniqueId(user).saveUser(user)
        val resetLink = S.hostAndPath+
        userOperations.passwordResetPath.mkString("/", "/", "/")+user.userUniqueId

        val msgXml = passwordResetMailBody(user, resetLink)
        Mailer.sendMail(From(userOperations.emailFrom),Subject(passwordResetEmailSubject),
                        (To(user.userEmail) :: xmlToMailBodyType(msgXml) ::
                         (userOperations.bccEmail.toList.map(BCC(_)))) :_*)

        S.notice(S.??("password.reset.email.sent"))
        S.redirectTo(userOperations.homePage)

      case Full(user) =>
        sendValidationEmail(user)
        S.notice(S.??("account.validation.resent"))
        S.redirectTo(userOperations.homePage)

      case _ => S.error(S.??("email.address.not.found"))
    }
  }

  def passwordResetEmailSubject = S.??("reset.password.request")

  def passwordResetMailBody(user: ModelType, resetLink: String) = {
    (<html>
        <head>
          <title>{S.??("reset.password.confirmation")}</title>
        </head>
        <body>
          <p>{S.??("dear")} {user.userFirstName},
            <br/>
            <br/>
            {S.??("click.reset.link")}
            <br/><a href={resetLink}>{resetLink}</a>
            <br/>
            <br/>
            {S.??("thank.you")}
          </p>
        </body>
     </html>)
  }

  def passwordResetXhtml = {
    (<form method="post" action={S.uri}>
        <table><tr><td colspan="2">{S.??("reset.your.password")}</td></tr>
          <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>
          <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
     </form>)
  }

  def passwordReset(xhtml: NodeSeq, id: String): NodeSeq =
  userService.findByUniqueId(id) match {
    case Full(user) =>
      def finishSet() {
        userService.validateUser(user) match {
          case Nil => S.notice(S.??("password.changed"))
            userService.saveUser(user)
            userService.logUserIn(user); S.redirectTo(userOperations.homePage)

          case xs => S.error(xs)
        }
      }
      userService.resetUniqueId(user).saveUser(user)

      bind("user", xhtml,
           "pwd" -> SHtml.password_*("", S.LFuncHolder((p: List[String]) => userService.setPassword(user, p))),
           "submit" -> SHtml.submit(S.??("set.password"), finishSet _))
    case _ => S.error(S.??("password.link.invalid")); S.redirectTo(userOperations.homePage)
  }

  def passwordReset(id: String): NodeSeq = passwordReset(passwordResetXhtml, id)
  def passwordReset(xhtml: NodeSeq): NodeSeq = passwordReset(passwordResetXhtml, S.param("id") openOr "")
}

}
}
}
