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
package field {

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{ S, DocType }
import _root_.java.util.regex._
import Helpers._
import S._
import _root_.net.liftweb.record.{ TypedField, Record }

trait RegExpTypedField extends TypedField[String] {

  def regExp: Box[String] = Empty
  def invalidFormatMsg: Box[String] = Empty

  private val _pattern = regExp.map(Pattern.compile(_))
  private def matchPattern_?(v: String): Boolean = _pattern.map(_.matcher(v).matches).getOrElse(true)

  private def validateRegExp(v: ValueType): List[FieldError] = {
    toBoxMyType(v) match {
      case Full(x) if matchPattern_?(x) => Nil
      case _ => Text(S.??(invalidFormatMsg.getOrElse("invalid.format")))
    }
  }

  override def validations = validateRegExp _ :: super.validations

  //TODO find a better way to support html5 addtional attributes (and a way to transform every type of field)
  protected def elemAttr: MetaData = {
    val b = new UnprefixedAttribute("type", "text", Null) // to inject maxLength,...
    isHtml5 match {
      case true => {
        val b2 = regExp.map { x => new UnprefixedAttribute("pattern", x, b) }.getOrElse(b)
        new UnprefixedAttribute("required", (!optional_?).toString, b2)
      }
      case false => b
    }
  }

  protected def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))) { funcName =>
    val b = <input name={ funcName } value={ valueBox openOr "" } tabindex={ tabIndex toString }/>
    b % elemAttr
  }

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  protected def isHtml5 = S.getDocType._2 match {
    case Full(x) if x == DocType.html5 => true
    case _ => false
  }
}

}
}
}