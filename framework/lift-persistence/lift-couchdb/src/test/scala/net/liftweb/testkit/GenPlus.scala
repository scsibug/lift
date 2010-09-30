package net.liftweb.testkit

import org.scalacheck.Gen
object GenPlus {

  def genNonEmptyString: Gen[String] = for (cs <- Gen.listOf1(Gen.genChar)) yield cs.mkString
  def genPassword: Gen[String] = for (cs <- Gen.listOfN(6, Gen.genChar)) yield cs.mkString

  def genEmail: Gen[String] = for {
    //hbase <- Gen.identifier
    name <- Gen.identifier
  } yield { name + "@test.xx" }
}