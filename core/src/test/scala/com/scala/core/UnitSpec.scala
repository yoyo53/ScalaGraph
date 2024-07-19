package com.scala.core

import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

abstract class UnitSpec extends AnyFlatSpec with Matchers with OptionValues with Inside with Inspectors with BeforeAndAfter with BeforeAndAfterEach