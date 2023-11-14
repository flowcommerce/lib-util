package io.flow.util

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class BooleansSpec extends AnyWordSpecLike with Matchers {

  "parse" in {
    Booleans.parse("") must be(None): Unit
    Booleans.parse("  ") must be(None): Unit
    Booleans.parse(" Foo ") must be(None): Unit

    Booleans.parse(" t ") must be(Some(true)): Unit
    Booleans.parse(" TRUE ") must be(Some(true)): Unit

    Booleans.parse(" f ") must be(Some(false)): Unit
    Booleans.parse(" FALSE ") must be(Some(false))
  }

  "parses true values" in {
    Booleans.TrueValues.foreach { v =>
      Booleans.parse(v) must be(Some(true))
    }
  }

  "parses false values" in {
    Booleans.FalseValues.foreach { v =>
      Booleans.parse(v) must be(Some(false))
    }
  }

}
