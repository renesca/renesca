package renesca

import java.math.{ MathContext, BigDecimal => BigDec }

package object compat {
  final def bigDecimal_isDecimalDouble(x:BigDecimal) = {
    val defaultMathContext = MathContext.DECIMAL128

    // FROM: https://github.com/scala/scala/blob/v2.11.4/src/library/scala/math/BigDecimal.scala#L487
    def isDecimalDouble(x:BigDecimal) = {
      val d = x.toDouble
      !d.isInfinity && x.equals(decimal(d))
    }

    // FROM: https://github.com/scala/scala/blob/v2.11.4/src/library/scala/math/BigDecimal.scala#L51
    def decimal(d:Double) = {
      new BigDecimal(new BigDec(java.lang.Double.toString(d), defaultMathContext))
    }

    isDecimalDouble(x)
  }
}
