package renesca

package object helpers {

  implicit class SeqUnzip4[+A](val seq: Seq[A]) extends AnyVal {
    def unzip4[A1, A2, A3, A4](implicit asTuple4: A => (A1, A2, A3, A4)): (List[A1], List[A2], List[A3], List[A4]) = {
      val b1 = List.newBuilder[A1]
      val b2 = List.newBuilder[A2]
      val b3 = List.newBuilder[A3]
      val b4 = List.newBuilder[A4]

      for(abcd <- seq) {
        val (a, b, c, d) = asTuple4(abcd)
        b1 += a
        b2 += b
        b3 += c
        b4 += d
      }
      (b1.result(), b2.result(), b3.result(), b4.result())
    }
  }
}
