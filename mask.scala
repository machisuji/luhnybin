object Masker {
  val (minDigits, maxDigits) = (14, 16)

  def luhnCheck(digits: Seq[Int]): Boolean = {
    digits.reverse.zipWithIndex.map { case (digit, index) =>
      if (index % 2 == 1) digit * 2 else digit
    }.flatMap { num =>
      if (num < 10) Seq(num) else Seq(1, num % 10)
    }.sum % 10 == 0
  }
  def checkCC(number: Seq[Char], len: Int): Boolean = {
    val ds = number.filter(_.isDigit).map(_ - '0')
    ds.size == len && luhnCheck(ds)
  }
  def startsWithDigit(str: String) = str.headOption.exists(_.isDigit)
  def isCCDigit(chr: Char) = chr.isDigit || chr == '-' || chr.isWhitespace

  // @scala.annotation.tailrec
  @scala.annotation.tailrec
  def split(parts: List[String], str: String, text: Boolean): List[String] = {
    val (part, rest) = str.span(c => if (text) !c.isDigit else isCCDigit(c))
    val maskedPart = if (!text) maskCC(part) else part
    if (rest.isEmpty) (maskedPart :: parts).reverse
    else split(maskedPart :: parts, rest, !text)
  }
  def split(str: String): List[String] = split(Nil, str, !startsWithDigit(str))

  def maskCC(str: String): String = {
    val num = str.filter(_.isDigit)
    val is = (14 to 16).map(len => num.sliding(len, 1).zipWithIndex.flatMap { case (seq, index) =>
      if (checkCC(seq, len)) Some(index until (index + len) toIndexedSeq) else None
    }.fold(Nil)(_++_).distinct).fold(Nil)(_++_).distinct
    val mi = num.zipWithIndex.map{case (c, i) => c -> (if (is contains i) true else false)}.toIterator
    var pc: Option[(Char, Boolean)] = None
    for (c <- str) yield {
      pc = pc orElse (if (mi.hasNext) Some(mi.next) else None)
      if (pc.isDefined) {
        val (chr, mask) = pc.get
        if (chr == c) {
          pc = None
          if (mask) 'X' else chr
        } else c
      } else c
    }
  }

  def mask(str: String): String = split(str).mkString
}

val in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
Iterator.continually(in.readLine).takeWhile(null ne).foreach(line => println(Masker mask line))
