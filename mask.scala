object Masker {
  val (minDigits, maxDigits) = (14, 16)

  def luhnCheck(digits: Seq[Int]): Boolean = {
    digits.reverse.zipWithIndex.map { case (digit, index) =>
      if (index % 2 == 1) digit * 2 else digit
    }.flatMap { num =>
      if (num < 10) Seq(num) else Seq(1, num % 10)
    }.sum % 10 == 0
  }
  def checkCC(number: Seq[Char]): Int = {
    val ds = number.filter(_.isDigit).map(_ - '0')
    if (ds.size >= minDigits && ds.size <= maxDigits && luhnCheck(ds)) ds.size
    else 0
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
    
  }

  def mask(str: String): String = split(str).mkString(" | ")
}

val in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
Iterator.continually(in.readLine).takeWhile(null ne).foreach(line => println(Masker mask line))
