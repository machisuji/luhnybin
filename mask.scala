object Masker {
  def digits(chrs: Seq[Char]): Seq[Int] = chrs.map(_ - 48)
  def digits(str: String): Seq[Int] = digits(str.toSeq)

  def luhnCheck(digits: Seq[Int]): Boolean = {
    digits.reverse.zipWithIndex.map { case (digit, index) =>
      if (index % 2 == 1) digit * 2
      else digit
    }.flatMap { num =>
      if (num < 10) Seq(num)
      else Seq(1, num % 10)
    }.sum % 10 == 0
  }

  def checkCC(number: Seq[Char]): Boolean = {
    val ds = digits(number.filter(_.isDigit))
    ds.size >= 14 && ds.size <= 16 && luhnCheck(ds)
  }

  def isCCDigit(chr: Char) = chr.isDigit || chr == '-' || chr == ' '

  @scala.annotation.tailrec
  def maskCC(prefix: String, number: Seq[Char], suffix: String): String = {
    val numDigits = number.filter(_.isDigit).size
    val hit = checkCC(number) && (numDigits == 16 || !suffix.headOption.exists(isCCDigit))

    if (hit)                  return maskCC(prefix ++ number.map(chr => if (chr.isDigit) 'X' else chr) ++ suffix)
    else if (suffix.isEmpty)  return prefix ++ number

    val chr = suffix.head

    if (numDigits >= 16)      maskCC(prefix :+ number.head,     number.tail,    suffix)
    else if (isCCDigit(chr))  maskCC(prefix,                    number :+ chr,  suffix.tail)
    else                      maskCC(prefix ++ number :+ chr,   Seq(),          suffix.tail)
  }

  def maskCC(str: String): String = maskCC("", Seq(), str)
}

object mask extends App {
  val reader = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
  Iterator.continually(reader.readLine).takeWhile(null ne).foreach(line =>
    println(Masker.maskCC(line)))
}

mask main Array()
