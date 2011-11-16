object Masker {
  def digits(chrs: Seq[Char]): Seq[Int] = chrs.map(_ - 48)

  def luhnCheck(digits: Seq[Int]): Boolean = {
    digits.reverse.zipWithIndex.map { case (digit, index) =>
      if (index % 2 == 1) digit * 2 else digit
    }.flatMap { num =>
      if (num < 10) Seq(num) else Seq(1, num % 10)
    }.sum % 10 == 0
  }

  def checkCC(number: Seq[Char]): Boolean = {
    val ds = digits(number.filter(_.isDigit))
    ds.size >= 14 && ds.size <= 16 && luhnCheck(ds)
  }

  def isCCDigit(chr: Char) = chr.isDigit || chr == '-' || chr == ' '

  @scala.annotation.tailrec
  def maskCC(prefix: String, number: String, suffix: String, lastHit: Option[(String, String)]): String = {
    val numDigits = number.filter(_.isDigit).size
    val hit = (if (checkCC(number)) Some(number -> suffix.take(16 - numDigits)) else None) orElse lastHit

    if (hit.isDefined && (numDigits >= 16 || suffix.isEmpty)) {
      val (num, peek) = hit.get
      return prefix ++ num.map(chr => if (chr.isDigit) 'X' else chr) ++ maskCC(peek ++ suffix)
    } else if (suffix.isEmpty) {
      return prefix ++ number
    }
    val chr = suffix.head

    if (numDigits >= 16)      maskCC(prefix :+ number.head,     number.tail,    suffix,       None)
    else if (isCCDigit(chr))  maskCC(prefix,                    number :+ chr,  suffix.tail,  hit)
    else                      maskCC(prefix ++ number :+ chr,   "",             suffix.tail,  None)
  }

  def maskCC(str: String): String = maskCC("", "", str, None)
}

val in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
Iterator.continually(in.readLine).takeWhile(null ne).foreach(line =>
  println(Masker maskCC line))
