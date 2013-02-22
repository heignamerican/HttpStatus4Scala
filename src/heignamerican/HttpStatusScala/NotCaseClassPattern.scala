package heignamerican.HttpStatusScala

object NotCaseClassPattern {
  def getAnswer(query: String): Seq[String] = {
    val list = HttpStatusList.get()

    query.matches("\\d+") match {
      case true => list.find { case (code, _) => code == query.toInt } match {
        case Some((_, reason)) => Seq(reason)
        case None => list.filter { case (code, _) => code.toString().startsWith(query) } map { case (code, reason) => "%s %s".format(code, reason) }
      }
      case false => list.filter { case (_, reason) => reason.contains(query) } map { case (code, reason) => "%s %s".format(code, reason) }
    }
  }
}
