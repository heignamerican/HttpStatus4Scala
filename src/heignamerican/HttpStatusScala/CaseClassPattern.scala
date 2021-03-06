package heignamerican.HttpStatusScala

object CaseClassPattern {
  val list = HttpStatusList.get().map { case (code, reason) => Status(code, reason) }

  def getAnswer(query: String) = {
    query match {
      case "" =>
        list.map(_.toString())
      case x if x.matches("\\d+") =>
        list.find(_.code == query) match {
          case Some(x) => Seq(x.reason)
          case None => list.filter(_.code.startsWith(query)).map(_.toString())
        }
      case _ =>
        list.filter(_.reason.contains(query)).map(_.toString())
    }
  }

  case class Status(code: String, reason: String) {
    override def toString() = "%s %s".format(code, reason)
  }
}
