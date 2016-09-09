package renesca

trait NonBacktickName {
  def name: String
  require(name.matches("^[^`]*$"), "Backticks are not allowed in label names")

  override def toString = name
}
