package io.flow.util

import java.util.UUID

object IdGenerator {

  val PrefixLength = 3
  val Separator = "-"
}

/**
  * Generates a new unique ID for a resource. Main feature is to
  * prefix the unique id with a three character identifer to
  * understand how the ID is used (e.g. 'usr')
  *
  * @param prefix Global prefix to identify the type of resource for which you
  *         are generating an ID. Must be 3 characters, lowercase.
  */
case class IdGenerator(prefix: String) {
  assert(prefix.toLowerCase == prefix, s"prefix[$prefix] must be in lower case")
  assert(prefix.trim == prefix, s"prefix[$prefix] must be trimmed")
  assert(prefix.length == IdGenerator.PrefixLength, s"prefix[$prefix] must be ${IdGenerator.PrefixLength} characters long")
  assert(!BadWords.contains(prefix), s"prefix[$prefix] is on the black list and cannot be used")

  private[this] val idFormat = Seq("%s", "%s").mkString(IdGenerator.Separator)
  private[this] val idPattern = s"$prefix${IdGenerator.Separator}[0-9a-f]{32}".r

  def randomId(): String =
    fromUuid(UUID.randomUUID)

  def validate(id: String): Boolean =
    idPattern.matches(id)

  /**
   * Generates an uuid based on the specified bytes.
   * This is a pure function: the same input produces the same output.
   *
   *  {{{
   *  scala> val id1 = "exp-b476512e183944feb77126843ecb0271"
   *  scala> val id2 = "itm-a9308bbbf5c3431bb2fd7c72c92d9d62"
   *  scala> IdGenerator("tst").fromBytes((id1 + id2).getBytes)
   *  res: String = tst-54622b3b58be361c93522bb9be6468a3
   *  }}}
   */
  def fromBytes(bytes: Array[Byte]): String =
    fromUuid(UUID.nameUUIDFromBytes(bytes))

  private def fromUuid(uuid: UUID): String =
    idFormat.format(prefix, uuid.toString.replaceAll("\\-", ""))

}
