package mill.contrib.gitlab

import upickle.default._

/**
 * Actual headers to inject to http requests to gitlab.
 *
 * @param headers header -> value pairs
 */
case class GitlabAuthHeaders(headers: Seq[(String, String)])

object GitlabAuthHeaders {
  implicit val rw: ReadWriter[GitlabAuthHeaders] = macroRW

  def apply(header: String, value: String): GitlabAuthHeaders =
    GitlabAuthHeaders(Seq(header -> value))

  def privateToken(token: String): GitlabAuthHeaders = GitlabAuthHeaders("Private-Token", token)
  def deployToken(token: String): GitlabAuthHeaders = GitlabAuthHeaders("Deploy-Token", token)
  def jobToken(token: String): GitlabAuthHeaders = GitlabAuthHeaders("Job-Token", token)
}
