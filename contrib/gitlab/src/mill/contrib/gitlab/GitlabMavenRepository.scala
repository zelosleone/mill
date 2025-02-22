package mill.contrib.gitlab

import coursier.core.Authentication
import coursier.maven.MavenRepository
import mill.api.Result
import mill.api.Result.{Failure, Success}
import mill.define.Task

trait GitlabMavenRepository {

  def tokenLookup: GitlabTokenLookup = new GitlabTokenLookup {} // For token discovery
  def gitlabRepository: GitlabPackageRepository // For package discovery

  def mavenRepository: Task[MavenRepository] = Task.task {
    tokenLookup.resolveGitlabToken(Task.env, sys.props.toMap, Task.workspace) match {
      case Result.Failure(msg) =>
        throw new Exception(s"Token lookup for PACKAGE repository ($gitlabRepository) failed with $msg")
      case Result.Success(headers) =>
        val auth = Authentication(headers.headers)
        MavenRepository(gitlabRepository.url(), Some(auth))
    }
  }
}
