package mill.contrib.gitlab

import coursier.core.Authentication
import coursier.maven.MavenRepository
import mill.api.Result
import mill.api.Result.{Failure, Success}
import mill.define.Task
import mill.define.Task.T

trait GitlabMavenRepository {

  def tokenLookup: GitlabTokenLookup = new GitlabTokenLookup {} // For token discovery
  def gitlabRepository: GitlabPackageRepository // For package discovery

  def mavenRepository: T[MavenRepository] = T {

    val gitlabAuth = tokenLookup.resolveGitlabToken(T.env, sys.props.toMap, T.workspace)
      .map(auth => Authentication(auth.headers))
      .map(auth => MavenRepository(gitlabRepository.url(), Some(auth)))

    gitlabAuth match {
      case Result.Failure(msg) =>
        Failure(
          s"Token lookup for PACKAGE repository ($gitlabRepository) failed with $msg"
        ): Result[MavenRepository]
      case Result.Success(value) => Success(value)
    }
  }
}
