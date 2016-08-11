package utils

import java.net.URI
import javax.inject.Inject

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Maven @Inject() (git: Git) (implicit ec: ExecutionContext) {

  def convertNpmBowerDependenciesToMaven(dependencies: Map[String, String])(validatedNameFuture: (String, String, String) => Future[String]): Future[Map[String, String]] = {

    sealed trait Dependency {
      val providedName: String
      val legalNameFuture: Future[String]
    }

    sealed trait Version {
      val providedVersion: String
      val semVersionMaybe: Option[String] = SemVer.convertSemVerToMaven(providedVersion)
    }

    case class BasicDependency(providedName: String, providedVersion: String) extends Dependency with Version {
      val legalNameFuture: Future[String] = artifactId(providedName)
    }

    case class GitDependency(providedName: String, fullGitUrl: String) extends Dependency {

      val gitUrlParts = fullGitUrl.split("#")

      val gitUrlFuture: Future[String] = {
        gitUrlParts.headOption.fold(Future.failed[String](new Exception(s"Could not parse $fullGitUrl")))(Future.successful).flatMap(git.gitUrl)
      }

      val maybeVersion: Option[String] = {
        if (gitUrlParts.size == 2) {
          Some(gitUrlParts(1).stripPrefix("v"))
        }
        else {
          None
        }
      }

      lazy val gitVersions: Future[Seq[String]] = {
        gitUrlFuture.flatMap { gitUrl =>
          git.versions(gitUrl)
        }
      }

      lazy val gitBranches: Future[Seq[String]] = {
        gitUrlFuture.flatMap { gitUrl =>
          git.branches(gitUrl)
        }
      }

      val latestVersion: Future[String] = {
        gitUrlFuture.flatMap { gitUrl =>
          maybeVersion.fold {
            gitVersions.flatMap { versions =>
              versions.headOption.fold {
                // could not get a tagged version so the latest commit instead
                git.versionsOnBranch(gitUrl, "master").flatMap { commits =>
                  commits.headOption.fold {
                    Future.failed[String](new Exception(s"The dependency definition for $gitUrl was not valid because it looked like a git repo reference but no version could be determined."))
                  } { latestCommit =>
                    Future.successful(latestCommit)
                  }
                }
              } { latestVersion =>
                Future.successful(latestVersion)
              }
            }
          }(Future.successful)
        }
      }

      // does the specified named artifact have the same Git repo url was provided? If so, use the actual name instead of creating one from the URL.
      val legalNameFuture: Future[String] = {
        gitUrlFuture.flatMap { gitUrl =>
          latestVersion.flatMap { version =>
            validatedNameFuture(providedName, version, gitUrl).recoverWith {
              case e: Exception => artifactId(gitUrl)
            }
          }
        }
      }

      def toBasicDependency: Future[BasicDependency] = {
        legalNameFuture.flatMap { legalName =>
          val versionFuture = maybeVersion.fold(Future.successful("")) { providedVersion =>
            // if the version was specified then see if it is a branch.  If so, set it to an empty version because we can't deal with branch version deps.
            gitBranches.filter(_.contains(providedVersion)).map(_ => "").recover {
              case e: Exception => providedVersion
            }
          }
          versionFuture.map { version =>
            BasicDependency(legalName, version)
          }
        }
      }

    }

    val basicDependenciesFutures = dependencies.map { case (name, versionOrUrl) =>
      val basicDependencyFuture = if (git.isGit(versionOrUrl)) {
        GitDependency(name, versionOrUrl).toBasicDependency
      }
      else {
        Future.successful(BasicDependency(name, versionOrUrl))
      }

      basicDependencyFuture.flatMap { basicDependency =>
        basicDependency.semVersionMaybe.fold {
          Future.failed[(String, String)](new Exception(s"Could not convert NPM / Bower version to Maven for: ${basicDependency.providedName} ${basicDependency.providedVersion}"))
        } { mavenVersion =>
          basicDependency.legalNameFuture.map(_ -> mavenVersion)
        }
      }
    }

    Future.sequence(basicDependenciesFutures).map(_.toMap)
  }

  def artifactId(nameOrUrlish: String): Future[String] = {
    if (git.isGit(nameOrUrlish)) {
      git.gitUrl(nameOrUrlish).flatMap { gitUrl =>
        Future.fromTry {
          Try {
            val uri = new URI(gitUrl.stripSuffix(".git"))

            val host = uri.getHost.replaceAll("[^\\w\\d]", "-")
            val path = uri.getPath.replaceAll("[^\\w\\d]", "-")

            host + path
          }
        }
      }
    }
    else {
      val encoded = nameOrUrlish.replaceAllLiterally("@", "").replaceAllLiterally("/", "__")
      Future.successful(encoded)
    }
  }

}
