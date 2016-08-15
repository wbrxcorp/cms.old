package modules

import org.eclipse.jgit.api.Git

// JavaDoc: http://download.eclipse.org/jgit/site/4.4.1.201607150455-r/apidocs/index.html
// https://git-scm.com/book/ja/v2/Git%E3%82%92%E3%81%82%E3%81%AA%E3%81%9F%E3%81%AE%E3%82%A2%E3%83%97%E3%83%AA%E3%82%B1%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3%E3%81%AB%E7%B5%84%E3%81%BF%E8%BE%BC%E3%82%80-JGit
// http://shirobit.hatenablog.com/entry/2015/03/24/JGit%E3%81%AE%E4%BD%BF%E3%81%84%E6%96%B91_Git%E3%82%AF%E3%83%A9%E3%82%B9%E3%81%A8init


package object jgit {
  def getGit(repositoryPath:String):Git = {
    val repository = new org.eclipse.jgit.lib.RepositoryBuilder().setGitDir(new java.io.File(repositoryPath)).readEnvironment.findGitDir.build
    new Git(repository)
  }
}
