# hgithub - Haskell bindings to the GitHub API

The library is in early stage. Currently only the following methods are
available:

  - list your repositories (i.e. GET /user/repos)
  - create a repository (i.e. POST /user/repos)

A command-line tool (also called `hgithub`) exposes the methods. It needs a
`github-username-password.txt` file in the current directory with your GitHub
username:password.
