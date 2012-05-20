Git submodule helper scripts & gui
==================================

A collection of scripts that should help make life with git submodules
easier. They are aimed at the case with a central repository. The
submodules are thought not only as quasi static repositories, but as
something where active development happens. The scripts support two levels
of submodules, but most scripts do not support more than that.

gitR is a simple graphical interface that should make working with
submodules even easier. It only covers the basic functionality
required for an average user. The main advantage in working with the
graphical interface lies in the way the status of the repository is
presented: the user always sees the full status of the repository and
the submodules without the need to actively query for it.

How to use
----------

### The standard (one-person-working) work flow is as follows:

* Clone the repository using `git rclone`.
* Work and commit in the repository and its submodules as if there was
  nothing special about them.
* Prepare repository to be published by using `git rcommit -am "msg"`.
* Publish your work to central server using `git rpush`.

### Using tags (and branches):

* Create tags in the super repository using `git tag` as usual.
* Switch to tags using `git rcheckout` in the super repository.

The same can be done for branches - at least in theory. The scripts were
not developed with this case in mind. Note that there are no scripts that
help with the creation or merging of branches for all submodules or
something similar (on purpose, how would you do that?). The scripts will
just try to attach the HEAD to the correct branch if the submodules feature
different branches.

### Working together

Then there are a couple of scripts that help to recover from conflicts that
will probably arise when working simultaneously at the same super
repository. 

* Pull changes in super repository using `git rpull`. This will also update
  submodules, if necessary.
* Check submodules for updates using `git rfetch --dry-run` and fetch them
  using `git rfetch`.
* Get a diff like master..origin for all submodules using `git rdiff`.
* Then incorporate the updates with `git pull` in the submodules.
* If the branches have diverged, e.g., when there are new local commits and
  in the central repository as well, use `git converge-submodules` to get
  rid of submodule conflicts in super repositories. (Note that if there are
  submodules on two levels, the command probably needs to be run only for
  the first level submodules. The super repository will resolve the
  conflict automatically when rcommitting.)

See also `tests.sh` for an example work flow on how to recover from
conflicts. 

Main scripts
------------

Commands that start with an "r" (for "recursive") followed by some familiar
git command name are do just what they should do, but with some added
functionality that should simplify the work with submodules.

* `git-rclone`: clone repository and avoid detached head state ("attach
  heads").
* `git-rpush`: push super and sub repositories starting with the innermost
  submodules.
* `git-rpull`: pull super repository and update submodules, initializes
  added and removes removes submodules. Attaches head if possible.
* `git-rcheckout`: checkout super repository and update submodules,
  initializes added and removes removes submodules. Attaches head if
  possible. 
* `git-rcommit`: runs the same commit command starting with the innermost
  submodules. This is thought to be used with -a so one can quickly create
  commits to update all pointers to submodules.
* `git-rfetch`: does a `git fetch` for the super repository and all
  submodules. Accepts the argument `--dry-run` to check for updates without
  really downloading them.
* `git-rdiff`: show the differences between local and remote rep. Thought
  to help after a 'git rfetch`. Does show differences only for regular
  files, not submodule pointers.
* `git-converge-submodules`: to resolve a situation where local and
  remote branches have diverged, "if divergent force convergence". 
  Basically run a `git pull` (only if necessary). Does a little more if just
  submodule pointers are involved, like removing clean submodules.
* `git-rm-submodule`: remove a submodule in git's config and in the working
  copy. Commits the removal of the submodule if the argument `--no-commit`
  is not given. 
* `git-mv-submodule`: move a submodule, also has a `--no-commit` argument.
* `git-fix-submodules`: script to fix up the submodules in a repository.

Helper Scripts
--------------

Some commands that are just thought to be used internally.

* `git-ccomit`: conditional commit. Only commit if there is something to
  commit. Warns if there are unstaged changes to tracked files. Runs 
  `git converge-submodules` afterwards to avoid diverged branches.
* `git-cpush`: conditional push: only push if the local branch is ahead.
* `git-rm-orphaned-submodule-dirs`: remove orphaned submodule directories.
* `git-attach-head`: if a repository is in detached head state, but the
  HEAD just points to a tip of a branch, check out this branch. If
  necessary, the local branch is fast forwarded. Otherwise we stay in the
  detached head state.
* `git-check-branch`: checks if the current checked out branch is a remote
  tracking branch. Fails if not or if rep. is in detached head state.
* `git-check-clean`: checks if a repository is clean and if not reports in
  what way: changes to submodules, staged, unstaged or unmerged changes to
  tracked files and untracked files. Either checks for all of them or only
  for the given arguments: `--unmerged`, `--unstaged`, `--uncommitted`,
  `--untracked`. Returns exit code if `--exit-code` is given. The argument
  `--ignore-submodules` is the same as for `git status`. 
* `git-check-unpushed`: checks for any unpushed remote tracking branch.
* `git-check-non-tracking`: checks if there are any non-tracking branches.
* `git-bfetch`: calls git fetch, but shows only output for the currently
  checked out branch.

Other Stuff
-----------

* The script `tests.sh` can be used to test the scripts.
* The script `gen-man.sh` can be used to generate man pages.
* Update hook `hooks/update` can be used to prevent pushing of commits 
  containing references to submodule commits which have not been pushed.

Installation
------------

The scripts can be placed anywhere in `$PATH`, e.g., `/usr/local/bin`.

gitR requires an current R installation (http://www.r-project.org) and
requires the R-package (contained in gitR-package/) to be installed.
Before installing the package, make sure to have installed the R-packages
* gWidgets,
* gWidgetsRGtk2,
* gtools,
* RGtk2.
For example by running the R-command: 
`install.packages(c("gWidgetsRGtk2", "gtools"), dependencies="Depends")`
After that, the gitR-package can be installed by running
`R CMD INSTALL gitR-package`
in the root of this repository.

Known Problems
--------------

* Scripts should be called in the same directory as where the .git and
  .gitmodules files lie. Otherwise the scripts might incorrectly assume
  that there are not submodules.
* Scripts are slow. 
* Scripts of master branch do not work on OS X, this because `xargs` there
  does not allow for an -r argument (use `no xargs`-branch).
* When invoking `git rcommit` without giving the message, the shell somehow
  breaks afterwards and has to be restarted.
* The scripts work fine for git versions 1.7.6.1, 1.7.7.3 and 1.7.10.1, but
  there are some known problems with 1.7.8.4 and 1.7.9.4.

Copyright/License
-----------------

License: GPL v2 Copyright (c) 2012 Manuel Koller

