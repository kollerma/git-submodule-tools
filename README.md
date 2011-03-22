Git submodule helper scripts
============================

A collection of scripts that should help make life with git submodules
easier. They are aimed at the case with a central repository. The
submodules are thought not only as quasi static repositories, but as
something where active development happens. The scripts support two levels
of submodules, but most scripts do not support more than that.

At the moment the scripts depend on `git-what-branch` by Seth
Robertson. https://github.com/SethRobertson/git-what-branch 
This dependency is likely to be removed soon.

Main scripts
------------

Commands that start with an "r" (for "recursive") followed by some familiar
git command name are do just what they should do, but with some added
functionality that should simplify the work with submodules.

* `git-rclone`: clone repository and avoid detached head state ("attach
  heads").
* `git-rpush`: push super- and subrepositories starting with the innermost
  submodules.
* `git-rpull`: pull super repository and update submodules, initializes
  added and removes removes submodules. Attaches head if possible.
* `git-rcheckout`: checkout super repository and update submodules, initializes
  added and removes removes submodules. Attaches head if possible.
* `git-rcommit`:
* `git-rdiff`:
* `git-rm-submodule`:
* `git-check-for-unpdates`:
* `git-converge-submodules`:

Helper Scripts
--------------

Some commands that are just thought to be used internally.

* `git-attach-head`:
* `git-ccomit`:
* `git-check-branch`:
* `git-check-clean`:
* `git-check-unpushed`:
* `git-cpush`:
* `git-ccommit`:

Other Stuff
-----------

The script `tests.sh` can be used to test the scripts.

Author
------

Manuel Koller, koller@stat.ethz.ch. Use at your own risk, etc...
