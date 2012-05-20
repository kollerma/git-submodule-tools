#!/bin/bash

####
## config
####

## set current directory as working directory
wd=`pwd`/test
if test -d $wd
then
  rm -rf $wd
fi
mkdir -p $wd

####
## setup
####

## stop on error
set -e

## switch to working dir
cd "$wd"

## create master (bare) clones for submodules
mkdir remote
git clone --bare http://github.com/kollerma/r-tutorial.git remote/r-tutorial.git
git clone --bare http://github.com/kollerma/aufgabe1.git remote/aufgabe1.git
git clone --bare http://github.com/kollerma/aufgabe2.git remote/aufgabe2.git
git clone --bare http://github.com/kollerma/aufgabe3.git remote/aufgabe3.git

## clone aufgabe2
git rclone remote/aufgabe2.git

## create super repositories
## serie x: 1-2
for i in 1 2; do
    mkdir serie$i
    cd serie$i
    git init
    touch series$i.tex
    touch solution$i.tex
    git add series$i.tex solution$i.tex
    git submodule add $wd/remote/aufgabe$i.git aufgabe1
    j=`expr $i + 1`
    git submodule add $wd/remote/aufgabe$j.git aufgabe2
    git commit -m 'first commit'
    cd ..
    git clone --bare serie$i remote/serie$i.git
    cd serie$i
    git remote add origin $wd/remote/serie$i.git
    ## to create tracking branches
    git push -u --all
    cd ..
done

## vorlesung
mkdir vorlesung
(cd vorlesung
    git init
    touch README
    git add README
    git submodule add $wd/remote/serie1.git serie1
    git submodule add $wd/remote/serie2.git serie2
    git submodule add $wd/remote/r-tutorial.git r-tutorial
    git commit -m 'first commit'
) || exit 1
git clone --bare vorlesung remote/vorlesung.git
(cd vorlesung
    git remote add origin $wd/remote/vorlesung.git
    git tag state-0
    git push --tag
) || exit 1

## clone vorlesung.git to vorlesung and vorlesung2 
rm -rf vorlesung
git rclone $wd/remote/vorlesung.git vorlesung
git rclone $wd/remote/vorlesung.git vorlesung2
git rclone $wd/remote/vorlesung.git vorlesung3

####
## start tests
####

## get some work done in vorlesung2
(cd vorlesung2
    (cd r-tutorial
	echo "some random content" >> tutorial.Rnw
    ) || exit 1
    (cd serie1/aufgabe1
    echo "try to do it today" >> ex.Rnw
    ) || exit 1
    ## edit aufgabe2 but do commit
    (cd serie1/aufgabe2
    	echo "try again" >> ex.Rnw
    	git add ex.Rnw
    	git commit -m 'This is no problem'
    ) || exit 1
    ## rcommit without -a should warn of unstaged changes now
    git rcommit -m "I did something"
    ## ok, then add files that are already tracked
    git rcommit -am "I did something"
    ## Now push this
    git rpush
    ## Add a tag as well
    git tag state-1
    git push --tag
) || exit 1

## pull this in vorlesung
(cd vorlesung
    git rpull
    ## This should report that there are updates available
    ## for serie2/aufgabe1 which points to the same rep. as
    ## serie1/aufgabe2.
) || exit 1

## clone vorlesung to vorlesung4
git rclone $wd/remote/vorlesung.git vorlesung4
## there is a detached head. do nothing...

####
## how to fix diverged branches
####

## now work in vorlesung3 without pulling first
## to force some conflicts
(cd vorlesung3
    ## rfetch --dry-run should report updates but not fetch them
    git rfetch --dry-run
    ## force a conflict
    (cd r-tutorial
	echo "some more content" >> tutorial.Rnw
    ) || exit 1
    (cd serie2/aufgabe1
	echo "we work against each other" >> ex.Rnw
    ) || exit 1
    ## try to pull now, should fail
    git rpull && exit 1 || echo "rpull failed, as required."
    ## try to push, should fail, since there are new commits
    git rpush && exit 1 || echo "rpush failed, as required"
    ## commit
    git rcommit -am "some message"
    ## now there's a conflict, try to push again
    git rpush && exit 1 || echo "rpush failed, as required."
    ## so try a pull as the message says
    ## and this should also report the differences
    git rpull && exit 1 || echo "rpull failed, as required."
    ## the way to go is to use rfetch
    git rfetch
    ## rpull and rpush should still fail
    git rpull && exit 1 || echo "rpull failed, as required."
    git rpush && exit 1 || echo "rpush failed, as required."
    ## do git rdiff to find differences
    git rdiff
    ## merge all differences
    (cd r-tutorial
	git pull && exit 1 || echo "corrected version" > tutorial.Rnw
    ) || exit 1
    ## This can be fast forwarded
    (cd serie1/aufgabe1
	git pull
    ) || exit 1
    ## This can be fast forwarded
    (cd serie1/aufgabe2
	git pull
    ) || exit 1
    (cd serie2/aufgabe1
	git pull && exit 1 || cat ex.Rnw | sed -e '7 d' -e '5 d' -e '3 d' > ex.Rnw
    ) || exit 1
    ## Now git rdiff should not show any changes
    git rdiff
    ## ok, commit then
    git rcommit -am 'merged remote'
    ## and push
    git rpush
    ## add an untracked file
    touch serie1/aufgabe2/blah.Rnw
    ## try to push, should not fail, since there only untracked files
    ## but there should be a warning.
    git rpush || { echo "rpush failed, this shouldn't happen!!"; exit 1; }
    ## remove the file again
    rm serie1/aufgabe2/blah.Rnw
) || exit 1

## Now pull this in vorlesung
(cd vorlesung
    git rpull
    ## show differences
    git rdiff
    ## update serie1/aufgabe2
    (cd serie1/aufgabe2
	git pull
    ) || exit 1
    ## commit and push this
    git rcommit -am 'updated serie1/aufgabe2'
    git rpush
) || exit 1

####
## try tag switching and removing submodules
###

## Now pull this in vorlesung2
(cd vorlesung2
    git rpull
    ## show differences
    git rdiff
    ## add tag and push it
    git tag state-2
    git push --tag
    ## list tags
    git tag
    ## checkout 
    git rcheckout state-1
    git rcheckout state-0
    ## try to do stuff
    (cd r-tutorial
	echo "testing..." >> tutorial.Rnw
    ) || exit 1
    git rcommit -am 'testing stuff' && exit 1 || echo "rcommit failed as it should have"
    git rcheckout state-1 && exit 1 || echo "rcheckout failed as it should have"
    ## revert
    (cd r-tutorial
	git reset --hard HEAD
    ) || exit 1
    git rcheckout state-2

    ## remove some submodules and switch tags
    (cd serie2
	## try to remove dirty submodule
	echo "test" > aufgabe2/hallo
    	git rm-submodule aufgabe2 && exit 1 || echo "rm-submodule failed as it should have"
	## try non tracking branch
	(cd aufgabe2
	    git checkout -b testbranch
	    git add hallo
	    git commit -m 'catch this!'
	    git checkout master
	) || exit 1
	git rm-submodule aufgabe2 && exit 1 || echo "rm-submodule caught non-tracking-branch"
	## so merge it to master and push
	(cd aufgabe2
	    git merge testbranch
	    git push
	) || exit 1
	## now it should work
    	git rm-submodule aufgabe2 || { echo "rm-submodule failed!"; exit 1; }
    ) || exit 1
    ## have to commit this in super repository by hand
    git rcommit -am 'removed submodule serie2/aufgabe2'
    ## revert to state-2
    git rcheckout state-2
    [ -f serie2/aufgabe2/ex.Rnw ] || { echo "Error: serie2/aufgabe2 not restored"; exit 1; }
    ## revert to newest state
    git rcheckout master
    [ ! -f serie2/aufgabe2/ex.Rnw ] || { echo "Error: serie2/aufgabe2 not removed"; exit 1; }
    ## now push this
    git rpush
) || exit 1

####
## cause conflict -- stage2: 
##  when submodules with local new commits were removed in remote
####

(cd vorlesung3
    ## check for updates, but ignore them
    git rfetch --dry-run
    (cd serie2/aufgabe2
	## there are already unpulled changes...
	git pull
    ) || exit 1
    ## should open an editor
    ## git rcommit -a
    git rcommit -am "playing with fire!!"
    ## now get updates
    git rpull && exit 1 || echo "This fails as it should"
    git rpush && exit 1 || echo "This pushes the second level, but then fails"
    ## get updates
    git rfetch --dry-run
    git rfetch
    git rdiff
) || exit 1
## make backup first
cp -r vorlesung3 vorlesung3a
## continue...
(cd vorlesung3
    ## update serie2
    (cd serie2
    	git converge-submodules
	[ ! -f aufgabe2/ex.Rnw ] || { echo "Error: serie2/aufgabe2 not removed"; exit 1; }
    ) || exit 1
    git rcommit -am 'updated serie1 and serie2'
    git rpush
    ## add tag and push
    git tag state-3
    git push --tags
) || exit 1
## now try this again in vorlesung3a
## (slightly different scenario for converge-submodules)
cp -r vorlesung3a vorlesung3b
(cd vorlesung3a
    (cd serie2
    	git converge-submodules
	[ ! -f aufgabe2/ex.Rnw ] || { echo "Error: serie2/aufgabe2 not removed"; exit 1; }
    ) || exit 1
    ## now commit
    git rcommit -am 'updated serie1 and serie2'
) || exit 1

####
## cause conflict -- stage3: when submodules have been locally added
####

(cd vorlesung2
    ## removed serie2/aufgabe2 earlier
    ## now add another aufgabe2 and try to push
    ## check for updates, but ignore this
    git rfetch --dry-run
    (cd serie2
	git submodule add $wd/remote/aufgabe1.git aufgabe2
	git commit -m 'added aufgabe1.git as aufgabe2'
    ) || exit 1
    git rcommit -am 'did some work in serie2'
    git rpush && exit 1 || echo "Ok, there was an error in serie2"
    ## ok, check for updates
    git rfetch
    ## so try the usual solution:
    (cd serie2
	git converge-submodules
	## ok, force it
	## this essentially forces the submodule add we just made
	git converge-submodules -f
    ) || exit 1
    git rcommit -am 'resolved conflict in serie2'
    ## don't push, want to go even further...
) || exit 1

####
## cause conflict -- stage4: when submodules will be removed in remote 
##                           but others already added in the same place
####

(cd vorlesung
    ## removed serie2/aufgabe2 earlier
    ## now add another aufgabe2 and try to push
    ## check for updates, but ignore this
    git rfetch --dry-run
    (cd serie2
	git rm-submodule aufgabe2
	git submodule add $wd/remote/aufgabe1.git aufgabe2
	git commit -m 'added aufgabe1.git as aufgabe2'
    ) || exit 1
    git rcommit -am 'did some work in serie2'
    git rpush && exit 1 || echo "Ok, there was an error in serie2"
    ## ok, check for updates
    git rfetch
    ## so try the usual solution:
    (cd serie2
	git converge-submodules
	## this will remove the submodule we just added
	## but give the info we need to add it again.
	## do so
	git submodule add $wd/remote/aufgabe1.git aufgabe2
    ) || exit 1
    git rcommit -am 'resolved conflict in serie2'
    git rpush
) || exit 1

####
## cause conflict -- stage 5: when submodules have been added twice
##                            in different versions 
####

(cd vorlesung3
    ## add aufgabe1.git as aufgabe2 again, aufgabe3.git as aufgabe3
    ## check for updates, but ignore them
    git rfetch --dry-run
    (cd serie2
	git submodule add $wd/remote/aufgabe1.git aufgabe2
	git submodule add $wd/remote/aufgabe3.git aufgabe3
	git commit -m 'added aufgabe1.git as aufgabe2, aufgabe3.git as aufgabe3'
    ) || exit 1
    git rcommit -am 'added some submodules in serie2'
) || exit 1

cp -r vorlesung3 vorlesung3c
(cd vorlesung3
    ## fetch updates
    git rfetch
    (cd serie2
	git converge-submodules
    ) || exit 1
    git rcommit -am 'converged submodules'
    git rpush
) || exit 1
    
####
## test submodule version conflict resolution
####

(cd vorlesung
    git rpull
    ## switch branch in serie2/aufgabe1
    (cd serie2/aufgabe1
	git branch alternative origin/alternative
	git checkout alternative
    ) || exit 1
    git rcommit -am 'switched branch in serie2/aufgabe1'
) || exit 1

(cd vorlesung3
    ## revert to an old commit in serie2/aufgabe1
    (cd serie2/aufgabe1
	git reset --hard HEAD~1
    ) || exit 1
    git rcommit -am 'reset version in serie2/aufgabe1'
    git rpush
) || exit 1

(cd vorlesung
    ## fetch updates
    git rfetch
    (cd serie2
	git converge-submodules
	## fails
    ) || exit 1
    git rcommit -am 'merged serie2'
    git rpush
) || exit 1

(cd vorlesung3
    ## pull
    git rpull
) || exit 1

####
## test mv-submodule
####

(cd vorlesung
    ## mv serie2 to serie3
    git mv-submodule serie1 serie2 && exit 1 || echo "Ok series2 exists already"
    git mv-submodule serie2 serie3

    [ ! -d serie2 ] || { echo "Error: serie2 not removed"; exit 1; }
    [ -d serie3 ] || { echo "Error: serie3 not created"; exit 1; }

    ## move it back
    git mv-submodule serie3 serie2

    [ ! -d serie3 ] || { echo "Error: serie3 not removed"; exit 1; }
    [ -d serie2 ] || { echo "Error: serie2 not created"; exit 1; }
) || exit 1

####
## test removal and adding of a submodule of the same name
####

## create a set of new submodules to be used 
## in the subsequent tests

(cd remote
    ## clone aufgabe1 into aufgabe4
    git clone --bare aufgabe1.git aufgabe4.git
) || exit 1

## add some commits to aufgabe1
git clone remote/aufgabe1.git aufgabe1
(cd aufgabe1
    echo more-content >> ex.Rnw
    git commit -am "first time adding more content to aufgabe1"
    echo even-more-content >> ex.Rnw
    git commit -am "second time adding more content to aufgabe1"
    git push
) || exit 1

## add some commits to aufgabe2
git clone remote/aufgabe4.git aufgabe4
(cd aufgabe4
    echo more-content2 >> ex.Rnw
    git commit -am "first time adding more content to aufgabe4"
    echo even-more-content2 >> ex.Rnw
    git commit -am "second time adding more content to aufgabe4"
    git push
) || exit 1

## create new repos vorlesung5 and vorlesung6
git rclone remote/vorlesung.git vorlesung5
git rclone remote/vorlesung.git vorlesung6

## update to newest version aufgabe1 in vorlesung5
(cd vorlesung5
    git rpull
    (cd serie1/aufgabe1
	git checkout master
    ) || exit 1
    git rcommit -am "updated series1/aufgabe1"
    ## the next push should be ok 
    ## (even if HEAD in serie2/aufgabe2 is not attached)
    git rpush
) || exit 1

## update to newest version aufgabe1 in vorlesung6
(cd vorlesung6
    git rpull
    (cd serie1
	git rm-submodule aufgabe1
	git submodule add $wd/remote/aufgabe4.git aufgabe1
    ) || exit 1
    git rcommit -am "added aufgabe4.git as aufgabe1"
    git rpush
) || exit 1

## go to vorlesung5 and pull again
(cd vorlesung5
    git rpull
    ## the head of serie1/aufgabe1 should be attached now
    (cd serie1/aufgabe1
	git branch -v -a
    ) || exit 1
) || exit 1
    
