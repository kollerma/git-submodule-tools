#!/bin/bash

####
## config
####

## set current directory as working directory
wd=`pwd`

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
)
git clone --bare vorlesung remote/vorlesung.git
(cd vorlesung
    git remote add origin $wd/remote/vorlesung.git
    git tag state-0
    git push --tag
)

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
    )
    (cd serie1/aufgabe1
    echo "try to do it today" >> ex.Rnw
    )
    ## edit aufgabe2 but do commit
    (cd serie1/aufgabe2
    	echo "try again" >> ex.Rnw
    	git add ex.Rnw
    	git commit -m 'This is no problem'
    )
    ## rcommit without -a should warn of unstaged changes now
    git rcommit -m "I did something"
    ## ok, then add files that are already tracked
    git rcommit -am "I did something"
    ## Now push this
    git rpush
    ## Add a tag as well
    git tag state-1
    git push --tag
)

## pull this in vorlesung
(cd vorlesung
    git rpull
    ## This should report that there are updates available
    ## for serie2/aufgabe1 which points to the same rep. as
    ## serie1/aufgabe2.
)

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
    )
    (cd serie2/aufgabe1
	echo "we work against each other" >> ex.Rnw
    )
    ## try to pull now, should fail
    git rpull && exit 1 || echo "rpull failed, as required."
    ## try to push, should not fail, since there are no new commits
    git rpush || { echo "rpush failed, this shouldn't happen!!"; exit 1; }
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
    )
    ## This can be fast forwarded
    (cd serie1/aufgabe1
	git pull
    )
    ## This can be fast forwarded
    (cd serie1/aufgabe2
	git pull
    )
    (cd serie2/aufgabe1
	git pull && exit 1 || cat ex.Rnw | sed -e '7 d' -e '5 d' -e '3 d' > ex.Rnw
    )
    ## Now git rdiff should not show any changes
    git rdiff
    ## ok, commit then
    git rcommit -am 'merged remote'
    ## and push
    git rpush
)

## Now pull this in vorlesung
(cd vorlesung
    git rpull
    ## show differences
    git rdiff
    ## update serie1/aufgabe2
    (cd serie1/aufgabe2
	git pull
    )
    ## commit and push this
    git rcommit -am 'updated serie1/aufgabe2'
    git rpush
)

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
    )
    git rcommit -am 'testing stuff' && exit 1 || echo "rcommit failed as it should have"
    git rcheckout state-1 && exit 1 || echo "rcheckout failed as it should have"
    ## revert
    (cd r-tutorial
	git reset --hard HEAD
    )
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
	)
	git rm-submodule aufgabe2 && exit 1 || echo "rm-submodule caught non-tracking-branch"
	## so merge it to master and push
	(cd aufgabe2
	    git merge testbranch
	    git push
	)
	## now it should work
    	git rm-submodule aufgabe2
    )
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
)

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
    )
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
)
## make backup first
cp -r vorlesung3 vorlesung3a
## continue...
(cd vorlesung3
    ## update serie2
    (cd serie2
    	git converge-submodules
	[ ! -f aufgabe2/ex.Rnw ] || { echo "Error: serie2/aufgabe2 not removed"; exit 1; }
    )
    git rcommit -am 'updated serie1 and serie2'
    git rpush
    ## add tag and push
    git tag state-3
    git push --tags
)
## now try this again in vorlesung3a
## (slightly different scenario for converge-submodules)
cp -r vorlesung3a vorlesung3b
(cd vorlesung3a
    (cd serie2
    	git converge-submodules
	[ ! -f aufgabe2/ex.Rnw ] || { echo "Error: serie2/aufgabe2 not removed"; exit 1; }
    )
    ## now commit
    git rcommit -am 'updated serie1 and serie2'
)

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
    )
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
    )
    git rcommit -am 'resolved conflict in serie2'
    ## don't push, want to go even further...
)

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
    )
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
    )
    git rcommit -am 'resolved conflict in serie2'
    git rpush
)