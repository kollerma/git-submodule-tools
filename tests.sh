#!/bin/bash

## stop on error
set -e

## set current directory as working directory
wd=`pwd`

## create master (bare) clones for submodules
mkdir remote
git clone --bare git@github.com:kollerma/r-tutorial.git remote/r-tutorial.git
git clone --bare git@github.com:kollerma/aufgabe1.git remote/aufgabe1.git
git clone --bare git@github.com:kollerma/aufgabe2.git remote/aufgabe2.git
git clone --bare git@github.com:kollerma/aufgabe3.git remote/aufgabe3.git

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
)

## now work in vorlesung3 without pulling first
(cd vorlesung3
    ## and force a conflict like this...
)