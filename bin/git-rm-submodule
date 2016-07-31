#!/bin/bash -e
## Remove the submodule
## Usage: git rm-submodule [ --no-commit ] dir
## does not commit if --no-commit is given 

## read input, display help if necessary
if [[ "$@" == "" || "$@" == *--help* ]]; then
    cat<<EOF
 Remove a submodule

 This command removes the submodule <directory> and creates a new commit
 with a simple commit message including the url of the removed
 submodule.

 Usage:
    git rm-submodule [--no-commit] <directory>

    --no-commit: do not commit the result
EOF
    exit 0;
fi

if [ "$1" == "--no-commit" ]; then
    nocommit=1
    dir=$2
else 
    dir=$1
fi

## from the git mailinglist:
function git
{
    LC_MESSAGES=C command git "$@"
} 
export git

## check if this is a valid submodule
if [[ ! "$dir" || $(git ls-files --error-unmatch --stage -- "$dir" | grep -E '^160000') == "" ]]; then
    echo >&2 "Error: \"$dir\" is not a submodule."
    exit 1
fi

## get the full path of the submodule
dir=$(git ls-files --full-name "$dir")
## ensure that we are in the toplevel directory
cdup=$(git rev-parse --show-toplevel) &&
cd "$cdup" || {
    echo >&2 "Cannot chdir to $cdup, the toplevel of the working tree"
    exit 1
}

cd "$dir"
## check if submodule is clean
git check-clean || exit 1
## check for unpushed changes
git check-unpushed || exit 1
## check for local non-tracking-branches
git check-non-tracking || exit 1
## find the real git-dir (might be git-file, for v.1.7.8 compatibility)
## git dir should be relative to current dir... but is it?
gitdir=$(cd `git rev-parse --git-dir`; pwd)
cd "$cdup"

## seems we're safe, so start removing
## get submodule url
url=`(git config --get submodule."$dir".url) || echo "unknown"`
## remove config entries
(git config -f .gitmodules --remove-section submodule."$dir" 2>/dev/null) || echo -n ""
(git add .gitmodules 2>/dev/null) || echo -n ""
(git config --remove-section submodule."$dir" 2>/dev/null) || echo -n ""
git rm --cached "$dir"
rm -rf "$dir"
## remove git dir as well (might git-file, for v.1.7.8 compatibility)
rm -rf "$gitdir"
## commit changes
if [[ ! "$nocommit" ]]; then
    git add .gitmodules
    git commit -m "removed submodule \"$dir\" (url: $url)"
    ## TODO: commit in super repositories of this one also??
fi

