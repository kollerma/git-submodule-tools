#!/bin/bash -e
## Generate man pages for git commands in this directory.
## just start this script in the directory of the commands
## and it will create a folder man/man1 and put the generated
## manpages there.

## call script --help
## parse help
## write man page

## get current date
date=`date +"%B %d, %Y"`
## mk man/man1 dir
mkdir -p man/man1

for CMD in git-*;
do
    man="man/man1/$CMD.1"
    name=
    desc=
    synopsis=
    options=
    usage=0
    echo "Creating man page for $CMD..."
    help=`$CMD --help`
    while IFS="" read line; do
	## remove one leading whitespace
	line=${line# }
	## remove trailing whitespaces
	line=${line%% }
	## remove any leading whitespaces
	cline=`echo $line`
	if [[ "$usage" -eq 3 ]]; then
	    ## we're in options
	    if [[ "${cline:1:1}" == "-" ]]; then
	    options="$options
$cline"
	    else
		options="$options $cline"
	    fi
	elif [[ "$usage" -eq 2 ]]; then
	    ## we're in Synopsis
	    if [[ "$cline" == "" ]]; then
		usage=3
	    else
		synopsis="$synopsis $cline"
	    fi
	elif [[ "$usage" -eq 1 ]]; then
	    ## we're in Description
	    if expr "$line" : 'Usage:' > /dev/null; then
		usage=2
	    elif [[ "$cline" != "" ]]; then
		if [[ "$cline" == "$line" ]]; then
		    desc="$desc 
$line"
		else
		    desc="$desc
$line
.br"
		fi
	    fi
	else 
	    ## we're in Name
	    if [[ "$cline" == "" ]]; then
		usage=1
	    else
		name="$name $cline"
	    fi
	fi	    
    done <<< "$help"

    ## remove leading whitespace, TODO: quote dashes
    synopsis=${synopsis:1}
    options=${options:1}
    desc=${desc:1}

    ## remove old version if available
    if [ -f "$man.gz" ]; then
	rm "$man.gz"
    fi

    ## write man page
    echo ".TH $CMD 1 \"$date\"" > "$man"
    echo ".SH NAME" >> "$man"
    echo "$CMD -$name" >> "$man"
    echo ".SH SYNOPSIS" >> "$man"
    echo ".B $synopsis" >> "$man"
    echo ".SH DESCRIPTION" >> "$man"
    echo "$desc" >> "$man"
    if [[ "$options" != "" ]]; then
	echo ".SH OPTIONS" >> "$man"
	while read line; do
	    echo ".TP" >> "$man"
	    echo "${line%% *}" >> "$man"
	    echo "${line#* }" >> "$man"
	done <<< "$options"
    fi
    echo ".SH AUTHOR" >> "$man"
    echo "Manuel Koller" >> "$man"
    
    ## gzip it
    gzip "$man"
done

