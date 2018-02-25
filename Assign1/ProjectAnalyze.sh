#!/bin/bash
# 1::
#To check if the local repo along with the remote repo is up to date


$(git fetch origin)   # This is used to fetch commits from the remote repository

# Using this command as a resolver(to SHA1) of revision specifiers, as:
localRepo=$(git rev-parse HEAD)
remoteRepo=$(git rev-parse @{u})

if [ $localRepo == $remoteRepo ] 
then 
	echo "LOCAL REPO is UP TO DATE with REMOTE REPO"
else
	echo "LOCAL REPO is *NOT* UP TO DATE with REMOTE REPO"
fi

# 2::
# All the uncommited changes in a file changes.log

git diff  >> changes.log
echo "All the uncommited changes are there in changes.log"


#3:: 
# Puts each line from every file of your project with the tag #TODO into a file todo.log

grep -r  --exclude="todo.log" --exclude="changes.log" --exclude="ProjectAnalyze.sh" --exclude="error.log" "#TODO" >> todo.log

#4::
#Checks all haskell files for syntax errors and puts the results into error.log 

ghc -fno-code "*.hs" &>> error.log  # used the hint as given in the info..

#5::
# Extra feature

# Counting the number of lines and number of words in a list of files
            

number_oflines () {               # Defined a function "number_oflines".
  local file=$1   #  $1 is the first argument passed
  l=`wc -l $file | sed 's/^\([0-9]*\).*$/\1/'`   # for line count
  w=`wc -w $file | sed 's/^\([0-9]*\).*$/\1/'`   # for word count
}

if [ $# -lt 1 ]  #  $# is the total number of arguments passed in the script
then
  echo "Usage: $0 file ..."  #  $0 is the name of the argument passed
  echo "You need to add a file..."
  exit 1  # 1 is a good status to exit
fi

echo "$0 gives us the total number of LINES and WORDS in EACH file as well as in TOTAL"
l=0
w=0
a=0
b=0
c=0
while [ "$*" != ""  ]   #  $* is like $1 as it gives all the arguments passed, the loop control variable is important here.
do
        number_oflines $1
        echo "$1: contains $l lines and $w words"
        echo size: $(du -k "$1" | cut -f 1)
        a=$[ $a + 1 ]   # we can have any number of files
        b=$[ $b + $l ]
        c=$[ $c + $w ]
        shift   # Built-in command which removes arguments in the beginning of the argument list
done
if [ $a = 1 ]
then
        echo "You passed only $a file, which contains $b lines and $c words"
else
        echo "You passed $a files in total, which contains $b lines and $c words in total"
fi
