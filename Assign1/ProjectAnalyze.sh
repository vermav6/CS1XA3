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

grep -r --exclude=todo.log "#TODO" $1 >> todo.log

#4::
#Checks all haskell files for syntax errors and puts the results into error.log 

ghc -fno-code "*.hs" &>> error.log  # used the hint as given in the info..

#5::
# Extra feature

# Counting the number of lines and number of words in a list of files
         
file_features () {               # Defined a function "file_features".
  local file=$1   #  $1 is the first argument passed
  lines=`wc -l $file | sed "s/^\([0-9]*\).*$/\1/"`   # For line count, used piping which redirects the output of a file into searching and mutuating.
  words=`wc -w $file | sed "s/^\([0-9]*\).*$/\1/"`   # Simalarly, for word count aswell
}

if [ $# -le 0 ]  #  $# is the total number of arguments passed in the script
then
  echo "You need to add a file"
  echo "$0 file(nothing) :: input a file to see the following features "  #  $0 is the name of the argument passed
 
  exit 0  # Here 0 is the exit status
fi

echo "$0 gives us the total number of LINES and WORDS in EACH file as well as in TOTAL along with the SIZE of each file"
lines=0 ; words=0 ; ran_a=0 ; ran_b=0 ; ran_c=0   #Initializing the varibles
while [ "$*" != ""  ]   #  $* is like $1 but as it gives all the arguments passed, the loop control variable is important here
                        # Could use $@ aswell but better with $* as @ instead of * passes multiple arguments in once
do
        file_features $1
        echo "$1: contains $lines lines and $words words"
        # Gives the size of each file
        echo size: $(du -h "$1" | cut -f 1)

        ran_a=$[ $ran_a + 1 ] ; ran_b=$[ $ran_b + $words ] ; ran_c=$[ $ran_c + $lines ]  

        shift   # Built-in command which removes arguments in the beginning of the argument list
done

if [ $ran_a = 1 ]
then
        echo "You passed only $ran_a file, which contains $ran_c lines and $ran_b words"
else
        echo "You passed $ran_a files in total, which contains $ran_c lines and $ran_b words in total"
fi
