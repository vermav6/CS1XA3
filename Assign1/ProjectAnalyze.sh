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

grep -r  --exclude="todo.log" --exclude="changes.log" --exclude="ProjectAnanlyze.sh" --exclude="error.log" "#TODO" >> todo.log
#4::
#Checks all haskell files for syntax errors and puts the results into error.log 

ghc -fno-code "*.hs" &>> error.log  # used the hint as given in the info..
