#!/bin/sh

# Check if ocamlformat exists
if ! command -v ocamlformat
then
   echo "Command 'ocamlformat' could not be found"
   exit 1
else
   echo "Command 'ocamlformat' found with version" $(ocamlformat --version)
fi

# Check if git exists
if ! command -v git
then
   echo "Command 'git' could not be found"
   exit 1
else
   echo "Command 'git' found with version" $(git --version)
fi

before_formatting=`git diff`
make format
after_formatting=`git diff`
fileonly=`git diff --name-only`

if [ "$after_formatting" = "$before_formatting" ] ; then
    echo "Success: all files are well indented"
    exit 0
else
    echo "Failure: following files are not well indented:"
    echo "$fileonly"
    exit 1
fi
