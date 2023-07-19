#!/bin/sh

MAIN="main"
LOCAL_BRANCH_NAME=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')

if [ "$CI_PIPELINE_SOURCE" = "merge_request_event" ]
then
    echo "MR from $LOCAL_BRANCH_NAME to $CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
    if [ "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" == "$MAIN" ]
    then
        echo "opam install . --deps-only --locked -y"
	    opam install . --deps-only --locked -y
    else
        echo "opam install . --deps-only -y"
	    opam install . --deps-only -y
    fi
else
    echo "Installing deps on $LOCAL_BRANCH_NAME"
    if [ "$LOCAL_BRANCH_NAME" = "$MAIN" ]
    then
        echo "opam install . --deps-only --locked -y"
	    opam install . --deps-only --locked -y
    else
        echo "opam install . --deps-only -y"
	    opam install . --deps-only -y
    fi
fi

