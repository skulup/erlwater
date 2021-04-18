#!/bin/bash

APP_SRC=$(cat src/erlwater.app.src)
LINE_TO_REPLACE=$(cat src/erlwater.app.src | grep vsn)
OLD_VSN=$(echo -n $LINE_TO_REPLACE | cut -d'"' -f 2)
CURRENT_VSN=$(echo -n $OLD_VSN | sed 's/[^0-9]*//g')
CURRENT_VSN=$(echo -n $CURRENT_VSN | sed 's/^0*//')
NEXT_VSN_NUMBER=$((CURRENT_VSN + 1))
NEXT_VSN=$(printf "%03d" $NEXT_VSN_NUMBER)
NEXT_VSN=$(echo -n $NEXT_VSN | sed 's/\(.\{1\}\)/\1./g')
NEXT_VSN=${NEXT_VSN::-1}

printf "New version=$NEXT_VSN\n"

echo "$LINE_TO_REPLACE"
NEW_LINE="${LINE_TO_REPLACE/$OLD_VSN/$NEXT_VSN}"
sed -i "s~$LINE_TO_REPLACE~$NEW_LINE~g" src/erlwater.app.src

printf "Creating local tag: $NEXT_VSN"
git tag -d $NEXT_VSN
git add $NEXT_VSN

printf "Pushing the remote tag: $NEXT_VSN"

git push origin $NEXT_VSN