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

NEW_LINE="${LINE_TO_REPLACE/$OLD_VSN/$NEXT_VSN}"
sed -i "s~$LINE_TO_REPLACE~$NEW_LINE~g" src/erlwater.app.src

printf "Committing the updated files to the master branch\n"
git add .
git commit -m "Version bumped to $NEXT_VSN"
git push -u origin master

printf "Creating the local tag: $NEXT_VSN\n"
git tag -d $NEXT_VSN
git tag $NEXT_VSN

printf "Pushing the remote tag: $NEXT_VSN\n"

git push origin $NEXT_VSN

printf "Your release versioned '$NEXT_VSN' is successfully created!!\n"