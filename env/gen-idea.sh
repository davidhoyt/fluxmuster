#!/bin/bash -e 

cd $(cd -P -- "$( dirname -- "${BASH_SOURCE[0]}")" && pwd -P)

#Remove all directories that are created by IntelliJ or SBT.
. ./manual-clean.sh

#Go to the top directory for this project.
cd ..

#Regenerate idea project files if you're using the sbt idea plugin.
echo "Regenerating IntelliJ project files..."
sbt clean gen-idea

