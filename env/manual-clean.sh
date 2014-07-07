#!/bin/bash -e

modules=". core"

toplevel=".idea .idea_modules"
moduleSubdirs="target project/project project/target"

curr=$(pwd -P)
cd $(cd -P -- "$( dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
cd ..

echo "Manually cleaning this project..."
echo "  Removing the following directories:"

find . -name ".DS_Store" -exec rm -rf {} \;

for dir in $toplevel ; do
  test -d "$dir" && {
    echo "    $(cd "$dir" && pwd -P)"
    rm -rf "$dir"
  } || true
done

for module in $modules ; do
  for subdir in $moduleSubdirs ; do
    dir=$module/$subdir
    test -d "$dir" && {
      echo "    $(cd "$dir" && pwd -P)"
      rm -rf "$dir"
    } || true
  done
done

cd "$curr"

