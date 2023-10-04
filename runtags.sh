#!/usr/bin/env bash

set -e

ivy="/home/fgb/.cache/coursier/v1/"
source_dir=".tag_sources/"
tagfile="dep_tags"

function get_source_jar {
  jar_name="$(basename $1 .jar)-sources.jar"
  find $ivy -name $jar_name
}

project_deps=$(find lib_managed/ -name '*.jar')

for dep in $project_deps
do
  source=$(get_source_jar $dep)
  if [ -n "$source" ]; then
    unzip -qq -o "$source" -d "$source_dir"
  fi
done

ctags -R -f "$tagfile" --languages=scala,java "$source_dir"
