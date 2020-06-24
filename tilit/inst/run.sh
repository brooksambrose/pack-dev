#! /usr/bash
function pgrep_live {
  pids=$(pgrep "$1");
  [ "$pids" ] || return;
  ps -o s= -o pid= "$pids" | sed -n 's/^[^ZT][[:space:]]\+//p';
}

pgrep_live cpumemlog | if [ ! $? ] ; then "/Library/Frameworks/R.framework/Versions/3.6/Resources/library/tilit/cpumemlog" 1 & sleep 5 ; fi
