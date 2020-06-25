#! /usr/bash
function pgrep_live {
  pids=$(pgrep "$1");
  [ "$pids" ] || return;
  ps -o s= -o pid= "$pids" | sed -n 's/^[^ZT][[:space:]]\+//p';
}

pgrep_live cpumemlog | if [ $? -eq 0 ] ; then $(Rscript -e 'cat(system.file("cpumemlog",package="tilit",mustWork=T))') 1 & sleep 5 ; fi
