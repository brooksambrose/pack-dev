#! /usr/bash
function pgrep_live {
  pids=$(pgrep "$1");
  [ "$pids" ] || return;
  ps -o s= -o pid= ${pids} | sed -n 's/^[^ZT][[:space:]]\+//p';
} && if [ ! $(pgrep_live cpumemlog) ] ; then $(Rscript -e 'cat(system.file("cpumemlog",package="tilit",mustWork=T))') 1 ; fi
