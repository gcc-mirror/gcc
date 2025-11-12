#! /usr/bin/awk -f

/^UNIMPLEMENTED/ {
  exit
}

/^DESCRIPTION/ {
  exit
}

/struct sched_param {$/ {
  exit
}

/SYNOPSIS/,/DESCRIPTION/ {
  if( /([.][.]|[{},;]) *$/ ) {
    print
  }
}
