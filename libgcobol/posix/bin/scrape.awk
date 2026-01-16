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

# Print lines that end in dots, a comma, a brace, or a semicolon.
/SYNOPSIS/,/DESCRIPTION/ {
  if( /([.][.]|[{},;]) *$/ ) {
    print
  }
}
