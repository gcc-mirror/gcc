#!/bin/awk -f

# Copyright (C) 2000  Free Software Foundation

#    This file is part of libgcj.

# This software is copyrighted work licensed under the terms of the
# Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
# details.

# This script emulates a little of the functionality of addr2line for
# those systems that don't have it.  The only command line argument is
# an executable name.  The script reads hexadecimal addresses from
# stdin and prints the corresponding symbol names to stdout.  The
# addresses must begin with "0x" and be fully zero filled or this
# won't work.

BEGIN {
  object = ARGV[1];
  ARGV[1] = "";

  while ("nm " object "| sort" | getline) {
    if ($2 == "t" || $2 == "T") {
      address[i] = "0x" $1; name[i] = $3;
      i++;
    }
  }
  syms = i;
}

{
  lo = 0;
  hi = syms - 1;

  while ((hi-1) > lo)
    {
      try = int ((hi + lo) / 2);
      if ($0 < address[try])
	hi = try;
      else if ($0 >= address[try])
	lo = try;
    }
  print name[lo] "\n"; fflush();
}

    
