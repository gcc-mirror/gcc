#!/usr/bin/perl
#
# usage:
#
# $PERL gen-upc-coll-reduce.pl upc_coll_all_reduce.in >
#                              upc_coll_all_reduce.upc
#
# $PERL gen-upc-coll-reduce.pl upc_coll_all_prefix_reduce.in >
#                              upc_coll_all_prefix_reduce.upc
#
# This script reads 'upc_coll_reduce.in' as a template file,
# and generates a new source file, upc_coll_reduce.upc,
# which customizes the "upc_all_reduce_GENERIC" for each
# data type that the procedure operates on.  The following
# steps are performed:
# 1. GENERIC is replaced with a one/two character type suffix
#    where the suffix encodes the type that the procedure
#    operates on.
# 2. _UPC_RED_T is replaced with the full C type name
#    of the type that the procedure operates on.
# 3. For floating point types, the code between
#    "#ifndef _UPC_NONINT_T" and "#endif" is removed.
#
use strict;
use warnings;
my @type_config = (
    ['signed char', 'C', 1],
    ['unsigned char', 'UC', 1],
    ['signed short', 'S', 1],
    ['unsigned short', 'US', 1],
    ['signed int', 'I', 1],
    ['unsigned int', 'UI', 1],
    ['signed long', 'L', 1],
    ['unsigned long', 'UL', 1],
    ['float', 'F', 0],
    ['double', 'D', 0],
    ['long double', 'LD', 0]
  );
my $src;
{
  local $/ = undef;
  $src = <>;
}
my ($hdr,$body) =
   ($src =~ /(.*)(^void upc_all(?:_prefix)?_reduce_GENERIC.*)/ms);
print $hdr;
$body = "\n$body";
for my $t (@type_config) {
  my ($name, $chars, $is_int) = @$t;
  my $out = $body;
  for ($out) {
    s/(^void upc_all(?:_prefix)?_reduce)_GENERIC/$1$chars/sm;
    s/_UPC_RED_T/$name/smg;
    if ($is_int)
      {
        s/^#ifndef\s+_UPC_NONINT_T.*?\n(.*?)^#endif.*?\n/$1/smg;
      }
    else
      {
        s/^#ifndef\s+_UPC_NONINT_T.*?\n.*?^#endif.*?\n//smg;
      }
  }
  print $out;
}
