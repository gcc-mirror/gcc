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
# 4. _UPC_TO_PTL_TYPECVT is replaced with a proper define
#    to convert UPC to PTL data types.
#
use strict;
use warnings;
my @type_config = (
    ['signed char', 'C', 1, "UPC_COLL_TO_PTL_CHAR"],
    ['unsigned char', 'UC', 1, "UPC_COLL_TO_PTL_UCHAR"],
    ['signed short', 'S', 1, "UPC_COLL_TO_PTL_SHORT"],
    ['unsigned short', 'US', 1, "UPC_COLL_TO_PTL_USHORT"],
    ['signed int', 'I', 1, "UPC_COLL_TO_PTL_INT"],
    ['unsigned int', 'UI', 1, "UPC_COLL_TO_PTL_UINT"],
    ['signed long', 'L', 1, "UPC_COLL_TO_PTL_LONG"],
    ['unsigned long', 'UL', 1, "UPC_COLL_TO_PTL_ULONG"],
    ['float', 'F', 0, "UPC_COLL_TO_PTL_FLOAT"],
    ['double', 'D', 0, "UPC_COLL_TO_PTL_DOUBLE"],
    ['long double', 'LD', 0, "UPC_COLL_TO_PTL_LONG_DOUBLE"]
  );
my $src;
{
  local $/ = undef;
  $src = <>;
}
my ($hdr,$body) =
   ($src =~ /(.*)PREPROCESS_BEGIN(.*)/ms);
print $hdr;
for my $t (@type_config) {
  my ($name, $chars, $is_int, $data_type) = @$t;
  my $out = $body;
  for ($out) {
    s/_GENERIC/$chars/smg;
    s/_UPC_RED_T/$name/smg;
    s/_UPC_TO_PTL_TYPECVT/$data_type/smg;
    if ($is_int)
      {
        s/^#ifndef\s+_UPC_NONINT_T.*?\n(.*?)^#endif.*?\n/$1/smg;
        s/^#ifdef\s+_UPC_NONINT_T.*?\n(.*?)^#endif.*?\n//smg;
      }
    else
      {
        s/^#ifndef\s+_UPC_NONINT_T.*?\n.*?^#endif.*?\n//smg;
        s/^#ifdef\s+_UPC_NONINT_T.*?\n(.*?)^#endif.*?\n/$1/smg;
      }
  }
  print $out;
}
