#!/usr/bin/perl -w
use strict;
#
#
# usage: gen-gupc-inline-lib template.in [input-file ...]
#
# Create a file from template.in that implements selectively inline'd
# GUPC runtime routines.
#
# The files are read first.  This processes lines with the
# following syntax.  Note, the '//' sequence must begin in column one.
#
# The following lines are processed in the input files.
#
# //begin <name>	Collect the following lines up to the matching 'end'
# 			and append them to an internal buffer named <name>
#			Nested Begin ... end brackets are _not_ supported.
#
# //end <name>		End the collection of items into the buffer named <name>.
#
# The following lines are processed in the template file.
#
# //include <name>	Expands into the contents of the buffer named <name>.
#
# //inline		Expands into:
#                       "__attribute__((__always_inline__)) static inline".
#			May appear inside a begin ... end bracket.
#
# The expanded text is written to stdout.
#
my $nargs = scalar @ARGV;
die "missing first (template file) arg" if $nargs < 1;
for (@ARGV)
{
  die "file not found: $_" if ! -f;
}
my $tfile = shift @ARGV;
open TEMPLATE, "<$tfile" or die "can't open template file: $tfile";
my %bufs = ();
my $buf;
# Read the input files listed in @ARGV
while (<>)
{
  chomp;
  if (m{^//begin\s+(\w+)\s*$})
    {
      die "nested buffers not supported,"
          . " last buffer is: $buf"
          if defined ($buf);
      $buf = $1;
      next;
    }
  elsif (m{^//end\s+(\w+)\s*$})
    {
      my $endbuf = $1;
      die "no matching begin for buffer: $endbuf"
          if !defined($buf);
      die "buffer mismatch: $buf != $endbuf"
          if $buf ne $endbuf;
      $buf = undef;
      next;
    }
  if (defined ($buf))
    {
      $bufs{$buf} .= "$_\n";
    }
}
my $inline_attr = "__attribute__((__always_inline__))\nstatic inline";
for $buf (keys %bufs)
{
  $bufs{$buf} =~ s{^//inline\s*$}{$inline_attr}mg;
}
while (<TEMPLATE>)
{
  chomp;
  if (m{^//include\s+(\w+)\s*$})
    {
      $buf = $1;
      die "unknown buffer: $buf" if !exists ($bufs{$buf});
      print $bufs{$buf};
      next;
    }
  elsif (m{^//inline\s*$})
    {
      print "$inline_attr\n";
      next;
    }
  print "$_\n";
}
close TEMPLATE;
