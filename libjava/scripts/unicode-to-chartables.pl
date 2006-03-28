#!/usr/bin/perl -w
# unicode-to-chartables.pl -- generate Unicode database for java.lang.Character
# Copyright (C) 1998, 2002, 2004, 2006  Free Software Foundation, Inc.
#
# This file is part of GNU Classpath.
#
# GNU Classpath is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Classpath is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Classpath; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.
#
# Linking this library statically or dynamically with other modules is
# making a combined work based on this library.  Thus, the terms and
# conditions of the GNU General Public License cover the whole
# combination.
#
# As a special exception, the copyright holders of this library give you
# permission to link this library with independent modules to produce an
# executable, regardless of the license terms of these independent
# modules, and to copy and distribute the resulting executable under
# terms of your choice, provided that you also meet, for each linked
# independent module, the terms and conditions of the license of that
# module.  An independent module is a module which is not derived from
# or based on this library.  If you modify this library, you may extend
# this exception to your version of the library, but you are not
# obligated to do so.  If you do not wish to do so, delete this
# exception statement from your version.

# Code for reading UnicodeData-4.0.0.txt and SpecialCasing-4.0.0.txt to generate
# the code for java-chartables.h. The relevant files can be found here:
#
#   http://www.unicode.org/Public/4.0-Update/UnicodeData-4.0.0.txt
#   http://www.unicode.org/Public/4.0-Update/SpecialCasing-4.0.0.txt
#
# Inspired by code from Jochen Hoenicke.
# author Eric Blake <ebb9@email.byu.edu>
# Unicode 4.0.0 support by Anthony Balkissoon <abalkiss@redhat.com>
#
# Usage: ./unicode-to-chartables.pl <UnicodeData> <SpecialCasing> <tables>
#   where <UnicodeData.txt> is obtained from www.unicode.org (named
#   UnicodeData-4.0.0.txt for Unicode version 4.0.0), <SpecialCasing>
#   is obtained from www.unicode too (named SpecialCasing-4.0.0.txt for Unicode
#   version 4.0.0), and <tables> is the final location for the header file
#   java-chartables.h. As of JDK 1.5, use Unicode version 4.0.0
#   for best results.


##
## Return the given variable interpreted as a 16 bit signed number.
##
sub cShort($) {
    my ($char) = @_;
    return unpack "s", pack "I", $char;
}

##
## Convert the text UnicodeData file from www.unicode.org into a header file
## interface with arrays holding the compressed information.
##
my @TYPECODES = qw(Cn Lu Ll Lt Lm Lo Mn Me Mc Nd Nl No Zs Zl Zp Cc Cf
                   SKIPPED Co Cs Pd Ps Pe Pc Po Sm Sc Sk So Pi Pf);
my @DIRCODES = qw(L R AL EN ES ET AN CS NSM BN B S WS ON LRE LRO RLE RLO PDF);

my $NOBREAK_FLAG  = 32;
my $MIRRORED_FLAG = 64;

my %special = ();

# infoArray is an array where each element is a list of character information
# for characters in a plane.  The index of each list is equal to the plane 
# that it corresponds to even though most of these lists will currently be
# empty.  This is done so that that this script can be easily modified to 
# accomodate future versions of Unicode.
my @infoArray = \((), (), (), (), (), (), (), (), 
    (), (), (), (), (), (), (), (), ());

# info is a reference to one of the lists in infoArray, depending on which 
# plane we're currently parsing.
my $info;

# largeNums is an array of numerical values that are too large to fit 
# into the 16 bit char where most numerical values are stored.  
# What is stored in the char then is a number N such that (-N - 3) is 
# the index into largeNums where the numerical value can be found.
my @largeNums = ();

my $titlecase = "";
my $count = 0;
my $range = 0;

die "Usage: $0 <UnicodeData.txt> <SpecialCasing.txt> <java-chartables.h>"
    unless @ARGV == 3;
$| = 1;
print "GNU Classpath Unicode Attribute Database Generator 2.1\n";
print "Copyright (C) 1998, 2002 Free Software Foundation, Inc.\n";


################################################################################
################################################################################
# Stage 0: Parse the special casing file
print "Parsing special casing file\n";
open (SPECIAL, "< $ARGV[1]") || die "Can't open special casing file: $!\n";
while (<SPECIAL>) {
    next if /^\#/;
    my ($ch, undef, undef, $upper) = split / *; */;

    # This grabs only the special casing for multi-char uppercase. Note that
    # there are no multi-char lowercase, and that Sun ignores multi-char
    # titlecase rules. This script omits 3 special cases in Unicode 3.0.0,
    # which must be hardcoded in java.lang.String:
    #  \u03a3 (Sun ignores this special case)
    #  \u0049 - lowercases to \u0131, but only in Turkish locale
    #  \u0069 - uppercases to \u0130, but only in Turkish locale
    next unless defined $upper and $upper =~ / /;
    $special{hex $ch} = [map {hex} split ' ', $upper];
}

close SPECIAL;


################################################################################
################################################################################
## Stage 1: Parse the attribute file
print "Parsing attributes file";
open (UNICODE, "< $ARGV[0]") || die "Can't open Unicode attribute file: $!\n";
while (<UNICODE>) {
    print "." unless $count++ % 1000;
    chomp;
    s/\r//g;
    my ($ch, $name, $category, undef, $bidir, $decomp, undef, undef, $numeric,
        $mirrored, undef, undef, $upcase, $lowcase, $title) = split ';';
    $ch = hex($ch);

    # plane tells us which Unicode code plane we're currently in and is an
    # index into infoArray.
    my $plane = int($ch / 0x10000);
    my $planeBase = $plane * 0x10000;
    $info = \@{$infoArray[$plane]};

    my ($type, $numValue, $upperchar, $lowerchar, $direction);

    $type = 0;
    while ($category !~ /^$TYPECODES[$type]$/) {
        if (++$type == @TYPECODES) {
            die "$ch: Unknown type: $category";
        }
    }
    $type |= $NOBREAK_FLAG if ($decomp =~ /noBreak/);
    $type |= $MIRRORED_FLAG if ($mirrored =~ /Y/);

    if ($numeric =~ /^[0-9]+$/) {
        $numValue = $numeric;
        # If numeric takes more than 16 bits to store we want to store that 
	# number in a separate array and store a number N in numValue such 
	# that (-N - 3) is the offset into the separate array containing the
	# large numerical value.
	if ($numValue >= 0x7fff) {
	    $numValue = -3 - @largeNums;
	    push @largeNums, $numeric;	    
	}
    } elsif ($numeric eq "") {
        # Special case sequences of 'a'-'z'
        if ($ch >= 0x0041 && $ch <= 0x005a) {
            $numValue = $ch - 0x0037;
        } elsif ($ch >= 0x0061 && $ch <= 0x007a) {
            $numValue = $ch - 0x0057;
        } elsif ($ch >= 0xff21 && $ch <= 0xff3a) {
            $numValue = $ch - 0xff17;
        } elsif ($ch >= 0xff41 && $ch <= 0xff5a) {
            $numValue = $ch - 0xff37;
        } else {
            $numValue = -1;
        }
    } else {
        $numValue = -2;
    }

    $upperchar = $upcase ? hex($upcase) - $ch : 0;
    $lowerchar = $lowcase ? hex($lowcase) - $ch : 0;
    if ($title ne $upcase) {
        my $titlechar = $title ? hex($title) : $ch;
        $titlecase .= pack("n2", $ch, $titlechar);
    }

    $direction = 0;
    while ($bidir !~ /^$DIRCODES[$direction]$/) {
        if (++$direction == @DIRCODES) {
            $direction = -1;
            last;
        }
    }
    $direction <<= 2;
    $direction += $#{$special{$ch}} if defined $special{$ch};

    if ($range) {
        die "Expecting end of range at $ch\n" unless $name =~ /Last>$/;
        for ($range + 1 .. $ch - 1) {
            $info->[$_ - $planeBase] = pack("n5", $type, $numValue, $upperchar,
                             $lowerchar, $direction);
        }
        $range = 0;
    } elsif ($name =~ /First>$/) {
        $range = $ch;
    }
    # Store all this parsed information into the element in infoArray that info
    # points to.
    $info->[$ch - $planeBase] = pack("n5", $type, $numValue, $upperchar, $lowerchar,
                      $direction);
}
close UNICODE;


################################################################################
################################################################################
## Stage 2: Compress the data structures
printf "\nCompressing data structures";
$count = 0;

# data is a String that will be used to create the DATA String containing 
# character information and offsets into the attribute tables.
my @data = ();

# charhashArray is an array of hashtables used so that we can reuse character
# attributes when characters share the same attributes ... this makes our
# attribute tables smaller.  charhash is a pointer into this array.
my @charhashArray = ({}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {});
my $charhash = ();

# charinfoArray is an array of arrays, one per plane, for storing character 
# information.  charinfo is a pointer into this array.
my @charinfoArray = \((), (), (), (), (), (), (), (), 
    (), (), (), (), (), (), (), (), ());
my $charinfo;

# charlen is an array, one element per plane, that tells us how many unique
# character attributes there are for that plane.
my @charlen = ();

for my $plane (0 .. 0x10) {
    $info = \@{$infoArray[$plane]};
    my $planeBase = $plane * 0x10000;
    $charhash = \%{$charhashArray[$plane]};
    $charinfo = \@{$charinfoArray[$plane]};

    for my $ch ($planeBase .. $planeBase + 0xffff) {
	my $index = $ch - $planeBase;
	print "." unless $count++ % 0x1000;
	$info->[$index] = pack("n5", 0, -1, 0, 0, -4) unless defined $info->[$index];
	
	my ($type, $numVal, $upper, $lower, $direction) = unpack("n5", $info->[$index]);
	if (! exists $charhash->{$info->[$index]}) {
	    # If we entered this loop that means the character we're looking at 
	    # now has attributes that are unique from those that we've looked
	    # at so far for this plane.  So we push its attributes into charinfo
	    # and store in charhash the offset into charinfo where these
	    # attributes can later be found.
	    push @{$charinfo}, [ $numVal, $upper, $lower, $direction ];
	    $charhash->{$info->[$index]} = @{$charinfo} - 1;
	    # When the file is generaged, the number we just stored in charhas
	    # will be the upper 9 bits in the DATA String that are an offset
	    # into the attribute tables.
	}
	$data[$plane] .= pack("n", ($charhash->{$info->[$index]} << 7) | $type);
    }
    $charlen[$plane] = scalar(@{$charinfoArray[$plane]});
}

# the shift that results in the best compression of the table.  This is an array
# because different shifts are better for the different tables for each plane.
my @bestshift;

# an initial guess.
my $bestest = 1000000;
my @bestblkstr;
my @blksize = ();

for my $plane (0 .. 0x10) {
    print "\n\nplane: $plane\n";
    print "Unique character entries: $charlen[$plane]\n";
    $bestest = 1000000;
    for my $i (3 .. 8) {
        my $blksize = 1 << $i;
        my %blocks = ();
        my @blkarray = ();
        my ($j, $k);
        print "shift: $i";

        for ($j = 0; $j < 0x10000; $j += $blksize) {
            my $blkkey = substr $data[$plane], 2 * $j, 2 * $blksize;
            if (! exists $blocks{$blkkey}) {
	        push @blkarray, $blkkey;
                $blocks{$blkkey} = $#blkarray;
            }
        }

        my $blknum = @blkarray;
        my $blocklen = $blknum * $blksize;
        printf " before %5d", $blocklen;

        # Now we try to pack the blkarray as tight as possible by finding matching
        # heads and tails.
        for ($j = $blksize - 1; $j > 0; $j--) {
            my %tails = ();
            for $k (0 .. $#blkarray) {
                next unless defined $blkarray[$k];
                my $len = length $blkarray[$k];
                my $tail = substr $blkarray[$k], $len - $j * 2;
                if (exists $tails{$tail}) {
                    push @{$tails{$tail}}, $k;
                } else {
                    $tails{$tail} = [ $k ];
                }
            }

            # tails are calculated, now calculate the heads and merge.
          BLOCK:
            for $k (0 .. $#blkarray) {
                next unless defined $blkarray[$k];
                my $tomerge = $k;
                while (1) {
                    my $head = substr($blkarray[$tomerge], 0, $j * 2);
                    my $entry = $tails{$head};
                    next BLOCK unless defined $entry;

                    my $other = shift @{$entry};
                    if ($other == $tomerge) {
                        if (@{$entry}) {
                            push @{$entry}, $other;
                            $other = shift @{$entry};
                        } else {
                            push @{$entry}, $other;
                            next BLOCK;
                        }
                    }
                    if (@{$entry} == 0) {
                        delete $tails{$head};
                    }

                    # a match was found
                    my $merge = $blkarray[$other]
                        . substr($blkarray[$tomerge], $j * 2);
                    $blocklen -= $j;
                    $blknum--;

                    if ($other < $tomerge) {
                        $blkarray[$tomerge] = undef;
                        $blkarray[$other] = $merge;
                        my $len = length $merge;
                        my $tail = substr $merge, $len - $j * 2;
                        $tails{$tail} = [ map { $_ == $tomerge ? $other : $_ }
                                          @{$tails{$tail}} ];
                        next BLOCK;
                    }
                    $blkarray[$tomerge] = $merge;
                    $blkarray[$other] = undef;
                }
            }
        }
        my $blockstr;
        for $k (0 .. $#blkarray) {
            $blockstr .= $blkarray[$k] if defined $blkarray[$k];
        }

        die "Unexpected $blocklen" if length($blockstr) != 2 * $blocklen;
        my $estimate = 2 * $blocklen + (0x20000 >> $i);
  
        printf " after merge %5d: %6d bytes\n", $blocklen, $estimate;
        if ($estimate < $bestest) {
            $bestest = $estimate;
            $bestshift[$plane] = $i;
            $bestblkstr[$plane] = $blockstr;
        }
    }
    $blksize[$plane] = 1 << $bestshift[$plane];
    print "best shift: ", $bestshift[$plane];
    print "     blksize: ", $blksize[$plane];
}
my @blocksArray = \((), (), (), (), (), (), (), (), 
    (), (), (), (), (), (), (), (), ());

for my $plane (0 .. 0x10) {
    for (my $j = 0; $j < 0x10000; $j += $blksize[$plane]) {
	my $blkkey = substr $data[$plane], 2 * $j, 2 * $blksize[$plane];
        my $index = index $bestblkstr[$plane], $blkkey;
        while ($index & 1) {
            die "not found: $j" if $index == -1;
            $index = index $bestblkstr[$plane], $blkkey, $index + 1;
        }
        push @{$blocksArray[$plane]}, ($index / 2 - $j) & 0xffff;
    }
}


################################################################################
################################################################################
## Stage 3: Generate the file
for my $plane (0 .. 0x10) {
    die "UTF-8 limit of blocks may be exceeded for plane $plane: " . scalar(@{$blocksArray[$plane]}) . "\n"
        if @{$blocksArray[$plane]} > 0xffff / 3;
    die "UTF-8 limit of data may be exceeded for plane $plane: " . length($bestblkstr[$plane]) . "\n"
        if length($bestblkstr[$plane]) > 0xffff / 3;
}

{
    print "\nGenerating $ARGV[2].";
    my ($i, $j);

    open OUTPUT, "> $ARGV[2]" or die "Failed creating output file: $!\n";
    print OUTPUT <<EOF;
/* java-chartables.h -- Character tables for java.lang.Character -*- c++ -*-
   Copyright (C) 2002, 2006 Free Software Foundation, Inc.
   *** This file is generated by scripts/unicode-to-chartables.pl ***

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

#ifndef __JAVA_CHARTABLES_H__
#define __JAVA_CHARTABLES_H__

// These tables are automatically generated by scripts/unicode_to_chartables.pl.
// The Unicode data comes from www.unicode.org; this header is based on
// UnicodeData-4.0.0.txt. JDK 1.5 uses Unicode version 4.0.0.
// DO NOT EDIT the tables.  Instead, fix the upstream scripts and run
// them again.

// The data is stored in C style arrays of the appropriate CNI types, to
// guarantee that the data is constant and non-relocatable.  The field
// <code>blocks</code> stores the offset of a block of 2<sup>SHIFT</sup>
// characters within <code>data</code>. The data field, in turn, stores
// information about each character in the low order bits, and an offset
// into the attribute tables <code>upper</code>, <code>lower</code>,
// <code>numValue</code>, and <code>direction</code>.  Notice that the
// attribute tables are much smaller than 0xffff entries; as many characters
// in Unicode share common attributes.  Finally, there is a listing for
// <code>title</code> exceptions (most characters just have the same title
// case as upper case).

// This file should only be included by natCharacter.cc

/**
 * The array containing the numeric values that are too large to be stored as
 * chars in NUM_VALUE.  NUM_VALUE in this case will contain a negative integer
 * N such that LARGENUMS[-N - 3] contains the correct numeric value.
 */
EOF
  print OUTPUT "static const jint largenums[] = {\n    ";
  for ($i = 0; $i < @largeNums; $i++) {
      print OUTPUT $largeNums[$i], ", ";
  }
  print OUTPUT "}";
  print OUTPUT <<EOF;
;

/**
 * The character shift amount to look up the block offset. In other words,
 * <code>(char) (blocks[p][off >> SHIFT[p]] + off)</code> is the index where
 * <code>ch</code> is described in <code>data</code>, where <code>off</code>
 * is ch & 0xffff and <code>p</code> is the plane the character belongs to.
 */
EOF
  print OUTPUT "static const int shift[] = {\n    ";
  for ($i = 0; $i < @bestshift; $i++) {
      print OUTPUT $bestshift[$i], ", ";
  }
  print OUTPUT "}";
  print OUTPUT <<EOF;
;

/**
 * The mapping of character blocks to their location in <code>data</code>.
 * Each entry has been adjusted so that a modulo 16 sum with the desired
 * character gives the actual index into <code>data</code>.
 */
EOF
  for ($plane = 0; $plane <= 0x10; $plane++) {
      # The following if statement handles the cases of unassigned planes
      # specially so we don't waste space with unused Strings.  As of 
      # Unicode version 4.0.0 only planes 0, 1, 2, and 14 are used.  If
      # you are updating this script to work with a later version of 
      # Unicode you may have to alter this if statement.
      next if ($plane > 2 && $plane != 14) ;
      
      print OUTPUT "static const jchar blocks", $plane, "[] = {\n";
      for ($i = 0; $i < @{$blocksArray[$plane]} / 10; $i++) {
	  print OUTPUT "    ";
	  for $j (0 .. 9) {
	      last if @{$blocksArray[$plane]} <= $i * 10 + $j;
	      my $val = $blocksArray[$plane]->[$i * 10 + $j];
	      print OUTPUT $val, ", ";
	  }
	  print OUTPUT "\n";
      }
      print OUTPUT "};\n\n";
  }
  print OUTPUT "static const int blocks_length[] = {\n    ";
  for ($plane = 0; $plane <= 0x10; $plane++) {
      if ($plane > 2 && $plane != 14){
	  print OUTPUT "-1, ";
      }
      else {
	  print OUTPUT scalar(@{$blocksArray[$plane]}), ", ";
      }
  }
  print OUTPUT "};\n";
  print OUTPUT <<EOF;
static const jchar* blocks[] = {
    blocks0, blocks1, blocks2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, blocks14, NULL, NULL};

/**
 * Information about each character.  The low order 5 bits form the
 * character type, the next bit is a flag for non-breaking spaces, and the
 * next bit is a flag for mirrored directionality.  The high order 9 bits
 * form the offset into the attribute tables.  Note that this limits the
 * number of unique character attributes per plane to 512, which is not a 
 * problem as of Unicode version 4.0.0, but may soon become one.
 */
EOF
  for ($plane = 0; $plane <= 0x10; $plane++) {
      # The following if statement handles the cases of unassigned planes
      # specially so we don't waste space with unused Strings.  As of 
      # Unicode version 4.0.0 only planes 0, 1, 2, and 14 are used.  If
      # you are updating this script to work with a later version of 
      # Unicode you may have to alter this if statement.
      next if ($plane > 2 && $plane != 14);
      
      print OUTPUT "static const jchar data", $plane, "[] = {\n";
      my $len = length($bestblkstr[$plane]) / 2;
      for ($i = 0; $i < $len / 10; $i++) {
	  print OUTPUT "    ";
	  for $j (0 .. 9) {
	      last if $len <= $i * 10 + $j;
	      my $val = unpack "n", substr($bestblkstr[$plane], 2 * ($i * 10 + $j), 2);
	      print OUTPUT $val, ", ";
	  }
	  print OUTPUT "\n";
      }
      print OUTPUT "};\n\n";
  }
  print OUTPUT "static const int data_length[] = {\n    ";
  for ($plane = 0; $plane <= 0x10; $plane++) {
      if ($plane > 2 && $plane != 14){
	  print OUTPUT "-1, ";
      }
      else {
	  print OUTPUT length($bestblkstr[$plane]) / 2, ", ";
      }
  }
  print OUTPUT "};\n";
  print OUTPUT <<EOF;
static const jchar* data[] = {
    data0, data1, data2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, data14, NULL, NULL};


/**
 * This is the attribute table for computing the numeric value of a
 * character.  The value is -1 if Unicode does not define a value, -2
 * if the value is not a positive integer, otherwise it is the value.
 */
EOF
  for ($plane = 0; $plane <= 0x10; $plane++) {
      # The following if statement handles the cases of unassigned planes
      # specially so we don't waste space with unused Strings.  As of 
      # Unicode version 4.0.0 only planes 0, 1, 2, and 14 are used.  If
      # you are updating this script to work with a later version of 
      # Unicode you may have to alter this if statement.
      next if ($plane > 2 && $plane != 14);
      
      print OUTPUT "static const jshort numValue", $plane, "[] = {\n";
      $len = @{$charinfoArray[$plane]};
      for ($i = 0; $i < $len / 13; $i++) {
	  print OUTPUT "    ";
	  for $j (0 .. 12) {
	      last if $len <= $i * 13 + $j;
	      my $val = $charinfoArray[$plane]->[$i * 13 + $j][0];
	      print OUTPUT cShort($val), ", ";
	  }
	  print OUTPUT "\n";
      }
      print OUTPUT "};\n\n";
  }
  print OUTPUT "static const int numValue_length[] = {\n    ";
  for ($plane = 0; $plane <= 0x10; $plane++) {
      if ($plane > 2 && $plane != 14){
	  print OUTPUT "-1, ";
      }
      else {
	  print OUTPUT scalar(@{$charinfoArray[$plane]}), ", ";
      }
  }
  print OUTPUT "};\n";
  print OUTPUT <<EOF;
static const jshort* numValue[] = {
    numValue0, numValue1, numValue2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, numValue14, NULL, NULL};



/**
 * This is the attribute table for computing the uppercase representation
 * of a character.  The value is the difference between the character and
 * its uppercase version.
 */
EOF
  for ($plane = 0; $plane <= 0x10; $plane++) {
      # The following if statement handles the cases of unassigned planes
      # specially so we don't waste space with unused Strings.  As of 
      # Unicode version 4.0.0 only planes 0, 1, 2, and 14 are used.  If
      # you are updating this script to work with a later version of 
      # Unicode you may have to alter this if statement.
      next if ($plane > 2 && $plane != 14);
      
      print OUTPUT "static const jshort upper", $plane, "[] = {\n";
      $len = @{$charinfoArray[$plane]};
      for ($i = 0; $i < $len / 13; $i++) {
	  print OUTPUT "    ";
	  for $j (0 .. 12) {
	      last if $len <= $i * 13 + $j;
	      my $val = $charinfoArray[$plane]->[$i * 13 + $j][1];
	      print OUTPUT cShort($val), ", ";
	  }
	  print OUTPUT "\n";
      }
      print OUTPUT "};\n\n";
  }
  print OUTPUT "static const int upper_length[] = {\n    ";
  for ($plane = 0; $plane <= 0x10; $plane++) {
      if ($plane > 2 && $plane != 14){
	  print OUTPUT "-1, ";
      }
      else {
	  print OUTPUT scalar(@{$charinfoArray[$plane]}), ", ";
      }
  }
  print OUTPUT "};\n";
  print OUTPUT <<EOF;
static const jshort* upper[] = {
    upper0, upper1, upper2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, upper14, NULL, NULL};


/**
 * This is the attribute table for computing the lowercase representation
 * of a character.  The value is the difference between the character and
 * its lowercase version.
 */
EOF
  for ($plane = 0; $plane <= 0x10; $plane++) {
      # The following if statement handles the cases of unassigned planes
      # specially so we don't waste space with unused Strings.  As of 
      # Unicode version 4.0.0 only planes 0, 1, 2, and 14 are used.  If
      # you are updating this script to work with a later version of 
      # Unicode you may have to alter this if statement.
      next if ($plane > 2 && $plane != 14);
      
      print OUTPUT "static const jshort lower", $plane, "[] = {\n";
      $len = @{$charinfoArray[$plane]};
      for ($i = 0; $i < $len / 13; $i++) {
	  print OUTPUT "    ";
	  for $j (0 .. 12) {
	      last if $len <= $i * 13 + $j;
	      my $val = $charinfoArray[$plane]->[$i * 13 + $j][2];
	      print OUTPUT cShort($val), ", ";
	  }
	  print OUTPUT "\n";
      }
      print OUTPUT "};\n\n";
  }
  print OUTPUT "static const int lower_length[] = {\n    ";
  for ($plane = 0; $plane <= 0x10; $plane++) {
      if ($plane > 2 && $plane != 14){
	  print OUTPUT "-1, ";
      }
      else {
	  print OUTPUT scalar(@{$charinfoArray[$plane]}), ", ";
      }
  }
  print OUTPUT "};\n";
  print OUTPUT <<EOF;
static const jshort* lower[] = {
    lower0, lower1, lower2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, lower14, NULL, NULL};


/**
 * This is the attribute table for computing the directionality class
 * of a character.  At present, the value is in the range 0 - 18 if the
 * character has a direction, otherwise it is -1.
 */
EOF
  for ($plane = 0; $plane <= 0x10; $plane++) {
      # The following if statement handles the cases of unassigned planes
      # specially so we don't waste space with unused Strings.  As of 
      # Unicode version 4.0.0 only planes 0, 1, 2, and 14 are used.  If
      # you are updating this script to work with a later version of 
      # Unicode you may have to alter this if statement.
      next if ($plane > 2 && $plane != 14);
      
      print OUTPUT "static const jbyte direction", $plane, "[] = {\n";
      $len = @{$charinfoArray[$plane]};
      for ($i = 0; $i < $len / 19; $i++) {
	  print OUTPUT "    ";
	  for $j (0 .. 18) {
	      last if $len <= $i * 19 + $j;
	      my $val = $charinfoArray[$plane]->[$i * 19 + $j][3];
	      $val >>= 2;
	      if ($val < 0 || $val > 18){
		  $val = -1;
	      }
	      print OUTPUT cShort($val), ", ";
	  }
	  print OUTPUT "\n";
      }
      print OUTPUT "};\n\n";
  }
  print OUTPUT "static const int direction_length[] = {\n    ";
  for ($plane = 0; $plane <= 0x10; $plane++) {
      if ($plane > 2 && $plane != 14){
	  print OUTPUT "-1, ";
      }
      else {
	  print OUTPUT scalar(@{$charinfoArray[$plane]}), ", ";
      }
  }
  print OUTPUT "};\n";
  print OUTPUT <<EOF;
static const jbyte* direction[] = {
    direction0, direction1, direction2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
    NULL, NULL, NULL, NULL, direction14, NULL, NULL};


/**
 * This is the listing of titlecase special cases (all other character
 * can use <code>upper</code> to determine their titlecase).  The listing
 * is a sequence of character pairs; converting the first character of the
 * pair to titlecase produces the second character.
 */
static const jchar title[] = {
EOF

  $len = length($titlecase) / 2;
  for ($i = 0; $i < $len / 10; $i++) {
      print OUTPUT $i ? "\n    " : "    ";
      for $j (0 .. 9) {
	  last if $len <= $i * 10 + $j;
	  my $val = unpack "n", substr($titlecase, 2 * ($i * 10 + $j), 2);
	  print OUTPUT $val, ", ";
      }
  }

  print OUTPUT "\n  };";
  print OUTPUT "\n/** Length of title. */\nstatic const int title_length = ", $len;
  print OUTPUT <<EOF;
;

#endif /* __JAVA_CHARTABLES_H__ */
EOF
  close OUTPUT;
}
print "\nDone.\n";
