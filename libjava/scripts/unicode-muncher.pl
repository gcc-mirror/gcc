#!/usr/bin/perl -w
# unicode-muncher.pl -- generate Unicode database for java.lang.Character
# Copyright (C) 1998, 2002, 2004  Free Software Foundation, Inc.
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

# Code for reading UnicodeData-3.0.0.txt and SpecialCasing-2.txt to generate
# the code for gnu.java.lang.CharData. The relevant files can be found here:
#
#   http://www.unicode.org/Public/3.0-Update/UnicodeData-3.0.0.txt
#   http://www.unicode.org/Public/3.0-Update/SpecialCasing-2.txt
#
# Inspired by code from Jochen Hoenicke.
# author Eric Blake <ebb9@email.byu.edu>
#
# Usage: ./unicode-muncher <UnicodeData.txt> <SpecialCasing> <CharData.java>
#   where <UnicodeData.txt> is obtained from www.unicode.org (named
#   UnicodeData-3.0.0.txt for Unicode version 3.0.0), <SpecialCasing>
#   is obtained from www.unicode too (named SpecialCasing-2.txt for Unicode
#   version 3.0.0), and <CharData.java> is the final location for the Java
#   interface gnu.java.lang.CharData. As of JDK 1.4, use Unicode version 3.0.0
#   for best results.

##
## Convert a 16-bit integer to a Java source code String literal character
##
sub javaChar($) {
    my ($char) = @_;
    die "Out of range: $char\n" if $char < -0x8000 or $char > 0xffff;
    $char += 0x10000 if $char < 0;
    # Special case characters that must be escaped, or are shorter as ASCII
    return sprintf("\\%03o", $char) if $char < 0x20;
    return "\\\"" if $char == 0x22;
    return "\\\\" if $char == 0x5c;
    return pack("C", $char) if $char < 0x7f;
    return sprintf("\\u%04x", $char);
}

##
## Convert the text UnicodeData file from www.unicode.org into a Java
## interface with string constants holding the compressed information.
##
my @TYPECODES = qw(Cn Lu Ll Lt Lm Lo Mn Me Mc Nd Nl No Zs Zl Zp Cc Cf
                   SKIPPED Co Cs Pd Ps Pe Pc Po Sm Sc Sk So Pi Pf);
my @DIRCODES = qw(L R AL EN ES ET AN CS NSM BN B S WS ON LRE LRO RLE RLO PDF);

my $NOBREAK_FLAG  = 32;
my $MIRRORED_FLAG = 64;

my %special = ();
my @info = ();
my $titlecase = "";
my $count = 0;
my $range = 0;

die "Usage: $0 <UnicodeData.txt> <SpecialCasing.txt> <CharData.java>"
    unless @ARGV == 3;
$| = 1;
print "GNU Classpath Unicode Attribute Database Generator 2.1\n";
print "Copyright (C) 1998, 2002 Free Software Foundation, Inc.\n";

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

# Stage 1: Parse the attribute file
print "Parsing attributes file";
open (UNICODE, "< $ARGV[0]") || die "Can't open Unicode attribute file: $!\n";
while (<UNICODE>) {
    print "." unless $count++ % 1000;
    chomp;
    s/\r//g;
    my ($ch, $name, $category, undef, $bidir, $decomp, undef, undef, $numeric,
        $mirrored, undef, undef, $upcase, $lowcase, $title) = split ';';
    $ch = hex($ch);
    next if $ch > 0xffff; # Ignore surrogate pairs, since Java does

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
        die "numValue too big: $ch, $numValue\n" if $numValue >= 0x7fff;
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
            $info[$_] = pack("n5", $type, $numValue, $upperchar,
                             $lowerchar, $direction);
        }
        $range = 0;
    } elsif ($name =~ /First>$/) {
        $range = $ch;
    }
    $info[$ch] = pack("n5", $type, $numValue, $upperchar, $lowerchar,
                      $direction);
}
close UNICODE;

# Stage 2: Compress the data structures
printf "\nCompressing data structures";
$count = 0;
my $info = ();
my %charhash = ();
my @charinfo = ();

for my $ch (0 .. 0xffff) {
    print "." unless $count++ % 0x1000;
    $info[$ch] = pack("n5", 0, -1, 0, 0, -4) unless defined $info[$ch];

    my ($type, $numVal, $upper, $lower, $direction) = unpack("n5", $info[$ch]);
    if (! exists $charhash{$info[$ch]}) {
        push @charinfo, [ $numVal, $upper, $lower, $direction ];
        $charhash{$info[$ch]} = $#charinfo;
    }
    $info .= pack("n", ($charhash{$info[$ch]} << 7) | $type);
}

my $charlen = @charinfo;
my $bestshift;
my $bestest = 1000000;
my $bestblkstr;
die "Too many unique character entries: $charlen\n" if $charlen > 512;
print "\nUnique character entries: $charlen\n";

for my $i (3 .. 8) {
    my $blksize = 1 << $i;
    my %blocks = ();
    my @blkarray = ();
    my ($j, $k);
    print "shift: $i";

    for ($j = 0; $j < 0x10000; $j += $blksize) {
        my $blkkey = substr $info, 2 * $j, 2 * $blksize;
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
        $bestshift = $i;
        $bestblkstr = $blockstr;
    }
}

my @blocks;
my $blksize = 1 << $bestshift;
for (my $j = 0; $j < 0x10000; $j += $blksize) {
    my $blkkey = substr $info, 2 * $j, 2 * $blksize;
    my $index = index $bestblkstr, $blkkey;
    while ($index & 1) {
        die "not found: $j" if $index == -1;
        $index = index $bestblkstr, $blkkey, $index + 1;
    }
    push @blocks, ($index / 2 - $j) & 0xffff;
}

# Phase 3: Generate the file
die "UTF-8 limit of blocks may be exceeded: " . scalar(@blocks) . "\n"
    if @blocks > 0xffff / 3;
die "UTF-8 limit of data may be exceeded: " . length($bestblkstr) . "\n"
    if length($bestblkstr) > 0xffff / 3;
{
    print "Generating $ARGV[2] with shift of $bestshift";
    my ($i, $j);

    open OUTPUT, "> $ARGV[2]" or die "Failed creating output file: $!\n";
    print OUTPUT <<EOF;
/* gnu/java/lang/CharData -- Database for java.lang.Character Unicode info
   Copyright (C) 2002 Free Software Foundation, Inc.
   *** This file is generated by scripts/unicode-muncher.pl ***

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

package gnu.java.lang;

/**
 * This contains the info about the unicode characters, that
 * java.lang.Character needs.  It is generated automatically from
 * <code>$ARGV[0]</code> and
 * <code>$ARGV[1]</code>, by some
 * perl scripts. These Unicode definition files can be found on the
 * <a href="http://www.unicode.org">http://www.unicode.org</a> website.
 * JDK 1.4 uses Unicode version 3.0.0.
 *
 * The data is stored as string constants, but Character will convert these
 * Strings to their respective <code>char[]</code> components.  The field
 * <code>BLOCKS</code> stores the offset of a block of 2<sup>SHIFT</sup>
 * characters within <code>DATA</code>.  The DATA field, in turn, stores
 * information about each character in the low order bits, and an offset
 * into the attribute tables <code>UPPER</code>, <code>LOWER</code>,
 * <code>NUM_VALUE</code>, and <code>DIRECTION</code>.  Notice that the
 * attribute tables are much smaller than 0xffff entries; as many characters
 * in Unicode share common attributes.  The DIRECTION table also contains
 * a field for detecting characters with multi-character uppercase expansions.
 * Next, there is a listing for <code>TITLE</code> exceptions (most characters
 * just have the same title case as upper case).  Finally, there are two
 * tables for multi-character capitalization, <code>UPPER_SPECIAL</code>
 * which lists the characters which are special cased, and
 * <code>UPPER_EXPAND</code>, which lists their expansion.
 *
 * \@author scripts/unicode-muncher.pl (written by Jochen Hoenicke,
 *         Eric Blake)
 * \@see Character
 * \@see String
 */
public interface CharData
{
  /**
   * The Unicode definition file that was parsed to build this database.
   */
  String SOURCE = \"$ARGV[0]\";

  /**
   * The character shift amount to look up the block offset. In other words,
   * <code>(char) (BLOCKS.value[ch >> SHIFT] + ch)</code> is the index where
   * <code>ch</code> is described in <code>DATA</code>.
   */
  int SHIFT = $bestshift;

  /**
   * The mapping of character blocks to their location in <code>DATA</code>.
   * Each entry has been adjusted so that the 16-bit sum with the desired
   * character gives the actual index into <code>DATA</code>.
   */
  String BLOCKS
EOF

    for ($i = 0; $i < @blocks / 11; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 10) {
            last if @blocks <= $i * 11 + $j;
            my $val = $blocks[$i * 11 + $j];
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * Information about each character.  The low order 5 bits form the
   * character type, the next bit is a flag for non-breaking spaces, and the
   * next bit is a flag for mirrored directionality.  The high order 9 bits
   * form the offset into the attribute tables.  Note that this limits the
   * number of unique character attributes to 512, which is not a problem
   * as of Unicode version 3.2.0, but may soon become one.
   */
  String DATA
EOF

    my $len = length($bestblkstr) / 2;
    for ($i = 0; $i < $len / 11; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 10) {
            last if $len <= $i * 11 + $j;
            my $val = unpack "n", substr($bestblkstr, 2 * ($i * 11 + $j), 2);
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is the attribute table for computing the numeric value of a
   * character.  The value is -1 if Unicode does not define a value, -2
   * if the value is not a positive integer, otherwise it is the value.
   * Note that this is a signed value, but stored as an unsigned char
   * since this is a String literal.
   */
  String NUM_VALUE
EOF

    $len = @charinfo;
    for ($i = 0; $i < $len / 11; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 10) {
            last if $len <= $i * 11 + $j;
            my $val = $charinfo[$i * 11 + $j][0];
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is the attribute table for computing the single-character uppercase
   * representation of a character.  The value is the signed difference
   * between the character and its uppercase version.  Note that this is
   * stored as an unsigned char since this is a String literal.  When
   * capitalizing a String, you must first check if a multi-character uppercase
   * sequence exists before using this character.
   */
  String UPPER
EOF

    $len = @charinfo;
    for ($i = 0; $i < $len / 11; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 10) {
            last if $len <= $i * 11 + $j;
            my $val = $charinfo[$i * 11 + $j][1];
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is the attribute table for computing the lowercase representation
   * of a character.  The value is the signed difference between the
   * character and its lowercase version.  Note that this is stored as an
   * unsigned char since this is a String literal.
   */
  String LOWER
EOF

    $len = @charinfo;
    for ($i = 0; $i < $len / 13; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 12) {
            last if $len <= $i * 13 + $j;
            my $val = $charinfo[$i * 13 + $j][2];
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is the attribute table for computing the directionality class
   * of a character, as well as a marker of characters with a multi-character
   * capitalization.  The direction is taken by performing a signed shift
   * right by 2 (where a result of -1 means an unknown direction, such as
   * for undefined characters). The lower 2 bits form a count of the
   * additional characters that will be added to a String when performing
   * multi-character uppercase expansion. This count is also used, along with
   * the offset in UPPER_SPECIAL, to determine how much of UPPER_EXPAND to use
   * when performing the case conversion. Note that this information is stored
   * as an unsigned char since this is a String literal.
   */
  String DIRECTION
EOF

    $len = @charinfo;
    for ($i = 0; $i < $len / 17; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 16) {
            last if $len <= $i * 17 + $j;
            my $val = $charinfo[$i * 17 + $j][3];
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is the listing of titlecase special cases (all other characters
   * can use <code>UPPER</code> to determine their titlecase).  The listing
   * is a sorted sequence of character pairs; converting the first character
   * of the pair to titlecase produces the second character.
   */
  String TITLE
EOF

    $len = length($titlecase) / 2;
    for ($i = 0; $i < $len / 11; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 10) {
            last if $len <= $i * 11 + $j;
            my $val = unpack "n", substr($titlecase, 2 * ($i * 11 + $j), 2);
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is a listing of characters with multi-character uppercase sequences.
   * A character appears in this list exactly when it has a non-zero entry
   * in the low-order 2-bit field of DIRECTION.  The listing is a sorted
   * sequence of pairs (hence a binary search on the even elements is an
   * efficient way to lookup a character). The first element of a pair is the
   * character with the expansion, and the second is the index into
   * UPPER_EXPAND where the expansion begins. Use the 2-bit field of
   * DIRECTION to determine where the expansion ends.
   */
  String UPPER_SPECIAL
EOF

    my @list = sort {$a <=> $b} keys %special;
    my $expansion = "";
    my $offset = 0;
    $len = @list;
    for ($i = 0; $i < $len / 5; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 4) {
            last if $len <= $i * 5 + $j;
            my $ch = $list[$i * 5 + $j];
            print OUTPUT javaChar($ch);
            print OUTPUT javaChar($offset);
            $offset += @{$special{$ch}};
            $expansion .= pack "n*", @{$special{$ch}};
        }
        print OUTPUT "\"";
    }

    print OUTPUT <<EOF;
;

  /**
   * This is the listing of special case multi-character uppercase sequences.
   * Characters listed in UPPER_SPECIAL index into this table to find their
   * uppercase expansion. Remember that you must also perform special-casing
   * on two single-character sequences in the Turkish locale, which are not
   * covered here in CharData.
   */
  String UPPER_EXPAND
EOF

    $len = length($expansion) / 2;
    for ($i = 0; $i < $len / 11; $i++) {
        print OUTPUT $i ? "\n    + \"" : "    = \"";
        for $j (0 .. 10) {
            last if $len <= $i * 11 + $j;
            my $val = unpack "n", substr($expansion, 2 * ($i * 11 + $j), 2);
            print OUTPUT javaChar($val);
        }
        print OUTPUT "\"";
    }

    print OUTPUT ";\n}\n";
    close OUTPUT;
}
print "\nDone.\n";
