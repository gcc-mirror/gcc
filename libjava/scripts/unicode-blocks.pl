#!/usr/bin/perl -w
# unicode-blocks.pl -- Script to generate java.lang.Character.UnicodeBlock
# Copyright (C) 2002 Free Software Foundation, Inc.
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
# Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307 USA.
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


# Code for reading Blocks.txt and generating (to standard out) the code for
# java.lang.Character.UnicodeBlock, for pasting into java/lang/Character.java.
# You should probably check that the results are accurate to the
# specification, but I made sure it works OOB for Unicode 3.0.0 and JDK 1.4.
# As the grammar for the Blocks.txt file is changing in Unicode 3.2.0, you
# will have to tweak this some for future use.  For now, the relevant
# Unicode definition files are found in libjava/gnu/gcj/convert/.
#
# author Eric Blake <ebb9@email.byu.edu>
#
# usage: unicode-blocks.pl <blocks.txt>
#    where <blocks.txt> is obtained from www.unicode.org (named Blocks-3.txt
#    for Unicode version 3.0.0).


die "Usage: $0 <blocks.txt>" unless @ARGV == 1;
open (BLOCKS, $ARGV[0]) || die "Can't open Unicode block file: $!\n";

# A hash of added fields and the JDK they were added in, to automatically
# print @since tags.  Maintaining this is optional (and tedious), but nice.
my %additions = ("SYRIAC" => "1.4",
                 "THAANA" => "1.4",
                 "SINHALA" => "1.4",
                 "MYANMAR" => "1.4",
                 "ETHIOPIC" => "1.4",
                 "CHEROKEE" => "1.4",
                 "UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS" => "1.4",
                 "OGHAM" => "1.4",
                 "RUNIC" => "1.4",
                 "KHMER" => "1.4",
                 "MONGOLIAN" => "1.4",
                 "BRAILLE_PATTERNS" => "1.4",
                 "CJK_RADICALS_SUPPLEMENT" => "1.4",
                 "KANGXI_RADICALS" => "1.4",
                 "IDEOGRAPHIC_DESCRIPTION_CHARACTERS" => "1.4",
                 "BOPOMOFO_EXTENDED" => "1.4",
                 "CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A" => "1.4",
                 "YI_SYLLABLES" => "1.4",
                 "YI_RADICALS" => "1.4",
                 );

print <<'EOF';
  /**
   * A family of character subsets in the Unicode specification. A character
   * is in at most one of these blocks.
   *
   * This inner class was generated automatically from
   * <code>$ARGV[0]</code>, by some perl scripts.
   * This Unicode definition file can be found on the
   * <a href="http://www.unicode.org">http://www.unicode.org</a> website.
   * JDK 1.4 uses Unicode version 3.0.0.
   *
   * @author scripts/unicode-blocks.pl (written by Eric Blake)
   * @since 1.2
   */
  public static final class UnicodeBlock extends Subset
  {
    /** The start of the subset. */
    private final char start;

    /** The end of the subset. */
    private final char end;

    /**
     * Constructor for strictly defined blocks.
     *
     * @param start the start character of the range
     * @param end the end character of the range
     * @param name the block name
     */
    private UnicodeBlock(char start, char end, String name)
    {
      super(name);
      this.start = start;
      this.end = end;
    }

    /**
     * Returns the Unicode character block which a character belongs to.
     *
     * @param ch the character to look up
     * @return the set it belongs to, or null if it is not in one
     */
    public static UnicodeBlock of(char ch)
    {
      // Special case, since SPECIALS contains two ranges.
      if (ch == '\uFEFF')
        return SPECIALS;
      // Simple binary search for the correct block.
      int low = 0;
      int hi = sets.length - 1;
      while (low <= hi)
        {
          int mid = (low + hi) >> 1;
          UnicodeBlock b = sets[mid];
          if (ch < b.start)
            hi = mid - 1;
          else if (ch > b.end)
            low = mid + 1;
          else
            return b;
        }
      return null;
    }
EOF

my $seenSpecials = 0;
my $seenSurrogates = 0;
my $surrogateStart = 0;
my @names = ();
while (<BLOCKS>) {
    next if /^\#/;
    my ($start, $end, $block) = split(/; /);
    next unless defined $block;
    chomp $block;
    $block =~ s/ *$//;
    if (! $seenSpecials and $block =~ /Specials/) {
        # Special case SPECIALS, since it is two disjoint ranges
        $seenSpecials = 1;
        next;              
    }
    if ($block =~ /Surrogates/) {
        # Special case SURROGATES_AREA, since it one range, not three
        # consecutive, in Java
        $seenSurrogates++;
        if ($seenSurrogates == 1) {
            $surrogateStart = $start;
            next;
        } elsif ($seenSurrogates == 2) {
            next;
        } else {
            $start = $surrogateStart;
            $block = "Surrogates Area";
        }
    }
    # Special case the name of PRIVATE_USE_AREA.
    $block =~ s/(Private Use)/$1 Area/;

    (my $name = $block) =~ tr/a-z -/A-Z__/;
    push @names, $name;
    my $since = (defined $additions{$name}
                 ? "\n     * \@since $additions{$name}" : "");
    my $extra = ($block =~ /Specials/ ? "'\\uFEFF', " : "");
    print <<EOF;

    /**
     * $block.
     * $extra'\\u$start' - '\\u$end'.$since
     */
    public static final UnicodeBlock $name
      = new UnicodeBlock('\\u$start', '\\u$end',
                         "$name");
EOF
}

print <<EOF;

    /**
     * The defined subsets.
     */
    private static final UnicodeBlock sets[] = {
EOF

foreach (@names) {
    print "      $_,\n";
}

print <<EOF;
    };
  } // class UnicodeBlock
EOF
