#!/usr/bin/perl -w
# unicode-blocks.pl -- Script to generate java.lang.Character.UnicodeBlock
# Copyright (C) 2002, 2004 Free Software Foundation, Inc.
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


# Code for reading Blocks.txt and generating (to standard out) the code for
# java.lang.Character.UnicodeBlock, for pasting into java/lang/Character.java.
# You should probably check that the results are accurate to the
# specification, but I made sure it works OOB for Unicode 3.0.0 and JDK 1.4.
# As the grammar for the Blocks.txt file is changing in Unicode 3.2.0, you
# will have to tweak this some for future use.  For now, the relevant
# Unicode definition files are found in doc/unicode/.
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
		 "CYRILLIC_SUPPLEMENTARY" => "1.5",
		 "TAGALOG" => "1.5",
		 "HANUNOO" => "1.5",
		 "BUHID" => "1.5",
		 "TAGBANWA" => "1.5",
		 "LIMBU" => "1.5",
		 "TAI_LE" => "1.5",
		 "KHMER_SYMBOLS" => "1.5",
		 "PHONETIC_EXTENSIONS" => "1.5",
		 "MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A" => "1.5",
		 "SUPPLEMENTAL_ARROWS_A" => "1.5",
		 "SUPPLEMENTAL_ARROWS_B" => "1.5",
		 "MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B" => "1.5",
		 "SUPPLEMENTAL_MATHEMATICAL_OPERATORS" => "1.5",
		 "MISCELLANEOUS_SYMBOLS_AND_ARROWS" => "1.5",
		 "KATAKANA_PHONETIC_EXTENSIONS" => "1.5",
		 "YIJING_HEXAGRAM_SYMBOLS" => "1.5",
		 "VARIATION_SELECTORS" => "1.5",
		 "LINEAR_B_SYLLABARY" => "1.5",
		 "LINEAR_B_IDEOGRAMS" => "1.5",
		 "AEGEAN_NUMBERS" => "1.5",
		 "OLD_ITALIC" => "1.5",
		 "GOTHIC" => "1.5",
		 "UGARITIC" => "1.5",
		 "DESERET" => "1.5",
		 "SHAVIAN" => "1.5",
		 "OSMANYA" => "1.5",
		 "CYPRIOT_SYLLABARY" => "1.5",
		 "BYZANTINE_MUSICAL_SYMBOLS" => "1.5",
		 "MUSICAL_SYMBOLS" => "1.5",
		 "TAI_XUAN_JING_SYMBOLS" => "1.5",
		 "MATHEMATICAL_ALPHANUMERIC_SYMBOLS" => "1.5",
		 "CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B" => "1.5",
		 "CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT" => "1.5",
		 "TAGS" => "1.5",
		 "VARIATION_SELECTORS_SUPPLEMENT" => "1.5",
		 "SUPPLEMENTARY_PRIVATE_USE_AREA_A" => "1.5",
		 "SUPPLEMENTARY_PRIVATE_USE_AREA_B" => "1.5",
		 "HIGH_SURROGATES" => "1.5",
		 "HIGH_PRIVATE_USE_SURROGATES" => "1.5",
		 "LOW_SURROGATES" => "1.5"
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
   * JDK 1.5 uses Unicode version 4.0.0.
   *
   * @author scripts/unicode-blocks.pl (written by Eric Blake)
   * @since 1.2
   */
  public static final class UnicodeBlock extends Subset
  {
    /** The start of the subset. */
    private final int start;

    /** The end of the subset. */
    private final int end;

    /** The canonical name of the block according to the Unicode standard. */
    private final String canonicalName;

    /** Constants for the <code>forName()</code> method */
    private static final int CANONICAL_NAME = 0;
    private static final int NO_SPACES_NAME = 1;
    private static final int CONSTANT_NAME = 2;

    /**
     * Constructor for strictly defined blocks.
     *
     * @param start the start character of the range
     * @param end the end character of the range
     * @param name the block name
     * @param canonicalName the name of the block as defined in the Unicode
     *        standard.
     */
    private UnicodeBlock(int start, int end, String name,
			 String canonicalName)
    {
      super(name);
      this.start = start;
      this.end = end;
      this.canonicalName = canonicalName;
    }

    /**
     * Returns the Unicode character block which a character belongs to.
     * <strong>Note</strong>: This method does not support the use of
     * supplementary characters.  For such support, <code>of(int)</code>
     * should be used instead.
     *
     * @param ch the character to look up
     * @return the set it belongs to, or null if it is not in one
     */
    public static UnicodeBlock of(char ch)
    {
      return of((int) ch);
    }

    /**
     * Returns the Unicode character block which a code point belongs to.
     *
     * @param codePoint the character to look up
     * @return the set it belongs to, or null if it is not in one.
     * @throws IllegalArgumentException if the specified code point is
     *         invalid.
     * @since 1.5
     */
    public static UnicodeBlock of(int codePoint)
    {
      if (codePoint > MAX_CODE_POINT)
	throw new IllegalArgumentException("The supplied integer value is " +
					   "too large to be a codepoint.");
      // Simple binary search for the correct block.
      int low = 0;
      int hi = sets.length - 1;
      while (low <= hi)
        {
          int mid = (low + hi) >> 1;
          UnicodeBlock b = sets[mid];
          if (codePoint < b.start)
            hi = mid - 1;
          else if (codePoint > b.end)
            low = mid + 1;
          else
            return b;
        }
      return null;
    }

    /**
     * <p>
     * Returns the <code>UnicodeBlock</code> with the given name, as defined
     * by the Unicode standard.  The version of Unicode in use is defined by
     * the <code>Character</code> class, and the names are given in the
     * <code>Blocks-<version>.txt</code> file corresponding to that version.
     * The name may be specified in one of three ways:
     * </p>
     * <ol>
     * <li>The canonical, human-readable name used by the Unicode standard.
     * This is the name with all spaces and hyphens retained.  For example,
     * `Basic Latin' retrieves the block, UnicodeBlock.BASIC_LATIN.</li>
     * <li>The canonical name with all spaces removed e.g. `BasicLatin'.</li>
     * <li>The name used for the constants specified by this class, which
     * is the canonical name with all spaces and hyphens replaced with
     * underscores e.g. `BASIC_LATIN'</li>
     * </ol>
     * <p>
     * The names are compared case-insensitively using the case comparison
     * associated with the U.S. English locale.  The method recognises the
     * previous names used for blocks as well as the current ones.  At
     * present, this simply means that the deprecated `SURROGATES_AREA'
     * will be recognised by this method (the <code>of()</code> methods
     * only return one of the three new surrogate blocks).
     * </p>
     *
     * @param blockName the name of the block to look up.
     * @return the specified block.
     * @throws NullPointerException if the <code>blockName</code> is
     *         <code>null</code>.
     * @throws IllegalArgumentException if the name does not match any Unicode
     *         block.
     * @since 1.5
     */
    public static final UnicodeBlock forName(String blockName)
    {
      int type;
      if (blockName.indexOf(' ') != -1)
        type = CANONICAL_NAME;
      else if (blockName.indexOf('_') != -1)
        type = CONSTANT_NAME;
      else
        type = NO_SPACES_NAME;
      Collator usCollator = Collator.getInstance(Locale.US);
      usCollator.setStrength(Collator.PRIMARY);
      /* Special case for deprecated blocks not in sets */
      switch (type)
      {
        case CANONICAL_NAME:
          if (usCollator.compare(blockName, "Surrogates Area") == 0)
            return SURROGATES_AREA;
          break;
        case NO_SPACES_NAME:
          if (usCollator.compare(blockName, "SurrogatesArea") == 0)
            return SURROGATES_AREA;
          break;
        case CONSTANT_NAME:
          if (usCollator.compare(blockName, "SURROGATES_AREA") == 0) 
            return SURROGATES_AREA;
          break;
      }
      /* Other cases */
      int setLength = sets.length;
      switch (type)
      {
        case CANONICAL_NAME:
          for (int i = 0; i < setLength; i++)
            {
              UnicodeBlock block = sets[i];
              if (usCollator.compare(blockName, block.canonicalName) == 0)
                return block;
            }
          break;
        case NO_SPACES_NAME:
          for (int i = 0; i < setLength; i++)
            {
              UnicodeBlock block = sets[i];
	      String nsName = block.canonicalName.replaceAll(" ","");
	      if (usCollator.compare(blockName, nsName) == 0)
		return block;
	    }
	  break;
        case CONSTANT_NAME:
          for (int i = 0; i < setLength; i++)
            {
              UnicodeBlock block = sets[i];
              if (usCollator.compare(blockName, block.toString()) == 0)
                return block;
	    }
          break;
      }
      throw new IllegalArgumentException("No Unicode block found for " +
                                         blockName + ".");
    }
EOF

my @names = ();
while (<BLOCKS>) {
    next if /^\#/;
    my ($range, $block) = split(/; /);
    my ($start, $end) = split /\.\./, $range;
    next unless defined $block;
    chomp $block;
    $block =~ s/ *$//;

    # Translate new Unicode names which have the old name in Java
    $block = "Greek" if $block =~ /Greek and Coptic/;
    $block = "Combining Marks for Symbols" 
      if $block =~ /Combining Diacritical Marks for Symbols/;
 
    (my $name = $block) =~ tr/a-z -/A-Z__/;
    push @names, $name;
    my $since = (defined $additions{$name}
                 ? "\n     * \@since $additions{$name}" : "");
    print <<EOF;

    /**
     * $block.
     * 0x$start - 0x$end.$since
     */
    public static final UnicodeBlock $name
      = new UnicodeBlock(0x$start, 0x$end,
                         "$name", 
                         "$block");
EOF
}

print <<EOF;

    /**
     * Surrogates Area.
     * '\uD800' - '\uDFFF'.
     * \@deprecated As of 1.5, the three areas, 
     * <a href="#HIGH_SURROGATES">HIGH_SURROGATES</a>,
     * <a href="#HIGH_PRIVATE_USE_SURROGATES">HIGH_PRIVATE_USE_SURROGATES</a>
     * and <a href="#LOW_SURROGATES">LOW_SURROGATES</a>, as defined
     * by the Unicode standard, should be used in preference to
     * this.  These are also returned from calls to <code>of(int)</code>
     * and <code>of(char)</code>.
     */
    \@Deprecated
    public static final UnicodeBlock SURROGATES_AREA
      = new UnicodeBlock(0xD800, 0xDFFF,
                         "SURROGATES_AREA",
			 "Surrogates Area");

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
