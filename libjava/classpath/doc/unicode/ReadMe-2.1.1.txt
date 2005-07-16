
UNICODE 2.1 CHARACTER DATABASE

Copyright (c) 1991-1998 Unicode, Inc.
All Rights reserved.

DISCLAIMER

The Unicode Character Database "UNIDAT21.TXT" is provided as-is by
Unicode, Inc. (The Unicode Consortium). No claims are made as to fitness for any
particular purpose. No warranties of any kind are expressed or implied. The
recipient agrees to determine applicability of information provided. If this
file has been purchased on magnetic or optical media from Unicode, Inc.,
the sole remedy for any claim will be exchange of defective media within
90 days of receipt.

This disclaimer is applicable for all other data files accompanying the
Unicode Character Database, some of which have been compiled by the
Unicode Consortium, and some of which have been supplied by other vendors.

LIMITATIONS ON RIGHTS TO REDISTRIBUTE THIS DATA

Recipient is granted the right to make copies in any form for internal
distribution and to freely use the information supplied in the creation of
products supporting the Unicode (TM) Standard. This file can be redistributed
to third parties or other organizations (whether for profit or not) as long
as this notice and the disclaimer notice are retained.

EXPLANATORY INFORMATION

The Unicode Character Database defines the default Unicode character
properties, and internal mappings. Particular implementations may choose to
override the properties and mappings that are not normative. If that is done,
it is up to the implementer to establish a protocol to convey that
information. For more information about character properties and mappings,
see "The Unicode Standard, Worldwide Character Encoding, Version 2.0",
published by Addison-Wesley. For information about other data files
accompanying the Unicode Character Database, see the section of the
Unicode Standard they were extracted from, or the explanatory readme
files and/or header sections with those files.

The Unicode Character Database has been updated to reflect Version 2.1
of the Unicode Standard, with two additional characters added to those
published in Version 2.0:

   U+20AC EURO SIGN
   U+FFFC OBJECT REPLACEMENT CHARACTER

A number of corrections have also been made to case mappings or other
errors in the database noted since the publication of Version 2.0. And
a few normative bidirectional properties have been modified to reflect
decisions of the Unicode Technical Committee.

The Unicode Character Database is a plain ASCII text file consisting of lines
containing fields terminated by semicolons. Each line represents the data for
one encoded character in the Unicode Standard, Version 2.1. Every encoded
character has a data entry, with the exception of certain special ranges, as
detailed below.

There are five special ranges of characters that are represented only by
their start and end characters, since the properties in the file are uniform,
except for code values (which are all sequential and assigned). The names of CJK
ideograph characters and Hangul syllable characters are algorithmically
derivable. (See the Unicode Standard for more information). Surrogate
characters and private use characters have no names.

The exact ranges represented by start and end characters are:

   The CJK Ideographs Area (U+4E00 - U+9FFF)
   The Hangul Syllables Area (U+AC00 - U+D7A3)
   The Surrogates Area (U+D800 - U+DFFF)
   The Private Use Area (U+E000 - U+F8FF)
   CJK Compatibility Ideographs (U+F900 - U+FAFF)

The following table describes the format and meaning of each field in a
data entry in the Unicode Character Database. Fields which contain
normative information are so indicated.

Field	Explanation
-----	-----------

  0	Code value in 4-digit hexadecimal format.
  	This field is normative.

  1	Unicode 2.1 Character Name. These names match exactly the
	names published in Chapter 7 of the Unicode Standard, Version
	2.0, except for the two additional characters.
  	This field is normative.

  2	General Category. This is a useful breakdown into various "character
	types" which can be used as a default categorization in implementations.
 	Some of the values are normative, and some are informative.
 	See below for a brief explanation.

  3	Canonical Combining Classes. The classes used for the
	Canonical Ordering Algorithm in the Unicode Standard. These
	classes are also printed in Chapter 4 of the Unicode Standard.
        This field is normative. See below for a brief explanation.

  4	Bidirectional Category. See the list below for an explanation of the
	abbreviations used in this field. These are the categories required
	by the Bidirectional Behavior Algorithm in the Unicode Standard.
	These categories are summarized in Chapter 4 of the Unicode Standard.
	This field is normative.

  5	Character Decomposition. In the Unicode Standard, not all of
	the decompositions are full decompositions. Recursive
	application of look-up for decompositions will, in all cases, lead to
	a maximal decomposition. The decompositions match exactly the
	decompositions published with the character names in Chapter 7
	of the Unicode Standard. This field is normative.

  6	Decimal digit value. This is a numeric field. If the character
	has the decimal digit property, as specified in Chapter 4 of
	the Unicode Standard, the value of that digit is represented
	with an integer value in this field. This field is normative.

  7	Digit value. This is a numeric field. If the character represents a
	digit, not necessarily a decimal digit, the value is here. This
	covers digits which do not form decimal radix forms, such as the
	compatibility superscript digits. This field is informative.

  8	Numeric value. This is a numeric field. If the character has the
	numeric property, as specified in Chapter 4 of the Unicode
	Standard, the value of that character is represented with an
	integer or rational number in this field. This includes fractions as,
	e.g., "1/5" for U+2155 VULGAR FRACTION ONE FIFTH.
	Also included are numerical values for compatibility characters
	such as circled numbers. This field is normative.

  9	If the characters has been identified as a "mirrored" character in
        bidirectional text, this field has the value "Y"; otherwise "N".
	The list of mirrored characters is also printed in Chapter 4 of
	the Unicode Standard. This field is normative.

 10	Unicode 1.0 Name. This is the old name as published in Unicode 1.0.
	This name is only provided when it is significantly different from
	the Unicode 2.1 name for the character. This field is informative.

 11	10646 Comment field. This field is informative.

 12	Upper case equivalent mapping. If a character is part of an
	alphabet with case distinctions, and has an upper case equivalent,
	then the upper case equivalent is in this field. See the explanation
	below on case distinctions. These mappings are always one-to-one,
	not one-to-many or many-to-one. This field is informative.

 13	Lower case equivalent mapping. Similar to 12. This field is informative.

 14	Title case equivalent mapping. Similar to 12. This field is informative.

GENERAL CATEGORY

The values in this field are abbreviations for the following. Some of the
values are normative, and some are informative. For more information, see
the Unicode Standard. Note: the standard does not assign information to
control characters (except for TAB in the Bidirectonal Algorithm).
Implementations will generally also assign categories to certain control
characters, notably CR and LF, according to platform conventions.


Normative
    Mn = Mark, Non-Spacing
    Mc = Mark, Spacing Combining
    Me = Mark, Enclosing

    Nd = Number, Decimal Digit
    Nl = Number, Letter
    No = Number, Other

    Zs = Separator, Space
    Zl = Separator, Line
    Zp = Separator, Paragraph

    Cc = Other, Control
    Cf = Other, Format
    Cs = Other, Surrogate
    Co = Other, Private Use
    Cn = Other, Not Assigned

Informative
    Lu = Letter, Uppercase
    Ll = Letter, Lowercase
    Lt = Letter, Titlecase
    Lm = Letter, Modifier
    Lo = Letter, Other

    Pc = Punctuation, Connector
    Pd = Punctuation, Dash
    Ps = Punctuation, Open
    Pe = Punctuation, Close
    Po = Punctuation, Other

    Sm = Symbol, Math
    Sc = Symbol, Currency
    Sk = Symbol, Modifier
    So = Symbol, Other

BIDIRECTIONAL PROPERTIES

Please refer to the Unicode Standard for an explanation of the algorithm for
Bidirectional Behavior and an explanation of the sigificance of these categories.
These values are normative.

Strong types:
	L	Left-Right; Most alphabetic, syllabic, and logographic
			characters (e.g., CJK ideographs)
	R	Right-Left; Arabic, Hebrew, and
			punctuation specific to those scripts
Weak types:
	EN	European Number
	ES	European Number Separator
	ET	European Number Terminator
	AN	Arabic Number
	CS	Common Number Separator

Separators:
	B	Block Separator
	S	Segment Separator

Neutrals:
	WS	Whitespace
	ON	Other Neutrals ; All other characters: punctuation, symbols

CHARACTER DECOMPOSITION TAGS

The decomposition is a normative property of a character. The tags supplied
with certain decompositions generally indicate formatting information.
Where no such tag is given, the decomposition is designated as canonical.
Conversely, the presence of a formatting tag also indicates
that the decomposition is a compatibility decomposition and not a canonical
decomposition. In the absence of other formatting information in a
compatibility decomposition, the tag <compat> is used to distinguish it from
canonical decompositions.

In some instances a canonical decomposition or a compatibility decomposition
may consist of a single character. For a canonical decomposition, this
indicates that the character is a canonical equivalent of another single
character. For a compatibility decomposition, this indicates that the
character is a compatibility equivalent of another single character.

The compatibility formatting tags used are:

	<font>		A font variant (e.g. a blackletter form).
	<noBreak>	A no-break version of a space or hyphen.
	<initial>	An initial presentation form (Arabic).
	<medial>	A medial presentation form (Arabic).
	<final>		A final presentation form (Arabic).
	<isolated>	An isolated presentation form (Arabic).
	<circle>	An encircled form.
	<super>		A superscript form.
	<sub>		A subscript form.
	<vertical>	A vertical layout presentation form.
	<wide>		A wide (or zenkaku) compatibility character.
	<narrow>	A narrow (or hankaku) compatibility character.
	<small>		A small variant form (CNS compatibility).
	<square>	A CJK squared font variant.
	<fraction>	A vulgar fraction form.
	<compat>	Otherwise unspecified compatibility character.

CANONICAL COMBINING CLASSES

  0: Spacing, enclosing, reordrant, and surrounding
  1: Overlays and interior
  6: Tibetan subjoined Letters
  7: Nuktas
  8: Hiragana/Katakana voiced marks
  9: Viramas
 10: Start of fixed position classes
199: End of fixed position classes
200: Below left attached
202: Below attached
204: Below right attached
208: Left attached (reordrant around single base character)
210: Right attached
212: Above left attached
214: Above attached
216: Above right attached
218: Below left
220: Below
222: Below right
224: Left (reordrant around single base character)
226: Right
228: Above left
230: Above
232: Above right
234: Double above

Note: some of the combining classes in this list do not currently have
members but are specified here for completeness.

CASE MAPPINGS

In addition to uppercase and lowercase, because of the inclusion of certain
composite characters for compatibility, such as "01F1;LATIN CAPITAL LETTER
DZ", there is a third case, called titlecase, which is used where the first
character of a word is to be capitalized (e.g. UPPERCASE, Titlecase,
lowercase). An example of such a character is "01F2;LATIN CAPITAL LETTER D
WITH SMALL LETTER Z".

The uppercase, titlecase and lowercase fields are only included for characters
that have a single corresponding character of that type. Composite characters
(such as "339D;SQUARE CM") that do not have a single corresponding character
of that type can be cased by decomposition.

The case mapping is an informative, default mapping. Certain languages, such
as Turkish, German, French, or Greek may have small deviations from the
default mappings listed in the Unicode Character Database.

MODIFICATION HISTORY

Modifications made in updating the Unicode Character Database for
the Unicode Standard, Version 2.1 (from Version 2.0) are:
* Added two characters (U+20AC and U+FFFC).
* Amended bidi properties for U+0026, U+002E, U+0040, U+2007.
* Corrected case mappings for U+018E, U+019F, U+01DD, U+0258, U+0275,
	U+03C2, U+1E9B.
* Changed combining order class for U+0F71.
* Corrected canonical decompositions for U+0F73, U+1FBE.
* Changed decomposition for U+FB1F from compatibility to canonical.
* Added compatibility decompositions for U+FBE8, U+FBE9, U+FBF9..U+FBFB.
* Corrected compatibility decompositions for U+2469, U+246A, U+3358.


Some of the modifications made in updating the Unicode Character Database
for the Unicode Standard, Version 2.0 are:
* Fixed decompositions with TONOS to use correct NSM: 030D.
* Removed old Hangul Syllables; mapping to new characters are
	in a separate table.
* Marked compability decompositions with additional tags.
* Changed old tag names for clarity.
* Revision of decompositions to use first-level decomposition, instead
	of maximal decomposition.
* Correction of all known errors in decompositions from earlier versions.
* Added control code names (as old Unicode names).
* Added Hangul Jamo decompositions.
* Added Number category to match properties list in book.
* Fixed categories of Koranic Arabic marks.
* Fixed categories of precomposed characters to match decomposition where possible.
* Added Hebrew cantillation marks and the Tibetan script.
* Added place holders for ranges such as CJK Ideographic Area and the
	Private Use Area.
* Added categories Me, Sk, Pc, Nl, Cs, Cf, and rectified a number of mistakes in the
	database.
