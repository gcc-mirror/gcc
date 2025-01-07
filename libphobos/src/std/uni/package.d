// Written in the D programming language.

/++
    $(P The `std.uni` module provides an implementation
    of fundamental Unicode algorithms and data structures.
    This doesn't include UTF encoding and decoding primitives,
    see $(REF decode, std,_utf) and $(REF encode, std,_utf) in $(MREF std, utf)
    for this functionality. )

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Decode) $(TD
    $(LREF byCodePoint)
    $(LREF byGrapheme)
    $(LREF decodeGrapheme)
    $(LREF graphemeStride)
    $(LREF popGrapheme)
))
$(TR $(TD Comparison) $(TD
    $(LREF icmp)
    $(LREF sicmp)
))
$(TR $(TD Classification) $(TD
    $(LREF isAlpha)
    $(LREF isAlphaNum)
    $(LREF isCodepointSet)
    $(LREF isControl)
    $(LREF isFormat)
    $(LREF isGraphical)
    $(LREF isIntegralPair)
    $(LREF isMark)
    $(LREF isNonCharacter)
    $(LREF isNumber)
    $(LREF isPrivateUse)
    $(LREF isPunctuation)
    $(LREF isSpace)
    $(LREF isSurrogate)
    $(LREF isSurrogateHi)
    $(LREF isSurrogateLo)
    $(LREF isSymbol)
    $(LREF isWhite)
))
$(TR $(TD Normalization) $(TD
    $(LREF NFC)
    $(LREF NFD)
    $(LREF NFKD)
    $(LREF NormalizationForm)
    $(LREF normalize)
))
$(TR $(TD Decompose) $(TD
    $(LREF decompose)
    $(LREF decomposeHangul)
    $(LREF UnicodeDecomposition)
))
$(TR $(TD Compose) $(TD
    $(LREF compose)
    $(LREF composeJamo)
))
$(TR $(TD Sets) $(TD
    $(LREF CodepointInterval)
    $(LREF CodepointSet)
    $(LREF InversionList)
    $(LREF unicode)
))
$(TR $(TD Trie) $(TD
    $(LREF codepointSetTrie)
    $(LREF CodepointSetTrie)
    $(LREF codepointTrie)
    $(LREF CodepointTrie)
    $(LREF toTrie)
    $(LREF toDelegate)
))
$(TR $(TD Casing) $(TD
    $(LREF asCapitalized)
    $(LREF asLowerCase)
    $(LREF asUpperCase)
    $(LREF isLower)
    $(LREF isUpper)
    $(LREF toLower)
    $(LREF toLowerInPlace)
    $(LREF toUpper)
    $(LREF toUpperInPlace)
))
$(TR $(TD Utf8Matcher) $(TD
    $(LREF isUtfMatcher)
    $(LREF MatcherConcept)
    $(LREF utfMatcher)
))
$(TR $(TD Separators) $(TD
    $(LREF lineSep)
    $(LREF nelSep)
    $(LREF paraSep)
))
$(TR $(TD Building blocks) $(TD
    $(LREF allowedIn)
    $(LREF combiningClass)
    $(LREF Grapheme)
))
))

    $(P All primitives listed operate on Unicode characters and
        sets of characters. For functions which operate on ASCII characters
        and ignore Unicode $(CHARACTERS), see $(MREF std, ascii).
        For definitions of Unicode $(CHARACTER), $(CODEPOINT) and other terms
        used throughout this module see the $(S_LINK Terminology, terminology) section
        below.
    )
    $(P The focus of this module is the core needs of developing Unicode-aware
        applications. To that effect it provides the following optimized primitives:
    )
    $(UL
        $(LI Character classification by category and common properties:
            $(LREF isAlpha), $(LREF isWhite) and others.
        )
        $(LI
            Case-insensitive string comparison ($(LREF sicmp), $(LREF icmp)).
        )
        $(LI
            Converting text to any of the four normalization forms via $(LREF normalize).
        )
        $(LI
            Decoding ($(LREF decodeGrapheme))  and iteration ($(LREF byGrapheme), $(LREF graphemeStride))
            by user-perceived characters, that is by $(LREF Grapheme) clusters.
        )
        $(LI
            Decomposing and composing of individual character(s) according to canonical
            or compatibility rules, see $(LREF compose) and $(LREF decompose),
            including the specific version for Hangul syllables $(LREF composeJamo)
            and $(LREF decomposeHangul).
        )
    )
    $(P It's recognized that an application may need further enhancements
        and extensions, such as less commonly known algorithms,
        or tailoring existing ones for region specific needs. To help users
        with building any extra functionality beyond the core primitives,
        the module provides:
    )
    $(UL
        $(LI
            $(LREF CodepointSet), a type for easy manipulation of sets of characters.
            Besides the typical set algebra it provides an unusual feature:
            a D source code generator for detection of $(CODEPOINTS) in this set.
            This is a boon for meta-programming parser frameworks,
            and is used internally to power classification in small
            sets like $(LREF isWhite).
        )
        $(LI
            A way to construct optimal packed multi-stage tables also known as a
            special case of $(LINK2 https://en.wikipedia.org/wiki/Trie, Trie).
            The functions $(LREF codepointTrie), $(LREF codepointSetTrie)
            construct custom tries that map dchar to value.
            The end result is a fast and predictable $(BIGOH 1) lookup that powers
            functions like $(LREF isAlpha) and $(LREF combiningClass),
            but for user-defined data sets.
        )
        $(LI
            A useful technique for Unicode-aware parsers that perform
            character classification of encoded $(CODEPOINTS)
            is to avoid unnecassary decoding at all costs.
            $(LREF utfMatcher) provides an improvement over the usual workflow
            of decode-classify-process, combining the decoding and classification
            steps. By extracting necessary bits directly from encoded
            $(S_LINK Code unit, code units) matchers achieve
            significant performance improvements. See $(LREF MatcherConcept) for
            the common interface of UTF matchers.
        )
        $(LI
            Generally useful building blocks for customized normalization:
            $(LREF combiningClass) for querying combining class
            and $(LREF allowedIn) for testing the Quick_Check
            property of a given normalization form.
        )
        $(LI
            Access to a large selection of commonly used sets of $(CODEPOINTS).
            $(S_LINK Unicode properties, Supported sets) include Script,
            Block and General Category. The exact contents of a set can be
            observed in the CLDR utility, on the
            $(HTTP www.unicode.org/cldr/utility/properties.jsp, property index) page
            of the Unicode website.
            See $(LREF unicode) for easy and (optionally) compile-time checked set
            queries.
        )
    )
    $(SECTION Synopsis)
    ---
    import std.uni;
    void main()
    {
        // initialize code point sets using script/block or property name
        // now 'set' contains code points from both scripts.
        auto set = unicode("Cyrillic") | unicode("Armenian");
        // same thing but simpler and checked at compile-time
        auto ascii = unicode.ASCII;
        auto currency = unicode.Currency_Symbol;

        // easy set ops
        auto a = set & ascii;
        assert(a.empty); // as it has no intersection with ascii
        a = set | ascii;
        auto b = currency - a; // subtract all ASCII, Cyrillic and Armenian

        // some properties of code point sets
        assert(b.length > 45); // 46 items in Unicode 6.1, even more in 6.2
        // testing presence of a code point in a set
        // is just fine, it is O(logN)
        assert(!b['$']);
        assert(!b['\u058F']); // Armenian dram sign
        assert(b['¥']);

        // building fast lookup tables, these guarantee O(1) complexity
        // 1-level Trie lookup table essentially a huge bit-set ~262Kb
        auto oneTrie = toTrie!1(b);
        // 2-level far more compact but typically slightly slower
        auto twoTrie = toTrie!2(b);
        // 3-level even smaller, and a bit slower yet
        auto threeTrie = toTrie!3(b);
        assert(oneTrie['£']);
        assert(twoTrie['£']);
        assert(threeTrie['£']);

        // build the trie with the most sensible trie level
        // and bind it as a functor
        auto cyrillicOrArmenian = toDelegate(set);
        auto balance = find!(cyrillicOrArmenian)("Hello ընկեր!");
        assert(balance == "ընկեր!");
        // compatible with bool delegate(dchar)
        bool delegate(dchar) bindIt = cyrillicOrArmenian;

        // Normalization
        string s = "Plain ascii (and not only), is always normalized!";
        assert(s is normalize(s));// is the same string

        string nonS = "A\u0308ffin"; // A ligature
        auto nS = normalize(nonS); // to NFC, the W3C endorsed standard
        assert(nS == "Äffin");
        assert(nS != nonS);
        string composed = "Äffin";

        assert(normalize!NFD(composed) == "A\u0308ffin");
        // to NFKD, compatibility decomposition useful for fuzzy matching/searching
        assert(normalize!NFKD("2¹⁰") == "210");
    }
    ---
    $(SECTION Terminology)
    $(P The following is a list of important Unicode notions
    and definitions. Any conventions used specifically in this
    module alone are marked as such. The descriptions are based on the formal
    definition as found in $(HTTP www.unicode.org/versions/Unicode6.2.0/ch03.pdf,
    chapter three of The Unicode Standard Core Specification.)
    )
    $(P $(DEF Abstract character) A unit of information used for the organization,
        control, or representation of textual data.
        Note that:
        $(UL
            $(LI When representing data, the nature of that data
                is generally symbolic as opposed to some other
                kind of data (for example, visual).
            )
             $(LI An abstract character has no concrete form
                and should not be confused with a $(S_LINK Glyph, glyph).
            )
            $(LI An abstract character does not necessarily
                correspond to what a user thinks of as a “character”
                and should not be confused with a $(LREF Grapheme).
            )
            $(LI The abstract characters encoded (see Encoded character)
                are known as Unicode abstract characters.
            )
            $(LI Abstract characters not directly
                encoded by the Unicode Standard can often be
                represented by the use of combining character sequences.
            )
        )
    )
    $(P $(DEF Canonical decomposition)
        The decomposition of a character or character sequence
        that results from recursively applying the canonical
        mappings found in the Unicode Character Database
        and these described in Conjoining Jamo Behavior
        (section 12 of
        $(HTTP www.unicode.org/uni2book/ch03.pdf, Unicode Conformance)).
    )
    $(P $(DEF Canonical composition)
        The precise definition of the Canonical composition
        is the algorithm as specified in $(HTTP www.unicode.org/uni2book/ch03.pdf,
        Unicode Conformance) section 11.
        Informally it's the process that does the reverse of the canonical
        decomposition with the addition of certain rules
        that e.g. prevent legacy characters from appearing in the composed result.
    )
    $(P $(DEF Canonical equivalent)
        Two character sequences are said to be canonical equivalents if
        their full canonical decompositions are identical.
    )
    $(P $(DEF Character) Typically differs by context.
        For the purpose of this documentation the term $(I character)
        implies $(I encoded character), that is, a code point having
        an assigned abstract character (a symbolic meaning).
    )
    $(P $(DEF Code point) Any value in the Unicode codespace;
        that is, the range of integers from 0 to 10FFFF (hex).
        Not all code points are assigned to encoded characters.
    )
    $(P $(DEF Code unit) The minimal bit combination that can represent
        a unit of encoded text for processing or interchange.
        Depending on the encoding this could be:
        8-bit code units in the UTF-8 (`char`),
        16-bit code units in the UTF-16 (`wchar`),
        and 32-bit code units in the UTF-32 (`dchar`).
        $(I Note that in UTF-32, a code unit is a code point
        and is represented by the D `dchar` type.)
    )
    $(P $(DEF Combining character) A character with the General Category
        of Combining Mark(M).
        $(UL
            $(LI All characters with non-zero canonical combining class
            are combining characters, but the reverse is not the case:
            there are combining characters with a zero combining class.
            )
            $(LI These characters are not normally used in isolation
            unless they are being described. They include such characters
            as accents, diacritics, Hebrew points, Arabic vowel signs,
            and Indic matras.
            )
        )
    )
    $(P $(DEF Combining class)
        A numerical value used by the Unicode Canonical Ordering Algorithm
        to determine which sequences of combining marks are to be
        considered canonically equivalent and  which are not.
    )
    $(P $(DEF Compatibility decomposition)
        The decomposition of a character or character sequence that results
        from recursively applying both the compatibility mappings and
        the canonical mappings found in the Unicode Character Database, and those
        described in Conjoining Jamo Behavior no characters
        can be further decomposed.
    )
    $(P $(DEF Compatibility equivalent)
        Two character sequences are said to be compatibility
        equivalents if their full compatibility decompositions are identical.
    )
    $(P $(DEF Encoded character) An association (or mapping)
        between an abstract character and a code point.
    )
    $(P $(DEF Glyph) The actual, concrete image of a glyph representation
        having been rasterized or otherwise imaged onto some display surface.
    )
    $(P $(DEF Grapheme base) A character with the property
        Grapheme_Base, or any standard Korean syllable block.
    )
    $(P $(DEF Grapheme cluster) Defined as the text between
        grapheme boundaries  as specified by Unicode Standard Annex #29,
        $(HTTP www.unicode.org/reports/tr29/, Unicode text segmentation).
        Important general properties of a grapheme:
        $(UL
            $(LI The grapheme cluster represents a horizontally segmentable
            unit of text, consisting of some grapheme base (which may
            consist of a Korean syllable) together with any number of
            nonspacing marks applied to it.
            )
            $(LI  A grapheme cluster typically starts with a grapheme base
            and then extends across any subsequent sequence of nonspacing marks.
            A grapheme cluster is most directly relevant to text rendering and
            processes such as cursor placement and text selection in editing,
            but may also be relevant to comparison and searching.
            )
            $(LI For many processes, a grapheme cluster behaves as if it was a
            single character with the same properties as its grapheme base.
            Effectively, nonspacing marks apply $(I graphically) to the base,
            but do not change its properties.
            )
        )
        $(P This module defines a number of primitives that work with graphemes:
        $(LREF Grapheme), $(LREF decodeGrapheme) and $(LREF graphemeStride).
        All of them are using $(I extended grapheme) boundaries
        as defined in the aforementioned standard annex.
        )
    )
    $(P $(DEF Nonspacing mark) A combining character with the
        General Category of Nonspacing Mark (Mn) or Enclosing Mark (Me).
    )
    $(P $(DEF Spacing mark) A combining character that is not a nonspacing mark.
    )
    $(SECTION Normalization)
    $(P The concepts of $(S_LINK Canonical equivalent, canonical equivalent)
        or $(S_LINK Compatibility equivalent, compatibility equivalent)
        characters in the Unicode Standard make it necessary to have a full, formal
        definition of equivalence for Unicode strings.
        String equivalence is determined by a process called normalization,
        whereby strings are converted into forms which are compared
        directly for identity. This is the primary goal of the normalization process,
        see the function $(LREF normalize) to convert into any of
        the four defined forms.
    )
    $(P A very important attribute of the Unicode Normalization Forms
        is that they must remain stable between versions of the Unicode Standard.
        A Unicode string normalized to a particular Unicode Normalization Form
        in one version of the standard is guaranteed to remain in that Normalization
        Form for implementations of future versions of the standard.
    )
    $(P The Unicode Standard specifies four normalization forms.
        Informally, two of these forms are defined by maximal decomposition
        of equivalent sequences, and two of these forms are defined
        by maximal $(I composition) of equivalent sequences.
            $(UL
            $(LI Normalization Form D (NFD): The $(S_LINK Canonical decomposition,
                canonical decomposition) of a character sequence.)
            $(LI Normalization Form KD (NFKD): The $(S_LINK Compatibility decomposition,
                compatibility decomposition) of a character sequence.)
            $(LI Normalization Form C (NFC): The canonical composition of the
                $(S_LINK Canonical decomposition, canonical decomposition)
                of a coded character sequence.)
            $(LI Normalization Form KC (NFKC): The canonical composition
            of the $(S_LINK Compatibility decomposition,
                compatibility decomposition) of a character sequence)
            )
    )
    $(P The choice of the normalization form depends on the particular use case.
        NFC is the best form for general text, since it's more compatible with
        strings converted from legacy encodings. NFKC is the preferred form for
        identifiers, especially where there are security concerns. NFD and NFKD
        are the most useful for internal processing.
    )
    $(SECTION Construction of lookup tables)
    $(P The Unicode standard describes a set of algorithms that
        depend on having the ability to quickly look up various properties
        of a code point. Given the codespace of about 1 million $(CODEPOINTS),
        it is not a trivial task to provide a space-efficient solution for
        the multitude of properties.
    )
    $(P Common approaches such as hash-tables or binary search over
        sorted code point intervals (as in $(LREF InversionList)) are insufficient.
        Hash-tables have enormous memory footprint and binary search
        over intervals is not fast enough for some heavy-duty algorithms.
    )
    $(P The recommended solution (see Unicode Implementation Guidelines)
        is using multi-stage tables that are an implementation of the
        $(HTTP en.wikipedia.org/wiki/Trie, Trie) data structure with integer
        keys and a fixed number of stages. For the remainder of the section
        this will be called a fixed trie. The following describes a particular
        implementation that is aimed for the speed of access at the expense
        of ideal size savings.
    )
    $(P Taking a 2-level Trie as an example the principle of operation is as follows.
        Split the number of bits in a key (code point, 21 bits) into 2 components
        (e.g. 15 and 8).  The first is the number of bits in the index of the trie
         and the other is number of bits in each page of the trie.
        The layout of the trie is then an array of size 2^^bits-of-index followed
        an array of memory chunks of size 2^^bits-of-page/bits-per-element.
    )
    $(P The number of pages is variable (but not less then 1)
        unlike the number of entries in the index. The slots of the index
        all have to contain a number of a page that is present. The lookup is then
        just a couple of operations - slice the upper bits,
        lookup an index for these, take a page at this index and use
        the lower bits as an offset within this page.

        Assuming that pages are laid out consequently
        in one array at `pages`, the pseudo-code is:
    )
    ---
    auto elemsPerPage = (2 ^^ bits_per_page) / Value.sizeOfInBits;
    pages[index[n >> bits_per_page]][n & (elemsPerPage - 1)];
    ---
    $(P Where if `elemsPerPage` is a power of 2 the whole process is
        a handful of simple instructions and 2 array reads. Subsequent levels
        of the trie are introduced by recursing on this notion - the index array
        is treated as values. The number of bits in index is then again
        split into 2 parts, with pages over 'current-index' and the new 'upper-index'.
    )

    $(P For completeness a level 1 trie is simply an array.
        The current implementation takes advantage of bit-packing values
        when the range is known to be limited in advance (such as `bool`).
        See also $(LREF BitPacked) for enforcing it manually.
        The major size advantage however comes from the fact
        that multiple $(B identical pages on every level are merged) by construction.
    )
    $(P The process of constructing a trie is more involved and is hidden from
        the user in a form of the convenience functions $(LREF codepointTrie),
        $(LREF codepointSetTrie) and the even more convenient $(LREF toTrie).
        In general a set or built-in AA with `dchar` type
        can be turned into a trie. The trie object in this module
        is read-only (immutable); it's effectively frozen after construction.
    )
    $(SECTION Unicode properties)
    $(P This is a full list of Unicode properties accessible through $(LREF unicode)
        with specific helpers per category nested within. Consult the
        $(HTTP www.unicode.org/cldr/utility/properties.jsp, CLDR utility)
        when in doubt about the contents of a particular set.
    )
    $(P General category sets listed below are only accessible with the
        $(LREF unicode) shorthand accessor.)
        $(BOOKTABLE $(B General category ),
             $(TR $(TH Abb.) $(TH Long form)
                $(TH Abb.) $(TH Long form)$(TH Abb.) $(TH Long form))
            $(TR $(TD L) $(TD Letter)
                $(TD Cn) $(TD Unassigned)  $(TD Po) $(TD Other_Punctuation))
            $(TR $(TD Ll) $(TD Lowercase_Letter)
                $(TD Co) $(TD Private_Use) $(TD Ps) $(TD Open_Punctuation))
            $(TR $(TD Lm) $(TD Modifier_Letter)
                $(TD Cs) $(TD Surrogate)   $(TD S) $(TD Symbol))
            $(TR $(TD Lo) $(TD Other_Letter)
                $(TD N) $(TD Number)  $(TD Sc) $(TD Currency_Symbol))
            $(TR $(TD Lt) $(TD Titlecase_Letter)
              $(TD Nd) $(TD Decimal_Number)  $(TD Sk) $(TD Modifier_Symbol))
            $(TR $(TD Lu) $(TD Uppercase_Letter)
              $(TD Nl) $(TD Letter_Number)   $(TD Sm) $(TD Math_Symbol))
            $(TR $(TD M) $(TD Mark)
              $(TD No) $(TD Other_Number)    $(TD So) $(TD Other_Symbol))
            $(TR $(TD Mc) $(TD Spacing_Mark)
              $(TD P) $(TD Punctuation) $(TD Z) $(TD Separator))
            $(TR $(TD Me) $(TD Enclosing_Mark)
              $(TD Pc) $(TD Connector_Punctuation)   $(TD Zl) $(TD Line_Separator))
            $(TR $(TD Mn) $(TD Nonspacing_Mark)
              $(TD Pd) $(TD Dash_Punctuation)    $(TD Zp) $(TD Paragraph_Separator))
            $(TR $(TD C) $(TD Other)
              $(TD Pe) $(TD Close_Punctuation) $(TD Zs) $(TD Space_Separator))
            $(TR $(TD Cc) $(TD Control) $(TD Pf)
              $(TD Final_Punctuation)   $(TD -) $(TD Any))
            $(TR $(TD Cf) $(TD Format)
              $(TD Pi) $(TD Initial_Punctuation) $(TD -) $(TD ASCII))
    )
    $(P Sets for other commonly useful properties that are
        accessible with $(LREF unicode):)
        $(BOOKTABLE $(B Common binary properties),
            $(TR $(TH Name) $(TH Name) $(TH Name))
            $(TR $(TD Alphabetic)  $(TD Ideographic) $(TD Other_Uppercase))
            $(TR $(TD ASCII_Hex_Digit) $(TD IDS_Binary_Operator) $(TD Pattern_Syntax))
            $(TR $(TD Bidi_Control)    $(TD ID_Start)    $(TD Pattern_White_Space))
            $(TR $(TD Cased)   $(TD IDS_Trinary_Operator)    $(TD Quotation_Mark))
            $(TR $(TD Case_Ignorable)  $(TD Join_Control)    $(TD Radical))
            $(TR $(TD Dash)    $(TD Logical_Order_Exception) $(TD Soft_Dotted))
            $(TR $(TD Default_Ignorable_Code_Point)    $(TD Lowercase)   $(TD STerm))
            $(TR $(TD Deprecated)  $(TD Math)    $(TD Terminal_Punctuation))
            $(TR $(TD Diacritic)   $(TD Noncharacter_Code_Point) $(TD Unified_Ideograph))
            $(TR $(TD Extender)    $(TD Other_Alphabetic)    $(TD Uppercase))
            $(TR $(TD Grapheme_Base)   $(TD Other_Default_Ignorable_Code_Point)  $(TD Variation_Selector))
            $(TR $(TD Grapheme_Extend) $(TD Other_Grapheme_Extend)   $(TD White_Space))
            $(TR $(TD Grapheme_Link)   $(TD Other_ID_Continue)   $(TD XID_Continue))
            $(TR $(TD Hex_Digit)   $(TD Other_ID_Start)  $(TD XID_Start))
            $(TR $(TD Hyphen)  $(TD Other_Lowercase) )
            $(TR $(TD ID_Continue) $(TD Other_Math)  )
    )
    $(P Below is the table with block names accepted by $(LREF unicode.block).
        Note that the shorthand version $(LREF unicode) requires "In"
        to be prepended to the names of blocks so as to disambiguate
        scripts and blocks.
    )
    $(BOOKTABLE $(B Blocks),
        $(TR $(TD Aegean Numbers)    $(TD Ethiopic Extended) $(TD Mongolian))
        $(TR $(TD Alchemical Symbols)    $(TD Ethiopic Extended-A)   $(TD Musical Symbols))
        $(TR $(TD Alphabetic Presentation Forms) $(TD Ethiopic Supplement)   $(TD Myanmar))
        $(TR $(TD Ancient Greek Musical Notation)    $(TD General Punctuation)   $(TD Myanmar Extended-A))
        $(TR $(TD Ancient Greek Numbers) $(TD Geometric Shapes)  $(TD New Tai Lue))
        $(TR $(TD Ancient Symbols)   $(TD Georgian)  $(TD NKo))
        $(TR $(TD Arabic)    $(TD Georgian Supplement)   $(TD Number Forms))
        $(TR $(TD Arabic Extended-A) $(TD Glagolitic)    $(TD Ogham))
        $(TR $(TD Arabic Mathematical Alphabetic Symbols)    $(TD Gothic)    $(TD Ol Chiki))
        $(TR $(TD Arabic Presentation Forms-A)   $(TD Greek and Coptic)  $(TD Old Italic))
        $(TR $(TD Arabic Presentation Forms-B)   $(TD Greek Extended)    $(TD Old Persian))
        $(TR $(TD Arabic Supplement) $(TD Gujarati)  $(TD Old South Arabian))
        $(TR $(TD Armenian)  $(TD Gurmukhi)  $(TD Old Turkic))
        $(TR $(TD Arrows)    $(TD Halfwidth and Fullwidth Forms) $(TD Optical Character Recognition))
        $(TR $(TD Avestan)   $(TD Hangul Compatibility Jamo) $(TD Oriya))
        $(TR $(TD Balinese)  $(TD Hangul Jamo)   $(TD Osmanya))
        $(TR $(TD Bamum) $(TD Hangul Jamo Extended-A)    $(TD Phags-pa))
        $(TR $(TD Bamum Supplement)  $(TD Hangul Jamo Extended-B)    $(TD Phaistos Disc))
        $(TR $(TD Basic Latin)   $(TD Hangul Syllables)  $(TD Phoenician))
        $(TR $(TD Batak) $(TD Hanunoo)   $(TD Phonetic Extensions))
        $(TR $(TD Bengali)   $(TD Hebrew)    $(TD Phonetic Extensions Supplement))
        $(TR $(TD Block Elements)    $(TD High Private Use Surrogates)   $(TD Playing Cards))
        $(TR $(TD Bopomofo)  $(TD High Surrogates)   $(TD Private Use Area))
        $(TR $(TD Bopomofo Extended) $(TD Hiragana)  $(TD Rejang))
        $(TR $(TD Box Drawing)   $(TD Ideographic Description Characters)    $(TD Rumi Numeral Symbols))
        $(TR $(TD Brahmi)    $(TD Imperial Aramaic)  $(TD Runic))
        $(TR $(TD Braille Patterns)  $(TD Inscriptional Pahlavi) $(TD Samaritan))
        $(TR $(TD Buginese)  $(TD Inscriptional Parthian)    $(TD Saurashtra))
        $(TR $(TD Buhid) $(TD IPA Extensions)    $(TD Sharada))
        $(TR $(TD Byzantine Musical Symbols) $(TD Javanese)  $(TD Shavian))
        $(TR $(TD Carian)    $(TD Kaithi)    $(TD Sinhala))
        $(TR $(TD Chakma)    $(TD Kana Supplement)   $(TD Small Form Variants))
        $(TR $(TD Cham)  $(TD Kanbun)    $(TD Sora Sompeng))
        $(TR $(TD Cherokee)  $(TD Kangxi Radicals)   $(TD Spacing Modifier Letters))
        $(TR $(TD CJK Compatibility) $(TD Kannada)   $(TD Specials))
        $(TR $(TD CJK Compatibility Forms)   $(TD Katakana)  $(TD Sundanese))
        $(TR $(TD CJK Compatibility Ideographs)  $(TD Katakana Phonetic Extensions)  $(TD Sundanese Supplement))
        $(TR $(TD CJK Compatibility Ideographs Supplement)   $(TD Kayah Li)  $(TD Superscripts and Subscripts))
        $(TR $(TD CJK Radicals Supplement)   $(TD Kharoshthi)    $(TD Supplemental Arrows-A))
        $(TR $(TD CJK Strokes)   $(TD Khmer) $(TD Supplemental Arrows-B))
        $(TR $(TD CJK Symbols and Punctuation)   $(TD Khmer Symbols) $(TD Supplemental Mathematical Operators))
        $(TR $(TD CJK Unified Ideographs)    $(TD Lao)   $(TD Supplemental Punctuation))
        $(TR $(TD CJK Unified Ideographs Extension A)    $(TD Latin-1 Supplement)    $(TD Supplementary Private Use Area-A))
        $(TR $(TD CJK Unified Ideographs Extension B)    $(TD Latin Extended-A)  $(TD Supplementary Private Use Area-B))
        $(TR $(TD CJK Unified Ideographs Extension C)    $(TD Latin Extended Additional) $(TD Syloti Nagri))
        $(TR $(TD CJK Unified Ideographs Extension D)    $(TD Latin Extended-B)  $(TD Syriac))
        $(TR $(TD Combining Diacritical Marks)   $(TD Latin Extended-C)  $(TD Tagalog))
        $(TR $(TD Combining Diacritical Marks for Symbols)   $(TD Latin Extended-D)  $(TD Tagbanwa))
        $(TR $(TD Combining Diacritical Marks Supplement)    $(TD Lepcha)    $(TD Tags))
        $(TR $(TD Combining Half Marks)  $(TD Letterlike Symbols)    $(TD Tai Le))
        $(TR $(TD Common Indic Number Forms) $(TD Limbu) $(TD Tai Tham))
        $(TR $(TD Control Pictures)  $(TD Linear B Ideograms)    $(TD Tai Viet))
        $(TR $(TD Coptic)    $(TD Linear B Syllabary)    $(TD Tai Xuan Jing Symbols))
        $(TR $(TD Counting Rod Numerals) $(TD Lisu)  $(TD Takri))
        $(TR $(TD Cuneiform) $(TD Low Surrogates)    $(TD Tamil))
        $(TR $(TD Cuneiform Numbers and Punctuation) $(TD Lycian)    $(TD Telugu))
        $(TR $(TD Currency Symbols)  $(TD Lydian)    $(TD Thaana))
        $(TR $(TD Cypriot Syllabary) $(TD Mahjong Tiles) $(TD Thai))
        $(TR $(TD Cyrillic)  $(TD Malayalam) $(TD Tibetan))
        $(TR $(TD Cyrillic Extended-A)   $(TD Mandaic)   $(TD Tifinagh))
        $(TR $(TD Cyrillic Extended-B)   $(TD Mathematical Alphanumeric Symbols) $(TD Transport And Map Symbols))
        $(TR $(TD Cyrillic Supplement)   $(TD Mathematical Operators)    $(TD Ugaritic))
        $(TR $(TD Deseret)   $(TD Meetei Mayek)  $(TD Unified Canadian Aboriginal Syllabics))
        $(TR $(TD Devanagari)    $(TD Meetei Mayek Extensions)   $(TD Unified Canadian Aboriginal Syllabics Extended))
        $(TR $(TD Devanagari Extended)   $(TD Meroitic Cursive)  $(TD Vai))
        $(TR $(TD Dingbats)  $(TD Meroitic Hieroglyphs)  $(TD Variation Selectors))
        $(TR $(TD Domino Tiles)  $(TD Miao)  $(TD Variation Selectors Supplement))
        $(TR $(TD Egyptian Hieroglyphs)  $(TD Miscellaneous Mathematical Symbols-A)  $(TD Vedic Extensions))
        $(TR $(TD Emoticons) $(TD Miscellaneous Mathematical Symbols-B)  $(TD Vertical Forms))
        $(TR $(TD Enclosed Alphanumerics)    $(TD Miscellaneous Symbols) $(TD Yijing Hexagram Symbols))
        $(TR $(TD Enclosed Alphanumeric Supplement)  $(TD Miscellaneous Symbols and Arrows)  $(TD Yi Radicals))
        $(TR $(TD Enclosed CJK Letters and Months)   $(TD Miscellaneous Symbols And Pictographs) $(TD Yi Syllables))
        $(TR $(TD Enclosed Ideographic Supplement)   $(TD Miscellaneous Technical)   )
        $(TR $(TD Ethiopic)  $(TD Modifier Tone Letters) )
    )
    $(P Below is the table with script names accepted by $(LREF unicode.script)
        and by the shorthand version $(LREF unicode):)
        $(BOOKTABLE $(B Scripts),
            $(TR $(TD Arabic)  $(TD Hanunoo) $(TD Old_Italic))
            $(TR $(TD Armenian)    $(TD Hebrew)  $(TD Old_Persian))
            $(TR $(TD Avestan) $(TD Hiragana)    $(TD Old_South_Arabian))
            $(TR $(TD Balinese)    $(TD Imperial_Aramaic)    $(TD Old_Turkic))
            $(TR $(TD Bamum)   $(TD Inherited)   $(TD Oriya))
            $(TR $(TD Batak)   $(TD Inscriptional_Pahlavi)   $(TD Osmanya))
            $(TR $(TD Bengali) $(TD Inscriptional_Parthian)  $(TD Phags_Pa))
            $(TR $(TD Bopomofo)    $(TD Javanese)    $(TD Phoenician))
            $(TR $(TD Brahmi)  $(TD Kaithi)  $(TD Rejang))
            $(TR $(TD Braille) $(TD Kannada) $(TD Runic))
            $(TR $(TD Buginese)    $(TD Katakana)    $(TD Samaritan))
            $(TR $(TD Buhid)   $(TD Kayah_Li)    $(TD Saurashtra))
            $(TR $(TD Canadian_Aboriginal) $(TD Kharoshthi)  $(TD Sharada))
            $(TR $(TD Carian)  $(TD Khmer)   $(TD Shavian))
            $(TR $(TD Chakma)  $(TD Lao) $(TD Sinhala))
            $(TR $(TD Cham)    $(TD Latin)   $(TD Sora_Sompeng))
            $(TR $(TD Cherokee)    $(TD Lepcha)  $(TD Sundanese))
            $(TR $(TD Common)  $(TD Limbu)   $(TD Syloti_Nagri))
            $(TR $(TD Coptic)  $(TD Linear_B)    $(TD Syriac))
            $(TR $(TD Cuneiform)   $(TD Lisu)    $(TD Tagalog))
            $(TR $(TD Cypriot) $(TD Lycian)  $(TD Tagbanwa))
            $(TR $(TD Cyrillic)    $(TD Lydian)  $(TD Tai_Le))
            $(TR $(TD Deseret) $(TD Malayalam)   $(TD Tai_Tham))
            $(TR $(TD Devanagari)  $(TD Mandaic) $(TD Tai_Viet))
            $(TR $(TD Egyptian_Hieroglyphs)    $(TD Meetei_Mayek)    $(TD Takri))
            $(TR $(TD Ethiopic)    $(TD Meroitic_Cursive)    $(TD Tamil))
            $(TR $(TD Georgian)    $(TD Meroitic_Hieroglyphs)    $(TD Telugu))
            $(TR $(TD Glagolitic)  $(TD Miao)    $(TD Thaana))
            $(TR $(TD Gothic)  $(TD Mongolian)   $(TD Thai))
            $(TR $(TD Greek)   $(TD Myanmar) $(TD Tibetan))
            $(TR $(TD Gujarati)    $(TD New_Tai_Lue) $(TD Tifinagh))
            $(TR $(TD Gurmukhi)    $(TD Nko) $(TD Ugaritic))
            $(TR $(TD Han) $(TD Ogham)   $(TD Vai))
            $(TR $(TD Hangul)  $(TD Ol_Chiki)    $(TD Yi))
    )
    $(P Below is the table of names accepted by $(LREF unicode.hangulSyllableType).)
        $(BOOKTABLE $(B Hangul syllable type),
            $(TR $(TH Abb.) $(TH Long form))
            $(TR $(TD L)   $(TD Leading_Jamo))
            $(TR $(TD LV)  $(TD LV_Syllable))
            $(TR $(TD LVT) $(TD LVT_Syllable) )
            $(TR $(TD T)   $(TD Trailing_Jamo))
            $(TR $(TD V)   $(TD Vowel_Jamo))
    )
    References:
        $(HTTP www.digitalmars.com/d/ascii-table.html, ASCII Table),
        $(HTTP en.wikipedia.org/wiki/Unicode, Wikipedia),
        $(HTTP www.unicode.org, The Unicode Consortium),
        $(HTTP www.unicode.org/reports/tr15/, Unicode normalization forms),
        $(HTTP www.unicode.org/reports/tr29/, Unicode text segmentation)
        $(HTTP www.unicode.org/uni2book/ch05.pdf,
            Unicode Implementation Guidelines)
        $(HTTP www.unicode.org/uni2book/ch03.pdf,
            Unicode Conformance)
    Trademarks:
        Unicode(tm) is a trademark of Unicode, Inc.

    Copyright: Copyright 2013 -
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Dmitry Olshansky
    Source:    $(PHOBOSSRC std/uni/package.d)
    Standards: $(HTTP www.unicode.org/versions/Unicode6.2.0/, Unicode v6.2)

Macros:

SECTION = <h3><a id="$1">$0</a></h3>
DEF = <div><a id="$1"><i>$0</i></a></div>
S_LINK = <a href="#$1">$+</a>
CODEPOINT = $(S_LINK Code point, code point)
CODEPOINTS = $(S_LINK Code point, code points)
CHARACTER = $(S_LINK Character, character)
CHARACTERS = $(S_LINK Character, characters)
CLUSTER = $(S_LINK Grapheme cluster, grapheme cluster)
+/
module std.uni;

import std.meta : AliasSeq;
import std.range.primitives : back, ElementEncodingType, ElementType, empty,
    front, hasLength, hasSlicing, isForwardRange, isInputRange,
    isRandomAccessRange, popFront, put, save;
import std.traits : isAutodecodableString, isConvertibleToString, isIntegral,
    isSomeChar, isSomeString, Unqual, isDynamicArray;
// debug = std_uni;

import std.internal.unicode_tables; // generated file

debug(std_uni) import std.stdio; // writefln, writeln

private:


void copyBackwards(T,U)(T[] src, U[] dest)
{
    assert(src.length == dest.length);
    for (size_t i=src.length; i-- > 0; )
        dest[i] = src[i];
}

void copyForward(T,U)(T[] src, U[] dest)
{
    assert(src.length == dest.length);
    for (size_t i=0; i<src.length; i++)
        dest[i] = src[i];
}

// TODO: update to reflect all major CPUs supporting unaligned reads
version (X86)
    enum hasUnalignedReads = true;
else version (X86_64)
    enum hasUnalignedReads = true;
else version (SystemZ)
    enum hasUnalignedReads = true;
else
    enum hasUnalignedReads = false; // better be safe then sorry

public enum dchar lineSep = '\u2028'; /// Constant $(CODEPOINT) (0x2028) - line separator.
public enum dchar paraSep = '\u2029'; /// Constant $(CODEPOINT) (0x2029) - paragraph separator.
public enum dchar nelSep  = '\u0085'; /// Constant $(CODEPOINT) (0x0085) - next line.

// test the intro example
@safe unittest
{
    import std.algorithm.searching : find;
    // initialize code point sets using script/block or property name
    // set contains code points from both scripts.
    auto set = unicode("Cyrillic") | unicode("Armenian");
    // or simpler and statically-checked look
    auto ascii = unicode.ASCII;
    auto currency = unicode.Currency_Symbol;

    // easy set ops
    auto a = set & ascii;
    assert(a.empty); // as it has no intersection with ascii
    a = set | ascii;
    auto b = currency - a; // subtract all ASCII, Cyrillic and Armenian

    // some properties of code point sets
    assert(b.length > 45); // 46 items in Unicode 6.1, even more in 6.2
    // testing presence of a code point in a set
    // is just fine, it is O(logN)
    assert(!b['$']);
    assert(!b['\u058F']); // Armenian dram sign
    assert(b['¥']);

    // building fast lookup tables, these guarantee O(1) complexity
    // 1-level Trie lookup table essentially a huge bit-set ~262Kb
    auto oneTrie = toTrie!1(b);
    // 2-level far more compact but typically slightly slower
    auto twoTrie = toTrie!2(b);
    // 3-level even smaller, and a bit slower yet
    auto threeTrie = toTrie!3(b);
    assert(oneTrie['£']);
    assert(twoTrie['£']);
    assert(threeTrie['£']);

    // build the trie with the most sensible trie level
    // and bind it as a functor
    auto cyrillicOrArmenian = toDelegate(set);
    auto balance = find!(cyrillicOrArmenian)("Hello ընկեր!");
    assert(balance == "ընկեր!");
    // compatible with bool delegate(dchar)
    bool delegate(dchar) bindIt = cyrillicOrArmenian;

    // Normalization
    string s = "Plain ascii (and not only), is always normalized!";
    assert(s is normalize(s));// is the same string

    string nonS = "A\u0308ffin"; // A ligature
    auto nS = normalize(nonS); // to NFC, the W3C endorsed standard
    assert(nS == "Äffin");
    assert(nS != nonS);
    string composed = "Äffin";

    assert(normalize!NFD(composed) == "A\u0308ffin");
    // to NFKD, compatibility decomposition useful for fuzzy matching/searching
    assert(normalize!NFKD("2¹⁰") == "210");
}

enum lastDchar = 0x10FFFF;

auto force(T, F)(F from)
if (isIntegral!T && !is(T == F))
{
    assert(from <= T.max && from >= T.min);
    return cast(T) from;
}

auto force(T, F)(F from)
if (isBitPacked!T && !is(T == F))
{
    assert(from <= 2^^bitSizeOf!T-1);
    return T(cast(TypeOfBitPacked!T) from);
}

auto force(T, F)(F from)
if (is(T == F))
{
    return from;
}

// repeat X times the bit-pattern in val assuming it's length is 'bits'
size_t replicateBits(size_t times, size_t bits)(size_t val) @safe pure nothrow @nogc
{
    static if (times == 1)
        return val;
    else static if (bits == 1)
    {
        static if (times == size_t.sizeof*8)
            return val ? size_t.max : 0;
        else
            return val ? (1 << times)-1 : 0;
    }
    else static if (times % 2)
        return (replicateBits!(times-1, bits)(val)<<bits) | val;
    else
        return replicateBits!(times/2, bits*2)((val << bits) | val);
}

@safe pure nothrow @nogc unittest // for replicate
{
    import std.algorithm.iteration : sum, map;
    import std.range : iota;
    size_t m = 0b111;
    size_t m2 = 0b01;
    static foreach (i; AliasSeq!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    {
        assert(replicateBits!(i, 3)(m)+1 == (1<<(3*i)));
        assert(replicateBits!(i, 2)(m2) == iota(0, i).map!"2^^(2*a)"().sum());
    }
}

// multiple arrays squashed into one memory block
struct MultiArray(Types...)
{
    import std.range.primitives : isOutputRange;
    this(size_t[] sizes...) @safe pure nothrow
    {
        assert(dim == sizes.length);
        size_t full_size;
        foreach (i, v; Types)
        {
            full_size += spaceFor!(bitSizeOf!v)(sizes[i]);
            sz[i] = sizes[i];
            static if (i >= 1)
                offsets[i] = offsets[i-1] +
                    spaceFor!(bitSizeOf!(Types[i-1]))(sizes[i-1]);
        }

        storage = new size_t[full_size];
    }

    this(const(size_t)[] raw_offsets,
        const(size_t)[] raw_sizes,
        return scope const(size_t)[] data) return scope const @safe pure nothrow @nogc
    {
        offsets[] = raw_offsets[];
        sz[] = raw_sizes[];
        storage = data;
    }

    @property auto slice(size_t n)()inout pure nothrow @nogc
    {
        auto ptr = raw_ptr!n;
        return packedArrayView!(Types[n])(ptr, sz[n]);
    }

    @property auto ptr(size_t n)()inout pure nothrow @nogc
    {
        auto ptr = raw_ptr!n;
        return inout(PackedPtr!(Types[n]))(ptr);
    }

    template length(size_t n)
    {
        @property size_t length()const @safe pure nothrow @nogc{ return sz[n]; }

        @property void length(size_t new_size)
        {
            if (new_size > sz[n])
            {// extend
                size_t delta = (new_size - sz[n]);
                sz[n] += delta;
                delta = spaceFor!(bitSizeOf!(Types[n]))(delta);
                storage.length +=  delta;// extend space at end
                // raw_slice!x must follow resize as it could be moved!
                // next stmts move all data past this array, last-one-goes-first
                static if (n != dim-1)
                {
                    auto start = raw_ptr!(n+1);
                    // len includes delta
                    size_t len = (storage.ptr+storage.length-start);

                    copyBackwards(start[0 .. len-delta], start[delta .. len]);

                    start[0 .. delta] = 0;
                    // offsets are used for raw_slice, ptr etc.
                    foreach (i; n+1 .. dim)
                        offsets[i] += delta;
                }
            }
            else if (new_size < sz[n])
            {// shrink
                size_t delta = (sz[n] - new_size);
                sz[n] -= delta;
                delta = spaceFor!(bitSizeOf!(Types[n]))(delta);
                // move all data past this array, forward direction
                static if (n != dim-1)
                {
                    auto start = raw_ptr!(n+1);
                    size_t len = (storage.ptr+storage.length-start);
                    copyForward(start[0 .. len-delta], start[delta .. len]);

                    // adjust offsets last, they affect raw_slice
                    foreach (i; n+1 .. dim)
                        offsets[i] -= delta;
                }
                storage.length -= delta;
            }
            // else - NOP
        }
    }

    @property size_t bytes(size_t n=size_t.max)() const @safe
    {
        static if (n == size_t.max)
            return storage.length*size_t.sizeof;
        else static if (n != Types.length-1)
            return (raw_ptr!(n+1)-raw_ptr!n)*size_t.sizeof;
        else
            return (storage.ptr+storage.length - raw_ptr!n)*size_t.sizeof;
    }

    void store(OutRange)(scope OutRange sink) const
    if (isOutputRange!(OutRange, char))
    {
        import std.format.write : formattedWrite;
        formattedWrite(sink, "[%( 0x%x, %)]", offsets[]);
        formattedWrite(sink, ", [%( 0x%x, %)]", sz[]);
        formattedWrite(sink, ", [%( 0x%x, %)]", storage);
    }

private:
    import std.meta : staticMap;
    @property auto raw_ptr(size_t n)()inout pure nothrow @nogc
    {
        static if (n == 0)
            return storage.ptr;
        else
        {
            return storage.ptr+offsets[n];
        }
    }
    enum dim = Types.length;
    size_t[dim] offsets;// offset for level x
    size_t[dim] sz;// size of level x
    alias bitWidth = staticMap!(bitSizeOf, Types);
    size_t[] storage;
}

@system unittest
{
    import std.conv : text;
    enum dg = (){
        // sizes are:
        // lvl0: 3, lvl1 : 2, lvl2: 1
        auto m = MultiArray!(int, ubyte, int)(3,2,1);

        static void check(size_t k, T)(ref T m, int n)
        {
            foreach (i; 0 .. n)
                assert(m.slice!(k)[i] == i+1, text("level:",i," : ",m.slice!(k)[0 .. n]));
        }

        static void checkB(size_t k, T)(ref T m, int n)
        {
            foreach (i; 0 .. n)
                assert(m.slice!(k)[i] == n-i, text("level:",i," : ",m.slice!(k)[0 .. n]));
        }

        static void fill(size_t k, T)(ref T m, int n)
        {
            foreach (i; 0 .. n)
                m.slice!(k)[i] = force!ubyte(i+1);
        }

        static void fillB(size_t k, T)(ref T m, int n)
        {
            foreach (i; 0 .. n)
                m.slice!(k)[i] = force!ubyte(n-i);
        }

        m.length!1 = 100;
        fill!1(m, 100);
        check!1(m, 100);

        m.length!0 = 220;
        fill!0(m, 220);
        check!1(m, 100);
        check!0(m, 220);

        m.length!2 = 17;
        fillB!2(m, 17);
        checkB!2(m, 17);
        check!0(m, 220);
        check!1(m, 100);

        m.length!2 = 33;
        checkB!2(m, 17);
        fillB!2(m, 33);
        checkB!2(m, 33);
        check!0(m, 220);
        check!1(m, 100);

        m.length!1 = 195;
        fillB!1(m, 195);
        checkB!1(m, 195);
        checkB!2(m, 33);
        check!0(m, 220);

        auto marr = MultiArray!(BitPacked!(uint, 4), BitPacked!(uint, 6))(20, 10);
        marr.length!0 = 15;
        marr.length!1 = 30;
        fill!1(marr, 30);
        fill!0(marr, 15);
        check!1(marr, 30);
        check!0(marr, 15);
        return 0;
    };
    enum ct = dg();
    auto rt = dg();
}

@system unittest
{// more bitpacking tests
    import std.conv : text;

    alias Bitty =
      MultiArray!(BitPacked!(size_t, 3)
                , BitPacked!(size_t, 4)
                , BitPacked!(size_t, 3)
                , BitPacked!(size_t, 6)
                , bool);
    alias fn1 = sliceBits!(13, 16);
    alias fn2 = sliceBits!( 9, 13);
    alias fn3 = sliceBits!( 6,  9);
    alias fn4 = sliceBits!( 0,  6);
    static void check(size_t lvl, MA)(ref MA arr){
        for (size_t i = 0; i< arr.length!lvl; i++)
            assert(arr.slice!(lvl)[i] == i, text("Mismatch on lvl ", lvl, " idx ", i, " value: ", arr.slice!(lvl)[i]));
    }

    static void fillIdx(size_t lvl, MA)(ref MA arr){
        for (size_t i = 0; i< arr.length!lvl; i++)
            arr.slice!(lvl)[i] = i;
    }
    Bitty m1;

    m1.length!4 = 10;
    m1.length!3 = 2^^6;
    m1.length!2 = 2^^3;
    m1.length!1 = 2^^4;
    m1.length!0 = 2^^3;

    m1.length!4 = 2^^16;

    for (size_t i = 0; i< m1.length!4; i++)
        m1.slice!(4)[i] = i % 2;

    fillIdx!1(m1);
    check!1(m1);
    fillIdx!2(m1);
    check!2(m1);
    fillIdx!3(m1);
    check!3(m1);
    fillIdx!0(m1);
    check!0(m1);
    check!3(m1);
    check!2(m1);
    check!1(m1);
    for (size_t i=0; i < 2^^16; i++)
    {
        m1.slice!(4)[i] = i % 2;
        m1.slice!(0)[fn1(i)] = fn1(i);
        m1.slice!(1)[fn2(i)] = fn2(i);
        m1.slice!(2)[fn3(i)] = fn3(i);
        m1.slice!(3)[fn4(i)] = fn4(i);
    }
    for (size_t i=0; i < 2^^16; i++)
    {
        assert(m1.slice!(4)[i] == i % 2);
        assert(m1.slice!(0)[fn1(i)] == fn1(i));
        assert(m1.slice!(1)[fn2(i)] == fn2(i));
        assert(m1.slice!(2)[fn3(i)] == fn3(i));
        assert(m1.slice!(3)[fn4(i)] == fn4(i));
    }
}

size_t spaceFor(size_t _bits)(size_t new_len) @safe pure nothrow @nogc
{
    import std.math.algebraic : nextPow2;
    enum bits = _bits == 1 ? 1 : nextPow2(_bits - 1);// see PackedArrayView
    static if (bits > 8*size_t.sizeof)
    {
        static assert(bits % (size_t.sizeof*8) == 0);
        return new_len * bits/(8*size_t.sizeof);
    }
    else
    {
        enum factor = size_t.sizeof*8/bits;
        return (new_len+factor-1)/factor; // rounded up
    }
}

template isBitPackableType(T)
{
    enum isBitPackableType = isBitPacked!T
        || isIntegral!T || is(T == bool) || isSomeChar!T;
}

//============================================================================
template PackedArrayView(T)
if ((is(T dummy == BitPacked!(U, sz), U, size_t sz)
    && isBitPackableType!U) || isBitPackableType!T)
{
    import std.math.algebraic : nextPow2;
    private enum bits = bitSizeOf!T;
    alias PackedArrayView = PackedArrayViewImpl!(T, bits > 1 ? nextPow2(bits - 1) : 1);
}

//unsafe and fast access to a chunk of RAM as if it contains packed values
template PackedPtr(T)
if ((is(T dummy == BitPacked!(U, sz), U, size_t sz)
    && isBitPackableType!U) || isBitPackableType!T)
{
    import std.math.algebraic : nextPow2;
    private enum bits = bitSizeOf!T;
    alias PackedPtr = PackedPtrImpl!(T, bits > 1 ? nextPow2(bits - 1) : 1);
}

struct PackedPtrImpl(T, size_t bits)
{
pure nothrow:
    static assert(isPow2OrZero(bits));

    this(inout(size_t)* ptr)inout @safe @nogc
    {
        origin = ptr;
    }

    private T simpleIndex(size_t n) inout
    {
        immutable q = n / factor;
        immutable r = n % factor;
        return cast(T)((origin[q] >> bits*r) & mask);
    }

    private void simpleWrite(TypeOfBitPacked!T val, size_t n)
    in
    {
        static if (isIntegral!T)
            assert(val <= mask);
    }
    do
    {
        immutable q = n / factor;
        immutable r = n % factor;
        immutable tgt_shift = bits*r;
        immutable word = origin[q];
        origin[q] = (word & ~(mask << tgt_shift))
            | (cast(size_t) val << tgt_shift);
    }

    static if (factor == bytesPerWord// can safely pack by byte
         || factor == 1 // a whole word at a time
         || ((factor == bytesPerWord/2 || factor == bytesPerWord/4)
                && hasUnalignedReads)) // this needs unaligned reads
    {
        static if (factor == bytesPerWord)
            alias U = ubyte;
        else static if (factor == bytesPerWord/2)
            alias U = ushort;
        else static if (factor == bytesPerWord/4)
            alias U = uint;
        else static if (size_t.sizeof == 8 && factor == bytesPerWord/8)
            alias U = ulong;

        T opIndex(size_t idx) inout
        {
            T ret;
            version (LittleEndian)
                ret = __ctfe ? simpleIndex(idx) :
                    cast(inout(T))(cast(U*) origin)[idx];
            else
                ret = simpleIndex(idx);
            return ret;
        }

        static if (isBitPacked!T) // lack of user-defined implicit conversion
        {
            void opIndexAssign(T val, size_t idx)
            {
                return opIndexAssign(cast(TypeOfBitPacked!T) val, idx);
            }
        }

        void opIndexAssign(TypeOfBitPacked!T val, size_t idx)
        {
            version (LittleEndian)
            {
                if (__ctfe)
                    simpleWrite(val, idx);
                else
                    (cast(U*) origin)[idx] = cast(U) val;
            }
            else
                simpleWrite(val, idx);
        }
    }
    else
    {
        T opIndex(size_t n) inout
        {
            return simpleIndex(n);
        }

        static if (isBitPacked!T) // lack of user-defined implicit conversion
        {
            void opIndexAssign(T val, size_t idx)
            {
                return opIndexAssign(cast(TypeOfBitPacked!T) val, idx);
            }
        }

        void opIndexAssign(TypeOfBitPacked!T val, size_t n)
        {
            return simpleWrite(val, n);
        }
    }

private:
    // factor - number of elements in one machine word
    enum factor = size_t.sizeof*8/bits, mask = 2^^bits-1;
    enum bytesPerWord =  size_t.sizeof;
    size_t* origin;
}

// data is packed only by power of two sized packs per word,
// thus avoiding mul/div overhead at the cost of ultimate packing
// this construct doesn't own memory, only provides access, see MultiArray for usage
struct PackedArrayViewImpl(T, size_t bits)
{
pure nothrow:

    this(inout(size_t)* origin, size_t offset, size_t items) inout @safe
    {
        ptr = inout(PackedPtr!(T))(origin);
        ofs = offset;
        limit = items;
    }

    bool zeros(size_t s, size_t e)
    in
    {
        assert(s <= e);
    }
    do
    {
        s += ofs;
        e += ofs;
        immutable pad_s = roundUp(s);
        if ( s >= e)
        {
            foreach (i; s .. e)
                if (ptr[i])
                    return false;
            return true;
        }
        immutable pad_e = roundDown(e);
        size_t i;
        for (i=s; i<pad_s; i++)
            if (ptr[i])
                return false;
        // all in between is x*factor elements
        for (size_t j=i/factor; i<pad_e; i+=factor, j++)
            if (ptr.origin[j])
                return false;
        for (; i<e; i++)
            if (ptr[i])
                return false;
        return true;
    }

    T opIndex(size_t idx) inout
    in
    {
        assert(idx < limit);
    }
    do
    {
        return ptr[ofs + idx];
    }

    static if (isBitPacked!T) // lack of user-defined implicit conversion
    {
        void opIndexAssign(T val, size_t idx)
        {
            return opIndexAssign(cast(TypeOfBitPacked!T) val, idx);
        }
    }

    void opIndexAssign(TypeOfBitPacked!T val, size_t idx)
    in
    {
        assert(idx < limit);
    }
    do
    {
        ptr[ofs + idx] = val;
    }

    static if (isBitPacked!T) // lack of user-defined implicit conversions
    {
        void opSliceAssign(T val, size_t start, size_t end)
        {
            opSliceAssign(cast(TypeOfBitPacked!T) val, start, end);
        }
    }

    void opSliceAssign(TypeOfBitPacked!T val, size_t start, size_t end)
    in
    {
        assert(start <= end);
        assert(end <= limit);
    }
    do
    {
        // account for ofsetted view
        start += ofs;
        end += ofs;
        // rounded to factor granularity
        immutable pad_start = roundUp(start);// rounded up
        if (pad_start >= end) //rounded up >= then end of slice
        {
            //nothing to gain, use per element assignment
            foreach (i; start .. end)
                ptr[i] = val;
            return;
        }
        immutable pad_end = roundDown(end); // rounded down
        size_t i;
        for (i=start; i<pad_start; i++)
            ptr[i] = val;
        // all in between is x*factor elements
        if (pad_start != pad_end)
        {
            immutable repval = replicateBits!(factor, bits)(val);
            for (size_t j=i/factor; i<pad_end; i+=factor, j++)
                ptr.origin[j] = repval;// so speed it up by factor
        }
        for (; i<end; i++)
            ptr[i] = val;
    }

    auto opSlice(size_t from, size_t to)inout
    in
    {
        assert(from <= to);
        assert(ofs + to <= limit);
    }
    do
    {
        return typeof(this)(ptr.origin, ofs + from, to - from);
    }

    auto opSlice(){ return opSlice(0, length); }

    bool opEquals(T)(auto ref T arr) const
    {
        if (limit != arr.limit)
           return false;
        size_t s1 = ofs, s2 = arr.ofs;
        size_t e1 = s1 + limit, e2 = s2 + limit;
        if (s1 % factor == 0 && s2 % factor == 0 && length % factor == 0)
        {
            return ptr.origin[s1/factor .. e1/factor]
                == arr.ptr.origin[s2/factor .. e2/factor];
        }
        for (size_t i=0;i<limit; i++)
            if (this[i] != arr[i])
                return false;
        return true;
    }

    @property size_t length()const{ return limit; }

private:
    auto roundUp()(size_t val){ return (val+factor-1)/factor*factor; }
    auto roundDown()(size_t val){ return val/factor*factor; }
    // factor - number of elements in one machine word
    enum factor = size_t.sizeof*8/bits;
    PackedPtr!(T) ptr;
    size_t ofs, limit;
}


private struct SliceOverIndexed(T)
{
    enum assignableIndex = is(typeof((){ T.init[0] = Item.init; }));
    enum assignableSlice = is(typeof((){ T.init[0 .. 0] = Item.init; }));
    auto opIndex(size_t idx)const
    in
    {
        assert(idx < to - from);
    }
    do
    {
        return (*arr)[from+idx];
    }

    static if (assignableIndex)
    void opIndexAssign(Item val, size_t idx)
    in
    {
        assert(idx < to - from);
    }
    do
    {
       (*arr)[from+idx] = val;
    }

    auto opSlice(size_t a, size_t b)
    {
        return typeof(this)(from+a, from+b, arr);
    }

    // static if (assignableSlice)
    void opSliceAssign(T)(T val, size_t start, size_t end)
    {
        (*arr)[start+from .. end+from] = val;
    }

    auto opSlice()
    {
        return typeof(this)(from, to, arr);
    }

    @property size_t length()const { return to-from;}

    alias opDollar = length;

    @property bool empty()const { return from == to; }

    @property auto front()const { return (*arr)[from]; }

    static if (assignableIndex)
    @property void front(Item val) { (*arr)[from] = val; }

    @property auto back()const { return (*arr)[to-1]; }

    static if (assignableIndex)
    @property void back(Item val) { (*arr)[to-1] = val; }

    @property auto save() inout { return this; }

    void popFront() {   from++; }

    void popBack() {    to--; }

    bool opEquals(T)(auto ref T arr) const
    {
        if (arr.length != length)
            return false;
        for (size_t i=0; i <length; i++)
            if (this[i] != arr[i])
                return false;
        return true;
    }
private:
    alias Item = typeof(T.init[0]);
    size_t from, to;
    T* arr;
}

@safe pure nothrow @nogc unittest
{
    static assert(isRandomAccessRange!(SliceOverIndexed!(int[])));
}

SliceOverIndexed!(const(T)) sliceOverIndexed(T)(size_t a, size_t b, const(T)* x)
if (is(Unqual!T == T))
{
    return SliceOverIndexed!(const(T))(a, b, x);
}

// BUG? inout is out of reach
//...SliceOverIndexed.arr only parameters or stack based variables can be inout
SliceOverIndexed!T sliceOverIndexed(T)(size_t a, size_t b, T* x)
if (is(Unqual!T == T))
{
    return SliceOverIndexed!T(a, b, x);
}

@system unittest
{
    int[] idxArray = [2, 3, 5, 8, 13];
    auto sliced = sliceOverIndexed(0, idxArray.length, &idxArray);

    assert(!sliced.empty);
    assert(sliced.front == 2);
    sliced.front = 1;
    assert(sliced.front == 1);
    assert(sliced.back == 13);
    sliced.popFront();
    assert(sliced.front == 3);
    assert(sliced.back == 13);
    sliced.back = 11;
    assert(sliced.back == 11);
    sliced.popBack();

    assert(sliced.front == 3);
    assert(sliced[$-1] == 8);
    sliced = sliced[];
    assert(sliced[0] == 3);
    assert(sliced.back == 8);
    sliced = sliced[1..$];
    assert(sliced.front == 5);
    sliced = sliced[0..$-1];
    assert(sliced[$-1] == 5);

    int[] other = [2, 5];
    assert(sliced[] == sliceOverIndexed(1, 2, &other));
    sliceOverIndexed(0, 2, &idxArray)[0 .. 2] = -1;
    assert(idxArray[0 .. 2] == [-1, -1]);
    uint[] nullArr = null;
    auto nullSlice = sliceOverIndexed(0, 0, &idxArray);
    assert(nullSlice.empty);
}

private inout(PackedArrayView!T) packedArrayView(T)(inout(size_t)* ptr, size_t items)
{
    return inout(PackedArrayView!T)(ptr, 0, items);
}


//============================================================================
// Partially unrolled binary search using Shar's method
//============================================================================

string genUnrolledSwitchSearch(size_t size) @safe pure nothrow
{
    import core.bitop : bsr;
    import std.array : replace;
    import std.conv : to;
    assert(isPow2OrZero(size));
    string code = `
    import core.bitop : bsr;
    auto power = bsr(m)+1;
    switch (power){`;
    size_t i = bsr(size);
    foreach_reverse (val; 0 .. bsr(size))
    {
        auto v = 2^^val;
        code ~= `
        case pow:
            if (pred(range[idx+m], needle))
                idx +=  m;
            goto case;
        `.replace("m", to!string(v))
        .replace("pow", to!string(i));
        i--;
    }
    code ~= `
        case 0:
            if (pred(range[idx], needle))
                idx += 1;
            goto default;
        `;
    code ~= `
        default:
    }`;
    return code;
}

bool isPow2OrZero(size_t sz) @safe pure nothrow @nogc
{
    // See also: std.math.isPowerOf2()
    return (sz & (sz-1)) == 0;
}

size_t uniformLowerBound(alias pred, Range, T)(Range range, T needle)
if (is(T : ElementType!Range))
{
    assert(isPow2OrZero(range.length));
    size_t idx = 0, m = range.length/2;
    while (m != 0)
    {
        if (pred(range[idx+m], needle))
            idx += m;
        m /= 2;
    }
    if (pred(range[idx], needle))
        idx += 1;
    return idx;
}

size_t switchUniformLowerBound(alias pred, Range, T)(Range range, T needle)
if (is(T : ElementType!Range))
{
    assert(isPow2OrZero(range.length));
    size_t idx = 0, m = range.length/2;
    enum max = 1 << 10;
    while (m >= max)
    {
        if (pred(range[idx+m], needle))
            idx += m;
        m /= 2;
    }
    mixin(genUnrolledSwitchSearch(max));
    return idx;
}

template sharMethod(alias uniLowerBound)
{
    size_t sharMethod(alias _pred="a<b", Range, T)(Range range, T needle)
    if (is(T : ElementType!Range))
    {
        import std.functional : binaryFun;
        import std.math.algebraic : nextPow2, truncPow2;
        alias pred = binaryFun!_pred;
        if (range.length == 0)
            return 0;
        if (isPow2OrZero(range.length))
            return uniLowerBound!pred(range, needle);
        size_t n = truncPow2(range.length);
        if (pred(range[n-1], needle))
        {// search in another 2^^k area that fully covers the tail of range
            size_t k = nextPow2(range.length - n + 1);
            return range.length - k + uniLowerBound!pred(range[$-k..$], needle);
        }
        else
            return uniLowerBound!pred(range[0 .. n], needle);
    }
}

alias sharLowerBound = sharMethod!uniformLowerBound;
alias sharSwitchLowerBound = sharMethod!switchUniformLowerBound;

@safe unittest
{
    import std.array : array;
    import std.range : assumeSorted, iota;

    auto stdLowerBound(T)(T[] range, T needle)
    {
        return assumeSorted(range).lowerBound(needle).length;
    }
    immutable MAX = 5*1173;
    auto arr = array(iota(5, MAX, 5));
    assert(arr.length == MAX/5-1);
    foreach (i; 0 .. MAX+5)
    {
        auto st = stdLowerBound(arr, i);
        assert(st == sharLowerBound(arr, i));
        assert(st == sharSwitchLowerBound(arr, i));
    }
    arr = [];
    auto st = stdLowerBound(arr, 33);
    assert(st == sharLowerBound(arr, 33));
    assert(st == sharSwitchLowerBound(arr, 33));
}
//============================================================================

@safe
{
// hope to see simillar stuff in public interface... once Allocators are out
//@@@BUG moveFront and friends? dunno, for now it's POD-only

@trusted size_t genericReplace(Policy=void, T, Range)
    (ref T dest, size_t from, size_t to, Range stuff)
{
    import std.algorithm.mutation : copy;
    size_t delta = to - from;
    size_t stuff_end = from+stuff.length;
    if (stuff.length > delta)
    {// replace increases length
        delta = stuff.length - delta;// now, new is > old  by delta
        static if (is(Policy == void))
            dest.length = dest.length+delta;//@@@BUG lame @property
        else
            dest = Policy.realloc(dest, dest.length+delta);
        copyBackwards(dest[to .. dest.length-delta],
            dest[to+delta .. dest.length]);
        copyForward(stuff, dest[from .. stuff_end]);
    }
    else if (stuff.length == delta)
    {
        copy(stuff, dest[from .. to]);
    }
    else
    {// replace decreases length by delta
        delta = delta - stuff.length;
        copy(stuff, dest[from .. stuff_end]);
        copyForward(dest[to .. dest.length],
            dest[stuff_end .. dest.length-delta]);
        static if (is(Policy == void))
            dest.length = dest.length - delta;//@@@BUG lame @property
        else
            dest = Policy.realloc(dest, dest.length-delta);
    }
    return stuff_end;
}


// Simple storage manipulation policy
@safe private struct GcPolicy
{
    import std.traits : isDynamicArray;

    static T[] dup(T)(const T[] arr)
    {
        return arr.dup;
    }

    static T[] alloc(T)(size_t size)
    {
        return new T[size];
    }

    static T[] realloc(T)(T[] arr, size_t sz)
    {
        arr.length = sz;
        return arr;
    }

    static void replaceImpl(T, Range)(ref T[] dest, size_t from, size_t to, Range stuff)
    {
        replaceInPlace(dest, from, to, stuff);
    }

    static void append(T, V)(ref T[] arr, V value)
    if (!isInputRange!V)
    {
        arr ~= force!T(value);
    }

    static void append(T, V)(ref T[] arr, V value)
    if (isInputRange!V)
    {
        insertInPlace(arr, arr.length, value);
    }

    static void destroy(T)(ref T arr) pure // pure required for -dip25, inferred for -dip1000
    if (isDynamicArray!T && is(Unqual!T == T))
    {
        debug
        {
            arr[] = cast(typeof(T.init[0]))(0xdead_beef);
        }
        arr = null;
    }

    static void destroy(T)(ref T arr) pure // pure required for -dip25, inferred for -dip1000
    if (isDynamicArray!T && !is(Unqual!T == T))
    {
        arr = null;
    }
}

// ditto
@safe struct ReallocPolicy
{
    import std.range.primitives : hasLength;

    static T[] dup(T)(const T[] arr)
    {
        auto result = alloc!T(arr.length);
        result[] = arr[];
        return result;
    }

    static T[] alloc(T)(size_t size) @trusted
    {
        import std.internal.memory : enforceMalloc;

        import core.checkedint : mulu;
        bool overflow;
        size_t nbytes = mulu(size, T.sizeof, overflow);
        if (overflow) assert(0);

        auto ptr = cast(T*) enforceMalloc(nbytes);
        return ptr[0 .. size];
    }

    static T[] realloc(T)(return scope T[] arr, size_t size) @trusted
    {
        import std.internal.memory : enforceRealloc;
        if (!size)
        {
            destroy(arr);
            return null;
        }

        import core.checkedint : mulu;
        bool overflow;
        size_t nbytes = mulu(size, T.sizeof, overflow);
        if (overflow) assert(0);

        auto ptr = cast(T*) enforceRealloc(arr.ptr, nbytes);
        return ptr[0 .. size];
    }

    static void replaceImpl(T, Range)(ref T[] dest, size_t from, size_t to, Range stuff)
    {
        genericReplace!(ReallocPolicy)(dest, from, to, stuff);
    }

    static void append(T, V)(ref T[] arr, V value)
    if (!isInputRange!V)
    {
        if (arr.length == size_t.max) assert(0);
        arr = realloc(arr, arr.length+1);
        arr[$-1] = force!T(value);
    }

    pure @safe unittest
    {
        int[] arr;
        ReallocPolicy.append(arr, 3);

        import std.algorithm.comparison : equal;
        assert(equal(arr, [3]));
    }

    static void append(T, V)(ref T[] arr, V value)
    if (isInputRange!V && hasLength!V)
    {
        import core.checkedint : addu;
        bool overflow;
        size_t nelems = addu(arr.length, value.length, overflow);
        if (overflow) assert(0);

        arr = realloc(arr, nelems);

        import std.algorithm.mutation : copy;
        copy(value, arr[$-value.length..$]);
    }

    pure @safe unittest
    {
        int[] arr;
        ReallocPolicy.append(arr, [1,2,3]);

        import std.algorithm.comparison : equal;
        assert(equal(arr, [1,2,3]));
    }

    static void destroy(T)(scope ref T[] arr) @trusted
    {
        import core.memory : pureFree;
        if (arr.ptr)
            pureFree(arr.ptr);
        arr = null;
    }
}

//build hack
alias _RealArray = CowArray!ReallocPolicy;

pure @safe unittest
{
    import std.algorithm.comparison : equal;

    with(ReallocPolicy)
    {
        bool test(T, U, V)(T orig, size_t from, size_t to, U toReplace, V result,
                   string file = __FILE__, size_t line = __LINE__)
        {
            {
                replaceImpl(orig, from, to, toReplace);
                scope(exit) destroy(orig);
                if (!equal(orig, result))
                    return false;
            }
            return true;
        }
        static T[] arr(T)(T[] args... )
        {
            return dup(args);
        }

        assert(test(arr([1, 2, 3, 4]), 0, 0, [5, 6, 7], [5, 6, 7, 1, 2, 3, 4]));
        assert(test(arr([1, 2, 3, 4]), 0, 2, cast(int[])[], [3, 4]));
        assert(test(arr([1, 2, 3, 4]), 0, 4, [5, 6, 7], [5, 6, 7]));
        assert(test(arr([1, 2, 3, 4]), 0, 2, [5, 6, 7], [5, 6, 7, 3, 4]));
        assert(test(arr([1, 2, 3, 4]), 2, 3, [5, 6, 7], [1, 2, 5, 6, 7, 4]));
    }
}

/**
    Tests if T is some kind a set of code points. Intended for template constraints.
*/
public template isCodepointSet(T)
{
    static if (is(T dummy == InversionList!(Args), Args...))
        enum isCodepointSet = true;
    else
        enum isCodepointSet = false;
}

/**
    Tests if `T` is a pair of integers that implicitly convert to `V`.
    The following code must compile for any pair `T`:
    ---
    (T x){ V a = x[0]; V b = x[1];}
    ---
    The following must not compile:
     ---
    (T x){ V c = x[2];}
    ---
*/
public template isIntegralPair(T, V=uint)
{
    enum isIntegralPair = is(typeof((T x){ V a = x[0]; V b = x[1];}))
        && !is(typeof((T x){ V c = x[2]; }));
}


/**
    The recommended default type for set of $(CODEPOINTS).
    For details, see the current implementation: $(LREF InversionList).
*/
public alias CodepointSet = InversionList!GcPolicy;


//@@@BUG: std.typecons tuples depend on std.format to produce fields mixin
// which relies on std.uni.isGraphical and this chain blows up with Forward reference error
// hence below doesn't seem to work
// public alias CodepointInterval = Tuple!(uint, "a", uint, "b");

/**
    The recommended type of $(REF Tuple, std,_typecons)
    to represent [a, b$(RPAREN) intervals of $(CODEPOINTS). As used in $(LREF InversionList).
    Any interval type should pass $(LREF isIntegralPair) trait.
*/
public struct CodepointInterval
{
pure:
    uint[2] _tuple;
    alias _tuple this;

@safe pure nothrow @nogc:

    this(uint low, uint high)
    {
        _tuple[0] = low;
        _tuple[1] = high;
    }
    bool opEquals(T)(T val) const
    {
        return this[0] == val[0] && this[1] == val[1];
    }
    @property ref inout(uint) a() return inout { return _tuple[0]; }
    @property ref inout(uint) b() return inout { return _tuple[1]; }
}

/**
    $(P
    `InversionList` is a set of $(CODEPOINTS)
    represented as an array of open-right [a, b$(RPAREN)
    intervals (see $(LREF CodepointInterval) above).
    The name comes from the way the representation reads left to right.
    For instance a set of all values [10, 50$(RPAREN), [80, 90$(RPAREN),
    plus a singular value 60 looks like this:
    )
    ---
    10, 50, 60, 61, 80, 90
    ---
    $(P
    The way to read this is: start with negative meaning that all numbers
    smaller then the next one are not present in this set (and positive -
    the contrary). Then switch positive/negative after each
    number passed from left to right.
    )
    $(P This way negative spans until 10, then positive until 50,
    then negative until 60, then positive until 61, and so on.
    As seen this provides a space-efficient storage of highly redundant data
    that comes in long runs. A description which Unicode $(CHARACTER)
    properties fit nicely. The technique itself could be seen as a variation
    on $(LINK2 https://en.wikipedia.org/wiki/Run-length_encoding, RLE encoding).
    )

    $(P Sets are value types (just like `int` is) thus they
        are never aliased.
    )
        Example:
        ---
        auto a = CodepointSet('a', 'z'+1);
        auto b = CodepointSet('A', 'Z'+1);
        auto c = a;
        a = a | b;
        assert(a == CodepointSet('A', 'Z'+1, 'a', 'z'+1));
        assert(a != c);
        ---
    $(P See also $(LREF unicode) for simpler construction of sets
        from predefined ones.
    )

    $(P Memory usage is 8 bytes per each contiguous interval in a set.
    The value semantics are achieved by using the
    $(HTTP en.wikipedia.org/wiki/Copy-on-write, COW) technique
    and thus it's $(RED not) safe to cast this type to $(D_KEYWORD shared).
    )

    Note:
    $(P It's not recommended to rely on the template parameters
    or the exact type of a current $(CODEPOINT) set in `std.uni`.
    The type and parameters may change when the standard
    allocators design is finalized.
    Use $(LREF isCodepointSet) with templates or just stick with the default
    alias $(LREF CodepointSet) throughout the whole code base.
    )
*/
public struct InversionList(SP=GcPolicy)
{
    import std.range : assumeSorted;

    /**
        Construct from another code point set of any type.
    */
    this(Set)(Set set) pure
    if (isCodepointSet!Set)
    {
        uint[] arr;
        foreach (v; set.byInterval)
        {
            arr ~= v.a;
            arr ~= v.b;
        }
        data = CowArray!(SP).reuse(arr);
    }

    /**
        Construct a set from a forward range of code point intervals.
    */
    this(Range)(Range intervals) pure
    if (isForwardRange!Range && isIntegralPair!(ElementType!Range))
    {
        uint[] arr;
        foreach (v; intervals)
        {
            SP.append(arr, v.a);
            SP.append(arr, v.b);
        }
        data = CowArray!(SP).reuse(arr);
        sanitize(); //enforce invariant: sort intervals etc.
    }

    //helper function that avoids sanity check to be CTFE-friendly
    private static fromIntervals(Range)(Range intervals) pure
    {
        import std.algorithm.iteration : map;
        import std.range : roundRobin;
        auto flattened = roundRobin(intervals.save.map!"a[0]"(),
            intervals.save.map!"a[1]"());
        InversionList set;
        set.data = CowArray!(SP)(flattened);
        return set;
    }
    //ditto untill sort is CTFE-able
    private static fromIntervals()(uint[] intervals...) pure
    in
    {
        import std.conv : text;
        assert(intervals.length % 2 == 0, "Odd number of interval bounds [a, b)!");
        for (uint i = 0; i < intervals.length; i += 2)
        {
            auto a = intervals[i], b = intervals[i+1];
            assert(a < b, text("illegal interval [a, b): ", a, " > ", b));
        }
    }
    do
    {
        InversionList set;
        set.data = CowArray!(SP)(intervals);
        return set;
    }

    /**
        Construct a set from plain values of code point intervals.
    */
    this()(uint[] intervals...)
    in
    {
        import std.conv : text;
        assert(intervals.length % 2 == 0, "Odd number of interval bounds [a, b)!");
        for (uint i = 0; i < intervals.length; i += 2)
        {
            auto a = intervals[i], b = intervals[i+1];
            assert(a < b, text("illegal interval [a, b): ", a, " > ", b));
        }
    }
    do
    {
        data = CowArray!(SP)(intervals);
        sanitize(); //enforce invariant: sort intervals etc.
    }

    ///
    pure @safe unittest
    {
        import std.algorithm.comparison : equal;

        auto set = CodepointSet('a', 'z'+1, 'а', 'я'+1);
        foreach (v; 'a'..'z'+1)
            assert(set[v]);
        // Cyrillic lowercase interval
        foreach (v; 'а'..'я'+1)
            assert(set[v]);
        //specific order is not required, intervals may interesect
        auto set2 = CodepointSet('а', 'я'+1, 'a', 'd', 'b', 'z'+1);
        //the same end result
        assert(set2.byInterval.equal(set.byInterval));
        // test constructor this(Range)(Range intervals)
        auto chessPiecesWhite = CodepointInterval(9812, 9818);
        auto chessPiecesBlack = CodepointInterval(9818, 9824);
        auto set3 = CodepointSet([chessPiecesWhite, chessPiecesBlack]);
        foreach (v; '♔'..'♟'+1)
            assert(set3[v]);
    }

    /**
        Get range that spans all of the $(CODEPOINT) intervals in this $(LREF InversionList).
    */
    @property auto byInterval() scope
    {
        // TODO: change this to data[] once the -dip1000 errors have been fixed
        // see e.g. https://github.com/dlang/phobos/pull/6638
        import std.array : array;
        return Intervals!(typeof(data.array))(data.array);
    }

    @safe unittest
    {
        import std.algorithm.comparison : equal;
        import std.typecons : tuple;

        auto set = CodepointSet('A', 'D'+1, 'a', 'd'+1);

        assert(set.byInterval.equal([tuple('A','E'), tuple('a','e')]));
    }

    package(std) @property const(CodepointInterval)[] intervals() const
    {
        import std.array : array;
        return Intervals!(typeof(data[]))(data[]).array;
    }

    /**
        Tests the presence of code point `val` in this set.
    */
    bool opIndex(uint val) const
    {
        // the <= ensures that searching in  interval of [a, b) for 'a' you get .length == 1
        // return assumeSorted!((a,b) => a <= b)(data[]).lowerBound(val).length & 1;
        return sharSwitchLowerBound!"a <= b"(data[], val) & 1;
    }

    ///
    pure @safe unittest
    {
        auto gothic = unicode.Gothic;
        // Gothic letter ahsa
        assert(gothic['\U00010330']);
        // no ascii in Gothic obviously
        assert(!gothic['$']);
    }


    // Linear scan for `ch`. Useful only for small sets.
    // TODO:
    // used internally in std.regex
    // should be properly exposed in a public API ?
    package(std) auto scanFor()(dchar ch) const
    {
        immutable len = data.length;
        for (size_t i = 0; i < len; i++)
            if (ch < data[i])
                return i & 1;
        return 0;
    }

    /// Number of $(CODEPOINTS) in this set
    @property size_t length()
    {
        size_t sum = 0;
        foreach (iv; byInterval)
        {
            sum += iv.b - iv.a;
        }
        return sum;
    }

// bootstrap full set operations from 4 primitives (suitable as a template mixin):
// addInterval, skipUpTo, dropUpTo & byInterval iteration
//============================================================================
public:
    /**
        $(P Sets support natural syntax for set algebra, namely: )
        $(BOOKTABLE ,
            $(TR $(TH Operator) $(TH Math notation) $(TH Description) )
            $(TR $(TD &) $(TD a ∩ b) $(TD intersection) )
            $(TR $(TD |) $(TD a ∪ b) $(TD union) )
            $(TR $(TD -) $(TD a ∖ b) $(TD subtraction) )
            $(TR $(TD ~) $(TD a ~ b) $(TD symmetric set difference i.e. (a ∪ b) \ (a ∩ b)) )
        )
    */
    This opBinary(string op, U)(U rhs)
    if (isCodepointSet!U || is(U:dchar))
    {
        static if (op == "&" || op == "|" || op == "~")
        {// symmetric ops thus can swap arguments to reuse r-value
            static if (is(U:dchar))
            {
                auto tmp = this;
                mixin("tmp "~op~"= rhs; ");
                return tmp;
            }
            else
            {
                static if (is(Unqual!U == U))
                {
                    // try hard to reuse r-value
                    mixin("rhs "~op~"= this;");
                    return rhs;
                }
                else
                {
                    auto tmp = this;
                    mixin("tmp "~op~"= rhs;");
                    return tmp;
                }
            }
        }
        else static if (op == "-") // anti-symmetric
        {
            auto tmp = this;
            tmp -= rhs;
            return tmp;
        }
        else
            static assert(0, "no operator "~op~" defined for Set");
    }

    ///
    pure @safe unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : iota;

        auto lower = unicode.LowerCase;
        auto upper = unicode.UpperCase;
        auto ascii = unicode.ASCII;

        assert((lower & upper).empty); // no intersection
        auto lowerASCII = lower & ascii;
        assert(lowerASCII.byCodepoint.equal(iota('a', 'z'+1)));
        // throw away all of the lowercase ASCII
        assert((ascii - lower).length == 128 - 26);

        auto onlyOneOf = lower ~ ascii;
        assert(!onlyOneOf['Δ']); // not ASCII and not lowercase
        assert(onlyOneOf['$']); // ASCII and not lowercase
        assert(!onlyOneOf['a']); // ASCII and lowercase
        assert(onlyOneOf['я']); // not ASCII but lowercase

        // throw away all cased letters from ASCII
        auto noLetters = ascii - (lower | upper);
        assert(noLetters.length == 128 - 26*2);
    }

    /// The 'op=' versions of the above overloaded operators.
    ref This opOpAssign(string op, U)(U rhs)
    if (isCodepointSet!U || is(U:dchar))
    {
        static if (op == "|")    // union
        {
            static if (is(U:dchar))
            {
                this.addInterval(rhs, rhs+1);
                return this;
            }
            else
                return this.add(rhs);
        }
        else static if (op == "&")   // intersection
                return this.intersect(rhs);// overloaded
        else static if (op == "-")   // set difference
                return this.sub(rhs);// overloaded
        else static if (op == "~")   // symmetric set difference
        {
            auto copy = this & rhs;
            this |= rhs;
            this -= copy;
            return this;
        }
        else
            static assert(0, "no operator "~op~" defined for Set");
    }

    /**
        Tests the presence of codepoint `ch` in this set,
        the same as $(LREF opIndex).
    */
    bool opBinaryRight(string op: "in", U)(U ch) const
    if (is(U : dchar))
    {
        return this[ch];
    }

    ///
    pure @safe unittest
    {
        assert('я' in unicode.Cyrillic);
        assert(!('z' in unicode.Cyrillic));
    }



    /**
     * Obtains a set that is the inversion of this set.
     *
     * See_Also: $(LREF inverted)
     */
    auto opUnary(string op: "!")()
    {
        return this.inverted;
    }

    /**
        A range that spans each $(CODEPOINT) in this set.
    */
    @property auto byCodepoint()
    {
        static struct CodepointRange
        {
            this(This set)
            {
                r = set.byInterval;
                if (!r.empty)
                    cur = r.front.a;
            }

            @property dchar front() const
            {
                return cast(dchar) cur;
            }

            @property bool empty() const
            {
                return r.empty;
            }

            void popFront()
            {
                cur++;
                while (cur >= r.front.b)
                {
                    r.popFront();
                    if (r.empty)
                        break;
                    cur = r.front.a;
                }
            }
        private:
            uint cur;
            typeof(This.init.byInterval) r;
        }

        return CodepointRange(this);
    }

    ///
    pure @safe unittest
    {
        import std.algorithm.comparison : equal;
        import std.range : iota;

        auto set = unicode.ASCII;
        set.byCodepoint.equal(iota(0, 0x80));
    }

    /**
        $(P Obtain textual representation of this set in from of
        open-right intervals and feed it to `sink`.
        )
        $(P Used by various standard formatting facilities such as
         $(REF formattedWrite, std,format), $(REF write, std,stdio),
         $(REF writef, std,stdio), $(REF to, std,conv) and others.
        )
        Example:
        ---
        import std.conv;
        assert(unicode.ASCII.to!string == "[0..128$(RPAREN)");
        ---
    */

    private import std.format.spec : FormatSpec;

    /***************************************
     * Obtain a textual representation of this InversionList
     * in form of open-right intervals.
     *
     * The formatting flag is applied individually to each value, for example:
     * $(LI $(B %s) and $(B %d) format the intervals as a [low .. high$(RPAREN) range of integrals)
     * $(LI $(B %x) formats the intervals as a [low .. high$(RPAREN) range of lowercase hex characters)
     * $(LI $(B %X) formats the intervals as a [low .. high$(RPAREN) range of uppercase hex characters)
     */
    void toString(Writer)(scope Writer sink, scope const ref FormatSpec!char fmt) /* const */
    {
        import std.format.write : formatValue;
        auto range = byInterval;
        if (range.empty)
            return;

        while (1)
        {
            auto i = range.front;
            range.popFront();

            put(sink, "[");
            formatValue(sink, i.a, fmt);
            put(sink, "..");
            formatValue(sink, i.b, fmt);
            put(sink, ")");
            if (range.empty) return;
            put(sink, " ");
        }
    }

    ///
    pure @safe unittest
    {
        import std.conv : to;
        import std.format : format;
        import std.uni : unicode;

        // This was originally using Cyrillic script.
        // Unfortunately this is a pretty active range for changes,
        // and hence broke in an update.
        // Therefore the range Basic latin was used instead as it
        // unlikely to ever change.

        assert(unicode.InBasic_latin.to!string == "[0..128)");

        // The specs '%s' and '%d' are equivalent to the to!string call above.
        assert(format("%d", unicode.InBasic_latin) == unicode.InBasic_latin.to!string);

        assert(format("%#x", unicode.InBasic_latin) == "[0..0x80)");
        assert(format("%#X", unicode.InBasic_latin) == "[0..0X80)");
    }

    pure @safe unittest
    {
        import std.exception : assertThrown;
        import std.format : format, FormatException;
        assertThrown!FormatException(format("%z", unicode.ASCII));
    }


    /**
        Add an interval [a, b$(RPAREN) to this set.
    */
    ref add()(uint a, uint b)
    {
        addInterval(a, b);
        return this;
    }

    ///
    pure @safe unittest
    {
        CodepointSet someSet;
        someSet.add('0', '5').add('A','Z'+1);
        someSet.add('5', '9'+1);
        assert(someSet['0']);
        assert(someSet['5']);
        assert(someSet['9']);
        assert(someSet['Z']);
    }

private:

  package(std)  // used from: std.regex.internal.parser
    ref intersect(U)(U rhs)
    if (isCodepointSet!U)
    {
        Marker mark;
        foreach ( i; rhs.byInterval)
        {
            mark = this.dropUpTo(i.a, mark);
            mark = this.skipUpTo(i.b, mark);
        }
        this.dropUpTo(uint.max, mark);
        return this;
    }

    ref intersect()(dchar ch)
    {
        foreach (i; byInterval)
            if (i.a <= ch && ch < i.b)
                return this = This.init.add(ch, ch+1);
        this = This.init;
        return this;
    }

    pure @safe unittest
    {
        assert(unicode.Cyrillic.intersect('-').byInterval.empty);
    }

    ref sub()(dchar ch)
    {
        return subChar(ch);
    }

    // same as the above except that skip & drop parts are swapped
  package(std)  // used from: std.regex.internal.parser
    ref sub(U)(U rhs)
    if (isCodepointSet!U)
    {
        Marker mark;
        foreach (i; rhs.byInterval)
        {
            mark = this.skipUpTo(i.a, mark);
            mark = this.dropUpTo(i.b, mark);
        }
        return this;
    }

  package(std)  // used from: std.regex.internal.parse
    ref add(U)(U rhs)
    if (isCodepointSet!U)
    {
        Marker start;
        foreach (i; rhs.byInterval)
        {
            start = addInterval(i.a, i.b, start);
        }
        return this;
    }

// end of mixin-able part
//============================================================================
public:
    /**
        Obtains a set that is the inversion of this set.

        See the '!' $(LREF opUnary) for the same but using operators.
    */
    @property auto inverted()
    {
        InversionList inversion = this;
        if (inversion.data.length == 0)
        {
            inversion.addInterval(0, lastDchar+1);
            return inversion;
        }
        if (inversion.data[0] != 0)
            genericReplace(inversion.data, 0, 0, [0]);
        else
            genericReplace(inversion.data, 0, 1, cast(uint[]) null);
        if (data[data.length-1] != lastDchar+1)
            genericReplace(inversion.data,
                inversion.data.length, inversion.data.length, [lastDchar+1]);
        else
            genericReplace(inversion.data,
                inversion.data.length-1, inversion.data.length, cast(uint[]) null);

        return inversion;
    }

    ///
    pure @safe unittest
    {
        auto set = unicode.ASCII;
        // union with the inverse gets all of the code points in the Unicode
        assert((set | set.inverted).length == 0x110000);
        // no intersection with the inverse
        assert((set & set.inverted).empty);
    }

    package(std) static string toSourceCode(const(CodepointInterval)[] range, string funcName)
    {
        import std.algorithm.searching : countUntil;
        import std.format : format;
        enum maxBinary = 3;
        static string linearScope(R)(R ivals, string indent)
        {
            string result = indent~"{\n";
            string deeper = indent~"    ";
            foreach (ival; ivals)
            {
                immutable span = ival[1] - ival[0];
                assert(span != 0);
                if (span == 1)
                {
                    result ~= format("%sif (ch == %s) return true;\n", deeper, ival[0]);
                }
                else if (span == 2)
                {
                    result ~= format("%sif (ch == %s || ch == %s) return true;\n",
                        deeper, ival[0], ival[0]+1);
                }
                else
                {
                    if (ival[0] != 0) // dchar is unsigned and  < 0 is useless
                        result ~= format("%sif (ch < %s) return false;\n", deeper, ival[0]);
                    result ~= format("%sif (ch < %s) return true;\n", deeper, ival[1]);
                }
            }
            result ~= format("%sreturn false;\n%s}\n", deeper, indent); // including empty range of intervals
            return result;
        }

        static string binaryScope(R)(R ivals, string indent) @safe
        {
            // time to do unrolled comparisons?
            if (ivals.length < maxBinary)
                return linearScope(ivals, indent);
            else
                return bisect(ivals, ivals.length/2, indent);
        }

        // not used yet if/elsebinary search is far better with DMD  as of 2.061
        // and GDC is doing fine job either way
        static string switchScope(R)(R ivals, string indent)
        {
            string result = indent~"switch (ch){\n";
            string deeper = indent~"    ";
            foreach (ival; ivals)
            {
                if (ival[0]+1 == ival[1])
                {
                    result ~= format("%scase %s: return true;\n",
                        deeper, ival[0]);
                }
                else
                {
                    result ~= format("%scase %s: .. case %s: return true;\n",
                         deeper, ival[0], ival[1]-1);
                }
            }
            result ~= deeper~"default: return false;\n"~indent~"}\n";
            return result;
        }

        static string bisect(R)(R range, size_t idx, string indent)
        {
            string deeper = indent ~ "    ";
            // bisect on one [a, b) interval at idx
            string result = indent~"{\n";
            // less branch, < a
            result ~= format("%sif (ch < %s)\n%s",
                deeper, range[idx][0], binaryScope(range[0 .. idx], deeper));
            // middle point,  >= a && < b
            result ~= format("%selse if (ch < %s) return true;\n",
                deeper, range[idx][1]);
            // greater or equal branch,  >= b
            result ~= format("%selse\n%s",
                deeper, binaryScope(range[idx+1..$], deeper));
            return result~indent~"}\n";
        }

        string code = format("bool %s(dchar ch) @safe pure nothrow @nogc\n",
            funcName.empty ? "function" : funcName);
        // special case first bisection to be on ASCII vs beyond
        auto tillAscii = countUntil!"a[0] > 0x80"(range);
        if (tillAscii <= 0) // everything is ASCII or nothing is ascii (-1 & 0)
            code ~= binaryScope(range, "");
        else
            code ~= bisect(range, tillAscii, "");
        return code;
    }

    /**
        Generates string with D source code of unary function with name of
        `funcName` taking a single `dchar` argument. If `funcName` is empty
        the code is adjusted to be a lambda function.

        The function generated tests if the $(CODEPOINT) passed
        belongs to this set or not. The result is to be used with string mixin.
        The intended usage area is aggressive optimization via meta programming
        in parser generators and the like.

        Note: Use with care for relatively small or regular sets. It
        could end up being slower then just using multi-staged tables.

        Example:
        ---
        import std.stdio;

        // construct set directly from [a, b$RPAREN intervals
        auto set = CodepointSet(10, 12, 45, 65, 100, 200);
        writeln(set);
        writeln(set.toSourceCode("func"));
        ---

        The above outputs something along the lines of:
        ---
        bool func(dchar ch)  @safe pure nothrow @nogc
        {
            if (ch < 45)
            {
                if (ch == 10 || ch == 11) return true;
                return false;
            }
            else if (ch < 65) return true;
            else
            {
                if (ch < 100) return false;
                if (ch < 200) return true;
                return false;
            }
        }
        ---
    */
    string toSourceCode(string funcName="")
    {
        import std.array : array;
        auto range = byInterval.array();
        return toSourceCode(range, funcName);
    }

    /**
        True if this set doesn't contain any $(CODEPOINTS).
    */
    @property bool empty() const
    {
        return data.length == 0;
    }

    ///
    pure @safe unittest
    {
        CodepointSet emptySet;
        assert(emptySet.length == 0);
        assert(emptySet.empty);
    }

private:
    alias This = typeof(this);
    alias Marker = size_t;

    // a random-access range of integral pairs
    static struct Intervals(Range)
    {
        import std.range.primitives : hasAssignableElements;

        this(Range sp) scope
        {
            slice = sp;
            start = 0;
            end = sp.length;
        }

        this(Range sp, size_t s, size_t e) scope
        {
            slice = sp;
            start = s;
            end = e;
        }

        @property auto front()const
        {
            immutable a = slice[start];
            immutable b = slice[start+1];
            return CodepointInterval(a, b);
        }

        //may break sorted property - but we need std.sort to access it
        //hence package(std) protection attribute
        static if (hasAssignableElements!Range)
        package(std) @property void front(CodepointInterval val)
        {
            slice[start] = val.a;
            slice[start+1] = val.b;
        }

        @property auto back()const
        {
            immutable a = slice[end-2];
            immutable b = slice[end-1];
            return CodepointInterval(a, b);
        }

        //ditto about package
        static if (hasAssignableElements!Range)
        package(std) @property void back(CodepointInterval val)
        {
            slice[end-2] = val.a;
            slice[end-1] = val.b;
        }

        void popFront()
        {
            start += 2;
        }

        void popBack()
        {
            end -= 2;
        }

        auto opIndex(size_t idx) const
        {
            immutable a = slice[start+idx*2];
            immutable b = slice[start+idx*2+1];
            return CodepointInterval(a, b);
        }

        //ditto about package
        static if (hasAssignableElements!Range)
        package(std) void opIndexAssign(CodepointInterval val, size_t idx)
        {
            slice[start+idx*2] = val.a;
            slice[start+idx*2+1] = val.b;
        }

        auto opSlice(size_t s, size_t e)
        {
            return Intervals(slice, s*2+start, e*2+start);
        }

        @property size_t length()const {  return slice.length/2; }

        @property bool empty()const { return start == end; }

        @property auto save(){ return this; }
    private:
        size_t start, end;
        Range slice;
    }

    // called after construction from intervals
    // to make sure invariants hold
    void sanitize()
    {
        import std.algorithm.comparison : max;
        import std.algorithm.mutation : SwapStrategy;
        import std.algorithm.sorting : sort;
        if (data.length == 0)
            return;
        alias Ival = CodepointInterval;
        //intervals wrapper for a _range_ over packed array
        auto ivals = Intervals!(typeof(data[]))(data[]);
        //@@@BUG@@@ can't use "a.a < b.a" see
        // https://issues.dlang.org/show_bug.cgi?id=12265
        sort!((a,b) => a.a < b.a, SwapStrategy.stable)(ivals);
        // what follows is a variation on stable remove
        // differences:
        // - predicate is binary, and is tested against
        //   the last kept element (at 'i').
        // - predicate mutates lhs (merges rhs into lhs)
        size_t len = ivals.length;
        size_t i = 0;
        size_t j = 1;
        while (j < len)
        {
            if (ivals[i].b >= ivals[j].a)
            {
                ivals[i] = Ival(ivals[i].a, max(ivals[i].b, ivals[j].b));
                j++;
            }
            else //unmergable
            {
                // check if there is a hole after merges
                // (in the best case we do 0 writes to ivals)
                if (j != i+1)
                    ivals[i+1] = ivals[j]; //copy over
                i++;
                j++;
            }
        }
        len = i + 1;
        for (size_t k=0; k + 1 < len; k++)
        {
            assert(ivals[k].a < ivals[k].b);
            assert(ivals[k].b < ivals[k+1].a);
        }
        data.length = len * 2;
    }

    // special case for normal InversionList
    ref subChar(dchar ch)
    {
        auto mark = skipUpTo(ch);
        if (mark != data.length
            && data[mark] == ch && data[mark-1] == ch)
        {
            // it has split, meaning that ch happens to be in one of intervals
            data[mark] = data[mark]+1;
        }
        return this;
    }

    //
    Marker addInterval(int a, int b, Marker hint=Marker.init) scope
    in
    {
        assert(a <= b);
    }
    do
    {
        import std.range : assumeSorted, SearchPolicy;
        auto range = assumeSorted(data[]);
        size_t pos;
        size_t a_idx = hint + range[hint..$].lowerBound!(SearchPolicy.gallop)(a).length;
        if (a_idx == range.length)
        {
            //  [---+++----++++----++++++]
            //  [                         a  b]
            data.append(a, b);
            return data.length-1;
        }
        size_t b_idx = range[a_idx .. range.length].lowerBound!(SearchPolicy.gallop)(b).length+a_idx;
        uint[3] buf = void;
        uint to_insert;
        debug(std_uni)
        {
            writefln("a_idx=%d; b_idx=%d;", a_idx, b_idx);
        }
        if (b_idx == range.length)
        {
            //  [-------++++++++----++++++-]
            //  [      s     a                 b]
            if (a_idx & 1)// a in positive
            {
                buf[0] = b;
                to_insert = 1;
            }
            else// a in negative
            {
                buf[0] = a;
                buf[1] = b;
                to_insert = 2;
            }
            pos = genericReplace(data, a_idx, b_idx, buf[0 .. to_insert]);
            return pos - 1;
        }

        uint top = data[b_idx];

        debug(std_uni)
        {
            writefln("a_idx=%d; b_idx=%d;", a_idx, b_idx);
            writefln("a=%s; b=%s; top=%s;", a, b, top);
        }
        if (a_idx & 1)
        {// a in positive
            if (b_idx & 1)// b in positive
            {
                //  [-------++++++++----++++++-]
                //  [       s    a        b    ]
                buf[0] = top;
                to_insert = 1;
            }
            else // b in negative
            {
                //  [-------++++++++----++++++-]
                //  [       s    a   b         ]
                if (top == b)
                {
                    assert(b_idx+1 < data.length);
                    buf[0] = data[b_idx+1];
                    pos = genericReplace(data, a_idx, b_idx+2, buf[0 .. 1]);
                    return pos - 1;
                }
                buf[0] = b;
                buf[1] = top;
                to_insert = 2;
            }
        }
        else
        { // a in negative
            if (b_idx & 1) // b in positive
            {
                //  [----------+++++----++++++-]
                //  [     a     b              ]
                buf[0] = a;
                buf[1] = top;
                to_insert = 2;
            }
            else// b in negative
            {
                //  [----------+++++----++++++-]
                //  [  a       s      b        ]
                if (top == b)
                {
                    assert(b_idx+1 < data.length);
                    buf[0] = a;
                    buf[1] = data[b_idx+1];
                    pos = genericReplace(data, a_idx, b_idx+2, buf[0 .. 2]);
                    return pos - 1;
                }
                buf[0] = a;
                buf[1] = b;
                buf[2] = top;
                to_insert = 3;
            }
        }
        pos = genericReplace(data, a_idx, b_idx+1, buf[0 .. to_insert]);
        debug(std_uni)
        {
            writefln("marker idx: %d; length=%d", pos, data[pos], data.length);
            writeln("inserting ", buf[0 .. to_insert]);
        }
        return pos - 1;
    }

    //
    Marker dropUpTo(uint a, Marker pos=Marker.init)
    in
    {
        assert(pos % 2 == 0); // at start of interval
    }
    do
    {
        auto range = assumeSorted!"a <= b"(data[pos .. data.length]);
        if (range.empty)
            return pos;
        size_t idx = pos;
        idx += range.lowerBound(a).length;

        debug(std_uni)
        {
            writeln("dropUpTo full length=", data.length);
            writeln(pos,"~~~", idx);
        }
        if (idx == data.length)
            return genericReplace(data, pos, idx, cast(uint[])[]);
        if (idx & 1)
        {   // a in positive
            //[--+++----++++++----+++++++------...]
            //      |<---si       s  a  t
            genericReplace(data, pos, idx, [a]);
        }
        else
        {   // a in negative
            //[--+++----++++++----+++++++-------+++...]
            //      |<---si              s  a  t
            genericReplace(data, pos, idx, cast(uint[])[]);
        }
        return pos;
    }

    //
    Marker skipUpTo(uint a, Marker pos=Marker.init)
    out(result)
    {
        assert(result % 2 == 0);// always start of interval
        //(may be  0-width after-split)
    }
    do
    {
        assert(data.length % 2 == 0);
        auto range = assumeSorted!"a <= b"(data[pos .. data.length]);
        size_t idx = pos+range.lowerBound(a).length;

        if (idx >= data.length) // could have Marker point to recently removed stuff
            return data.length;

        if (idx & 1)// inside of interval, check for split
        {

            immutable top = data[idx];
            if (top == a)// no need to split, it's end
                return idx+1;
            immutable start = data[idx-1];
            if (a == start)
                return idx-1;
            // split it up
            genericReplace(data, idx, idx+1, [a, a, top]);
            return idx+1;        // avoid odd index
        }
        return idx;
    }

    CowArray!SP data;
}

pure @safe unittest
{
    import std.conv : to;
    assert(unicode.ASCII.to!string() == "[0..128)");
}

// pedantic version for ctfe, and aligned-access only architectures
@system private uint safeRead24(scope const ubyte* ptr, size_t idx) pure nothrow @nogc
{
    idx *= 3;
    version (LittleEndian)
        return ptr[idx] + (cast(uint) ptr[idx+1]<<8)
             + (cast(uint) ptr[idx+2]<<16);
    else
        return (cast(uint) ptr[idx]<<16) + (cast(uint) ptr[idx+1]<<8)
             + ptr[idx+2];
}

// ditto
@system private void safeWrite24(scope ubyte* ptr, uint val, size_t idx) pure nothrow @nogc
{
    idx *= 3;
    version (LittleEndian)
    {
        ptr[idx] = val & 0xFF;
        ptr[idx+1] = (val >> 8) & 0xFF;
        ptr[idx+2] = (val >> 16) & 0xFF;
    }
    else
    {
        ptr[idx] = (val >> 16) & 0xFF;
        ptr[idx+1] = (val >> 8) & 0xFF;
        ptr[idx+2] = val & 0xFF;
    }
}

// unaligned x86-like read/write functions
@system private uint unalignedRead24(scope const ubyte* ptr, size_t idx) pure nothrow @nogc
{
    uint* src = cast(uint*)(ptr+3*idx);
    version (LittleEndian)
        return *src & 0xFF_FFFF;
    else
        return *src >> 8;
}

// ditto
@system private void unalignedWrite24(scope ubyte* ptr, uint val, size_t idx) pure nothrow @nogc
{
    uint* dest = cast(uint*)(cast(ubyte*) ptr + 3*idx);
    version (LittleEndian)
        *dest = val | (*dest & 0xFF00_0000);
    else
        *dest = (val << 8) | (*dest & 0xFF);
}

@system private uint read24(scope const ubyte* ptr, size_t idx) pure nothrow @nogc
{
    static if (hasUnalignedReads)
        return __ctfe ? safeRead24(ptr, idx) : unalignedRead24(ptr, idx);
    else
        return safeRead24(ptr, idx);
}

@system private void write24(scope ubyte* ptr, uint val, size_t idx) pure nothrow @nogc
{
    static if (hasUnalignedReads)
        return __ctfe ? safeWrite24(ptr, val, idx) : unalignedWrite24(ptr, val, idx);
    else
        return safeWrite24(ptr, val, idx);
}

struct CowArray(SP=GcPolicy)
{
    import std.range.primitives : hasLength;

  @safe:
    static auto reuse(uint[] arr)
    {
        CowArray cow;
        cow.data = arr;
        SP.append(cow.data, 1);
        assert(cow.refCount == 1);
        assert(cow.length == arr.length);
        return cow;
    }

    this(Range)(Range range)
    if (isInputRange!Range && hasLength!Range)
    {
        import std.algorithm.mutation : copy;
        length = range.length;
        copy(range, data[0..$-1]);
    }

    this(Range)(Range range)
    if (isForwardRange!Range && !hasLength!Range)
    {
        import std.algorithm.mutation : copy;
        import std.range.primitives : walkLength;
        immutable len = walkLength(range.save);
        length = len;
        copy(range, data[0..$-1]);
    }

    this(this)
    {
        if (!empty)
        {
            refCount = refCount + 1;
        }
    }

    ~this()
    {
        if (!empty)
        {
            immutable cnt = refCount;
            if (cnt == 1)
                SP.destroy(data);
            else
                refCount = cnt - 1;
        }
    }

    // no ref-count for empty U24 array
    @property bool empty() const { return data.length == 0; }

    // report one less then actual size
    @property size_t length() const
    {
        return data.length ? data.length - 1 : 0;
    }

    //+ an extra slot for ref-count
    @property void length(size_t len)
    {
        import std.algorithm.comparison : min;
        import std.algorithm.mutation : copy;
        if (len == 0)
        {
            if (!empty)
                freeThisReference();
            return;
        }
        immutable total = len + 1; // including ref-count
        if (empty)
        {
            data = SP.alloc!uint(total);
            refCount = 1;
            return;
        }
        immutable cur_cnt = refCount;
        if (cur_cnt != 1) // have more references to this memory
        {
            refCount = cur_cnt - 1;
            auto new_data = SP.alloc!uint(total);
            // take shrinking into account
            auto to_copy = min(total, data.length) - 1;
            copy(data[0 .. to_copy], new_data[0 .. to_copy]);
            data = new_data; // before setting refCount!
            refCount = 1;
        }
        else // 'this' is the only reference
        {
            // use the realloc (hopefully in-place operation)
            data = SP.realloc(data, total);
            refCount = 1; // setup a ref-count in the new end of the array
        }
    }

    alias opDollar = length;

    uint opIndex()(size_t idx)const
    {
        return data[idx];
    }

    void opIndexAssign(uint val, size_t idx)
    {
        auto cnt = refCount;
        if (cnt != 1)
            dupThisReference(cnt);
        data[idx] = val;
    }

    //
    auto opSlice(size_t from, size_t to)
    {
        if (!empty)
        {
            auto cnt = refCount;
            if (cnt != 1)
                dupThisReference(cnt);
        }
        return data[from .. to];

    }

    //
    auto opSlice(size_t from, size_t to) const
    {
        return data[from .. to];
    }

    // length slices before the ref count
    auto opSlice()
    {
        return opSlice(0, length);
    }

    // ditto
    auto opSlice() const
    {
        return opSlice(0, length);
    }

    void append(Range)(Range range)
    if (isInputRange!Range && hasLength!Range && is(ElementType!Range : uint))
    {
        size_t nl = length + range.length;
        length = nl;
        copy(range, this[nl-range.length .. nl]);
    }

    void append()(uint[] val...)
    {
        length = length + val.length;
        data[$-val.length-1 .. $-1] = val[];
    }

    bool opEquals()(auto ref const CowArray rhs) const
    {
        if (empty ^ rhs.empty)
            return false; // one is empty and the other isn't
        return empty || data[0..$-1] == rhs.data[0..$-1];
    }

private:
    // ref-count is right after the data
    @property uint refCount() const
    {
        return data[$-1];
    }

    @property void refCount(uint cnt)
    {
        data[$-1] = cnt;
    }

    void freeThisReference()
    {
        immutable count = refCount;
        if (count != 1) // have more references to this memory
        {
            // dec shared ref-count
            refCount = count - 1;
            data = [];
        }
        else
            SP.destroy(data);
        assert(!data.ptr);
    }

    void dupThisReference(uint count)
    in
    {
        assert(!empty && count != 1 && count == refCount);
    }
    do
    {
        import std.algorithm.mutation : copy;
        // dec shared ref-count
        refCount = count - 1;
        // copy to the new chunk of RAM
        auto new_data = SP.alloc!uint(data.length);
        // bit-blit old stuff except the counter
        copy(data[0..$-1], new_data[0..$-1]);
        data = new_data; // before setting refCount!
        refCount = 1; // so that this updates the right one
    }

    uint[] data;
}

pure @safe unittest// Uint24 tests
{
    import std.algorithm.comparison : equal;
    import std.algorithm.mutation : copy;
    import std.conv : text;
    import std.range : iota, chain;
    import std.range.primitives : isBidirectionalRange, isOutputRange;
    void funcRef(T)(ref T u24)
    {
        u24.length = 2;
        u24[1] = 1024;
        T u24_c = u24;
        assert(u24[1] == 1024);
        u24.length = 0;
        assert(u24.empty);
        u24.append([1, 2]);
        assert(equal(u24[], [1, 2]));
        u24.append(111);
        assert(equal(u24[], [1, 2, 111]));
        assert(!u24_c.empty && u24_c[1] == 1024);
        u24.length = 3;
        copy(iota(0, 3), u24[]);
        assert(equal(u24[], iota(0, 3)));
        assert(u24_c[1] == 1024);
    }

    void func2(T)(T u24)
    {
        T u24_2 = u24;
        T u24_3;
        u24_3 = u24_2;
        assert(u24_2 == u24_3);
        assert(equal(u24[], u24_2[]));
        assert(equal(u24_2[], u24_3[]));
        funcRef(u24_3);

        assert(equal(u24_3[], iota(0, 3)));
        assert(!equal(u24_2[], u24_3[]));
        assert(equal(u24_2[], u24[]));
        u24_2 = u24_3;
        assert(equal(u24_2[], iota(0, 3)));
        // to test that passed arg is intact outside
        // plus try out opEquals
        u24 = u24_3;
        u24 = T.init;
        u24_3 = T.init;
        assert(u24.empty);
        assert(u24 == u24_3);
        assert(u24 != u24_2);
    }

    static foreach (Policy; AliasSeq!(GcPolicy, ReallocPolicy))
    {{
        alias Range = typeof(CowArray!Policy.init[]);
        alias U24A = CowArray!Policy;
        static assert(isForwardRange!Range);
        static assert(isBidirectionalRange!Range);
        static assert(isOutputRange!(Range, uint));
        static assert(isRandomAccessRange!(Range));

        auto arr = U24A([42u, 36, 100]);
        assert(arr[0] == 42);
        assert(arr[1] == 36);
        arr[0] = 72;
        arr[1] = 0xFE_FEFE;
        assert(arr[0] == 72);
        assert(arr[1] == 0xFE_FEFE);
        assert(arr[2] == 100);
        U24A arr2 = arr;
        assert(arr2[0] == 72);
        arr2[0] = 11;
        // test COW-ness
        assert(arr[0] == 72);
        assert(arr2[0] == 11);
        // set this to about 100M to stress-test COW memory management
        foreach (v; 0 .. 10_000)
            func2(arr);
        assert(equal(arr[], [72, 0xFE_FEFE, 100]));

        auto r2 = U24A(iota(0, 100));
        assert(equal(r2[], iota(0, 100)), text(r2[]));
        copy(iota(10, 170, 2), r2[10 .. 90]);
        assert(equal(r2[], chain(iota(0, 10), iota(10, 170, 2), iota(90, 100)))
               , text(r2[]));
    }}
}

pure @safe unittest// core set primitives test
{
    import std.conv : text;
    alias AllSets = AliasSeq!(InversionList!GcPolicy, InversionList!ReallocPolicy);
    foreach (CodeList; AllSets)
    {
        CodeList a;
        //"plug a hole" test
        a.add(10, 20).add(25, 30).add(15, 27);
        assert(a == CodeList(10, 30), text(a));

        auto x = CodeList.init;
        x.add(10, 20).add(30, 40).add(50, 60);

        a = x;
        a.add(20, 49);//[10, 49) [50, 60)
        assert(a == CodeList(10, 49, 50 ,60));

        a = x;
        a.add(20, 50);
        assert(a == CodeList(10, 60), text(a));

        // simple unions, mostly edge effects
        x = CodeList.init;
        x.add(10, 20).add(40, 60);

        a = x;
        a.add(10, 25); //[10, 25) [40, 60)
        assert(a == CodeList(10, 25, 40, 60));

        a = x;
        a.add(5, 15); //[5, 20) [40, 60)
        assert(a == CodeList(5, 20, 40, 60));

        a = x;
        a.add(0, 10); // [0, 20) [40, 60)
        assert(a == CodeList(0, 20, 40, 60));

        a = x;
        a.add(0, 5); // prepand
        assert(a == CodeList(0, 5, 10, 20, 40, 60), text(a));

        a = x;
        a.add(5, 20);
        assert(a == CodeList(5, 20, 40, 60));

        a = x;
        a.add(3, 37);
        assert(a == CodeList(3, 37, 40, 60));

        a = x;
        a.add(37, 65);
        assert(a == CodeList(10, 20, 37, 65));

        // some tests on helpers for set intersection
        x = CodeList.init.add(10, 20).add(40, 60).add(100, 120);
        a = x;

        auto m = a.skipUpTo(60);
        a.dropUpTo(110, m);
        assert(a == CodeList(10, 20, 40, 60, 110, 120), text(a.data[]));

        a = x;
        a.dropUpTo(100);
        assert(a == CodeList(100, 120), text(a.data[]));

        a = x;
        m = a.skipUpTo(50);
        a.dropUpTo(140, m);
        assert(a == CodeList(10, 20, 40, 50), text(a.data[]));
        a = x;
        a.dropUpTo(60);
        assert(a == CodeList(100, 120), text(a.data[]));
    }
}


//test constructor to work with any order of intervals
pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text, to;
    import std.range : chain, iota;
    import std.typecons : tuple;
    //ensure constructor handles bad ordering and overlap
    auto c1 = CodepointSet('а', 'я'+1, 'А','Я'+1);
    foreach (ch; chain(iota('а', 'я'+1), iota('А','Я'+1)))
        assert(ch in c1, to!string(ch));

    //contiguos
    assert(CodepointSet(1000, 1006, 1006, 1009)
        .byInterval.equal([tuple(1000, 1009)]));
    //contains
    assert(CodepointSet(900, 1200, 1000, 1100)
        .byInterval.equal([tuple(900, 1200)]));
    //intersect left
    assert(CodepointSet(900, 1100, 1000, 1200)
        .byInterval.equal([tuple(900, 1200)]));
    //intersect right
    assert(CodepointSet(1000, 1200, 900, 1100)
        .byInterval.equal([tuple(900, 1200)]));

    //ditto with extra items at end
    assert(CodepointSet(1000, 1200, 900, 1100, 800, 850)
        .byInterval.equal([tuple(800, 850), tuple(900, 1200)]));
    assert(CodepointSet(900, 1100, 1000, 1200, 800, 850)
        .byInterval.equal([tuple(800, 850), tuple(900, 1200)]));

    //"plug a hole" test
    auto c2 = CodepointSet(20, 40,
        60, 80, 100, 140, 150, 200,
        40, 60, 80, 100, 140, 150
    );
    assert(c2.byInterval.equal([tuple(20, 200)]));

    auto c3 = CodepointSet(
        20, 40, 60, 80, 100, 140, 150, 200,
        0, 10, 15, 100, 10, 20, 200, 220);
    assert(c3.byInterval.equal([tuple(0, 140), tuple(150, 220)]));
}


pure @safe unittest
{   // full set operations
    import std.conv : text;
    alias AllSets = AliasSeq!(InversionList!GcPolicy, InversionList!ReallocPolicy);
    foreach (CodeList; AllSets)
    {
        CodeList a, b, c, d;

        //"plug a hole"
        a.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b.add(40, 60).add(80, 100).add(140, 150);
        c = a | b;
        d = b | a;
        assert(c == CodeList(20, 200), text(CodeList.stringof," ", c));
        assert(c == d, text(c," vs ", d));

        b = CodeList.init.add(25, 45).add(65, 85).add(95,110).add(150, 210);
        c = a | b; //[20,45) [60, 85) [95, 140) [150, 210)
        d = b | a;
        assert(c == CodeList(20, 45, 60, 85, 95, 140, 150, 210), text(c));
        assert(c == d, text(c," vs ", d));

        b = CodeList.init.add(10, 20).add(30,100).add(145,200);
        c = a | b;//[10, 140) [145, 200)
        d = b | a;
        assert(c == CodeList(10, 140, 145, 200));
        assert(c == d, text(c," vs ", d));

        b = CodeList.init.add(0, 10).add(15, 100).add(10, 20).add(200, 220);
        c = a | b;//[0, 140) [150, 220)
        d = b | a;
        assert(c == CodeList(0, 140, 150, 220));
        assert(c == d, text(c," vs ", d));


        a = CodeList.init.add(20, 40).add(60, 80);
        b = CodeList.init.add(25, 35).add(65, 75);
        c = a & b;
        d = b & a;
        assert(c == CodeList(25, 35, 65, 75), text(c));
        assert(c == d, text(c," vs ", d));

        a = CodeList.init.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b = CodeList.init.add(25, 35).add(65, 75).add(110, 130).add(160, 180);
        c = a & b;
        d = b & a;
        assert(c == CodeList(25, 35, 65, 75, 110, 130, 160, 180), text(c));
        assert(c == d, text(c," vs ", d));

        a = CodeList.init.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b = CodeList.init.add(10, 30).add(60, 120).add(135, 160);
        c = a & b;//[20, 30)[60, 80) [100, 120) [135, 140) [150, 160)
        d = b & a;

        assert(c == CodeList(20, 30, 60, 80, 100, 120, 135, 140, 150, 160),text(c));
        assert(c == d, text(c, " vs ",d));
        assert((c & a) == c);
        assert((d & b) == d);
        assert((c & d) == d);

        b = CodeList.init.add(40, 60).add(80, 100).add(140, 200);
        c = a & b;
        d = b & a;
        assert(c == CodeList(150, 200), text(c));
        assert(c == d, text(c, " vs ",d));
        assert((c & a) == c);
        assert((d & b) == d);
        assert((c & d) == d);

        assert((a & a) == a);
        assert((b & b) == b);

        a = CodeList.init.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b = CodeList.init.add(30, 60).add(75, 120).add(190, 300);
        c = a - b;// [30, 40) [60, 75) [120, 140) [150, 190)
        d = b - a;// [40, 60) [80, 100) [200, 300)
        assert(c == CodeList(20, 30, 60, 75, 120, 140, 150, 190), text(c));
        assert(d == CodeList(40, 60, 80, 100, 200, 300), text(d));
        assert(c - d == c, text(c-d, " vs ", c));
        assert(d - c == d, text(d-c, " vs ", d));
        assert(c - c == CodeList.init);
        assert(d - d == CodeList.init);

        a = CodeList.init.add(20, 40).add( 60, 80).add(100, 140).add(150,            200);
        b = CodeList.init.add(10,  50).add(60,                           160).add(190, 300);
        c = a - b;// [160, 190)
        d = b - a;// [10, 20) [40, 50) [80, 100) [140, 150) [200, 300)
        assert(c == CodeList(160, 190), text(c));
        assert(d == CodeList(10, 20, 40, 50, 80, 100, 140, 150, 200, 300), text(d));
        assert(c - d == c, text(c-d, " vs ", c));
        assert(d - c == d, text(d-c, " vs ", d));
        assert(c - c == CodeList.init);
        assert(d - d == CodeList.init);

        a = CodeList.init.add(20,    40).add(60, 80).add(100,      140).add(150,  200);
        b = CodeList.init.add(10, 30).add(45,         100).add(130,             190);
        c = a ~ b; // [10, 20) [30, 40) [45, 60) [80, 130) [140, 150) [190, 200)
        d = b ~ a;
        assert(c == CodeList(10, 20, 30, 40, 45, 60, 80, 130, 140, 150, 190, 200),
               text(c));
        assert(c == d, text(c, " vs ", d));
    }
}

}

pure @safe unittest// vs single dchar
{
    import std.conv : text;
    CodepointSet a = CodepointSet(10, 100, 120, 200);
    assert(a - 'A' == CodepointSet(10, 65, 66, 100, 120, 200), text(a - 'A'));
    assert((a & 'B') == CodepointSet(66, 67));
}

pure @safe unittest// iteration & opIndex
{
    import std.algorithm.comparison : equal;
    import std.conv : text;
    import std.typecons : tuple, Tuple;

    static foreach (CodeList; AliasSeq!(InversionList!(ReallocPolicy)))
    {{
        auto arr = "ABCDEFGHIJKLMabcdefghijklm"d;
        auto a = CodeList('A','N','a', 'n');
        assert(equal(a.byInterval,
                [tuple(cast(uint)'A', cast(uint)'N'), tuple(cast(uint)'a', cast(uint)'n')]
            ), text(a.byInterval));

        // same @@@BUG as in https://issues.dlang.org/show_bug.cgi?id=8949 ?
        version (bug8949)
        {
            import std.range : retro;
            assert(equal(retro(a.byInterval),
                [tuple(cast(uint)'a', cast(uint)'n'), tuple(cast(uint)'A', cast(uint)'N')]
            ), text(retro(a.byInterval)));
        }
        auto achr = a.byCodepoint;
        assert(equal(achr, arr), text(a.byCodepoint));
        foreach (ch; a.byCodepoint)
            assert(a[ch]);
        auto x = CodeList(100, 500, 600, 900, 1200, 1500);
        assert(equal(x.byInterval, [ tuple(100, 500), tuple(600, 900), tuple(1200, 1500)]), text(x.byInterval));
        foreach (ch; x.byCodepoint)
            assert(x[ch]);
        static if (is(CodeList == CodepointSet))
        {
            auto y = CodeList(x.byInterval);
            assert(equal(x.byInterval, y.byInterval));
        }
        assert(equal(CodepointSet.init.byInterval, cast(Tuple!(uint, uint)[])[]));
        assert(equal(CodepointSet.init.byCodepoint, cast(dchar[])[]));
    }}
}

//============================================================================
// Generic Trie template and various ways to build it
//============================================================================

// debug helper to get a shortened array dump
auto arrayRepr(T)(T x)
{
    import std.conv : text;
    if (x.length > 32)
    {
        return text(x[0 .. 16],"~...~", x[x.length-16 .. x.length]);
    }
    else
        return text(x);
}

/**
    Maps `Key` to a suitable integer index within the range of `size_t`.
    The mapping is constructed by applying predicates from `Prefix` left to right
    and concatenating the resulting bits.

    The first (leftmost) predicate defines the most significant bits of
    the resulting index.
 */
template mapTrieIndex(Prefix...)
{
    size_t mapTrieIndex(Key)(Key key)
    if (isValidPrefixForTrie!(Key, Prefix))
    {
        alias p = Prefix;
        size_t idx;
        foreach (i, v; p[0..$-1])
        {
            idx |= p[i](key);
            idx <<= p[i+1].bitSize;
        }
        idx |= p[$-1](key);
        return idx;
    }
}

/*
    `TrieBuilder` is a type used for incremental construction
    of $(LREF Trie)s.

    See $(LREF buildTrie) for generic helpers built on top of it.
*/
@trusted private struct TrieBuilder(Value, Key, Args...)
if (isBitPackableType!Value && isValidArgsForTrie!(Key, Args))
{
    import std.exception : enforce;

private:
    // last index is not stored in table, it is used as an offset to values in a block.
    static if (is(Value == bool))// always pack bool
        alias V = BitPacked!(Value, 1);
    else
        alias V = Value;
    static auto deduceMaxIndex(Preds...)()
    {
        size_t idx = 1;
        foreach (v; Preds)
            idx *= 2^^v.bitSize;
        return idx;
    }

    static if (is(typeof(Args[0]) : Key)) // Args start with upper bound on Key
    {
        alias Prefix = Args[1..$];
        enum lastPageSize = 2^^Prefix[$-1].bitSize;
        enum translatedMaxIndex = mapTrieIndex!(Prefix)(Args[0]);
        enum roughedMaxIndex =
            (translatedMaxIndex + lastPageSize-1)/lastPageSize*lastPageSize;
        // check warp around - if wrapped, use the default deduction rule
        enum maxIndex = roughedMaxIndex < translatedMaxIndex ?
            deduceMaxIndex!(Prefix)() : roughedMaxIndex;
    }
    else
    {
        alias Prefix = Args;
        enum maxIndex = deduceMaxIndex!(Prefix)();
    }

    alias getIndex = mapTrieIndex!(Prefix);

    enum lastLevel = Prefix.length-1;
    struct ConstructState
    {
        size_t idx_zeros, idx_ones;
    }
    // iteration over levels of Trie, each indexes its own level and thus a shortened domain
    size_t[Prefix.length] indices;
    // default filler value to use
    Value defValue;
    // this is a full-width index of next item
    size_t curIndex;
    // all-zeros page index, all-ones page index (+ indicator if there is such a page)
    ConstructState[Prefix.length] state;
    // the table being constructed
    MultiArray!(idxTypes!(Key, fullBitSize!(Prefix), Prefix[0..$]), V) table;

    @disable this();

    //shortcut for index variable at level 'level'
    @property ref idx(size_t level)(){ return indices[level]; }

    // this function assumes no holes in the input so
    // indices are going one by one
    void addValue(size_t level, T)(T val, size_t numVals)
    {
        alias j = idx!level;
        enum pageSize = 1 << Prefix[level].bitSize;
        if (numVals == 0)
            return;
        auto ptr = table.slice!(level);
        if (numVals == 1)
        {
            static if (level == Prefix.length-1)
                ptr[j] = val;
            else
            {// can incur narrowing conversion
                assert(j < ptr.length);
                ptr[j] = force!(typeof(ptr[j]))(val);
            }
            j++;
            if (j % pageSize == 0)
                spillToNextPage!level(ptr);
            return;
        }
        // longer row of values
        // get to the next page boundary
        immutable nextPB = (j + pageSize) & ~(pageSize-1);
        immutable n =  nextPB - j;// can fill right in this page
        if (numVals < n) //fits in current page
        {
            ptr[j .. j+numVals]  = val;
            j += numVals;
            return;
        }
        static if (level != 0)//on the first level it always fits
        {
            numVals -= n;
            //write till the end of current page
            ptr[j .. j+n]  = val;
            j += n;
            //spill to the next page
            spillToNextPage!level(ptr);
            // page at once loop
            if (state[level].idx_zeros != size_t.max && val == T.init)
            {
                alias NextIdx = typeof(table.slice!(level-1)[0]);
                addValue!(level-1)(force!NextIdx(state[level].idx_zeros),
                    numVals/pageSize);
                ptr = table.slice!level; //table structure might have changed
                numVals %= pageSize;
            }
            else
            {
                while (numVals >= pageSize)
                {
                    numVals -= pageSize;
                    ptr[j .. j+pageSize]  = val;
                    j += pageSize;
                    spillToNextPage!level(ptr);
                }
            }
            if (numVals)
            {
                // the leftovers, an incomplete page
                ptr[j .. j+numVals]  = val;
                j += numVals;
            }
        }
    }

    void spillToNextPage(size_t level, Slice)(ref Slice ptr)
    {
        // last level (i.e. topmost) has 1 "page"
        // thus it need not to add a new page on upper level
        static if (level != 0)
            spillToNextPageImpl!(level)(ptr);
    }

    // this can re-use the current page if duplicate or allocate a new one
    // it also makes sure that previous levels point to the correct page in this level
    void spillToNextPageImpl(size_t level, Slice)(ref Slice ptr)
    {
        alias NextIdx = typeof(table.slice!(level-1)[0]);
        NextIdx next_lvl_index;
        enum pageSize = 1 << Prefix[level].bitSize;
        assert(idx!level % pageSize == 0);
        immutable last = idx!level-pageSize;
        const slice = ptr[idx!level - pageSize .. idx!level];
        size_t j;
        for (j=0; j<last; j+=pageSize)
        {
            if (ptr[j .. j+pageSize] == slice)
            {
                // get index to it, reuse ptr space for the next block
                next_lvl_index = force!NextIdx(j/pageSize);
                version (none)
                {
                import std.stdio : writefln, writeln;
                writefln("LEVEL(%s) page mapped idx: %s: 0..%s  ---> [%s..%s]"
                        ,level
                        ,indices[level-1], pageSize, j, j+pageSize);
                writeln("LEVEL(", level
                        , ") mapped page is: ", slice, ": ", arrayRepr(ptr[j .. j+pageSize]));
                writeln("LEVEL(", level
                        , ") src page is :", ptr, ": ", arrayRepr(slice[0 .. pageSize]));
                }
                idx!level -= pageSize; // reuse this page, it is duplicate
                break;
            }
        }
        if (j == last)
        {
    L_allocate_page:
            next_lvl_index = force!NextIdx(idx!level/pageSize - 1);
            if (state[level].idx_zeros == size_t.max && ptr.zeros(j, j+pageSize))
            {
                state[level].idx_zeros = next_lvl_index;
            }
            // allocate next page
            version (none)
            {
            import std.stdio : writefln;
            writefln("LEVEL(%s) page allocated: %s"
                     , level, arrayRepr(slice[0 .. pageSize]));
            writefln("LEVEL(%s) index: %s ; page at this index %s"
                     , level
                     , next_lvl_index
                     , arrayRepr(
                         table.slice!(level)
                          [pageSize*next_lvl_index..(next_lvl_index+1)*pageSize]
                        ));
            }
            table.length!level = table.length!level + pageSize;
        }
    L_know_index:
        // for the previous level, values are indices to the pages in the current level
        addValue!(level-1)(next_lvl_index, 1);
        ptr = table.slice!level; //re-load the slice after moves
    }

    // idx - full-width index to fill with v (full-width index != key)
    // fills everything in the range of [curIndex, idx) with filler
    void putAt(size_t idx, Value v)
    {
        assert(idx >= curIndex);
        immutable numFillers = idx - curIndex;
        addValue!lastLevel(defValue, numFillers);
        addValue!lastLevel(v, 1);
        curIndex = idx + 1;
    }

    // ditto, but sets the range of [idxA, idxB) to v
    void putRangeAt(size_t idxA, size_t idxB, Value v)
    {
        assert(idxA >= curIndex);
        assert(idxB >= idxA);
        size_t numFillers = idxA - curIndex;
        addValue!lastLevel(defValue, numFillers);
        addValue!lastLevel(v, idxB - idxA);
        curIndex = idxB; // open-right
    }

    enum errMsg = "non-monotonic prefix function(s), an unsorted range or "~
        "duplicate key->value mapping";

public:
    /**
        Construct a builder, where `filler` is a value
        to indicate empty slots (or "not found" condition).
    */
    this(Value filler)
    {
        curIndex = 0;
        defValue = filler;
        // zeros-page index, ones-page index
        foreach (ref v; state)
            v = ConstructState(size_t.max, size_t.max);
        table = typeof(table)(indices);
        // one page per level is a bootstrap minimum
        foreach (i, Pred; Prefix)
            table.length!i = (1 << Pred.bitSize);
    }

    /**
        Put a value `v` into interval as
        mapped by keys from `a` to `b`.
        All slots prior to `a` are filled with
        the default filler.
    */
    void putRange(Key a, Key b, Value v)
    {
        auto idxA = getIndex(a), idxB = getIndex(b);
        // indexes of key should always grow
        enforce(idxB >= idxA && idxA >= curIndex, errMsg);
        putRangeAt(idxA, idxB, v);
    }

    /**
        Put a value `v` into slot mapped by `key`.
        All slots prior to `key` are filled with the
        default filler.
    */
    void putValue(Key key, Value v)
    {
        auto idx = getIndex(key);
        enforce(idx >= curIndex, errMsg);
        putAt(idx, v);
    }

    /// Finishes construction of Trie, yielding an immutable Trie instance.
    auto build()
    {
        static if (maxIndex != 0) // doesn't cover full range of size_t
        {
            assert(curIndex <= maxIndex);
            addValue!lastLevel(defValue, maxIndex - curIndex);
        }
        else
        {
            if (curIndex != 0 // couldn't wrap around
                || (Prefix.length != 1 && indices[lastLevel] == 0)) // can be just empty
            {
                addValue!lastLevel(defValue, size_t.max - curIndex);
                addValue!lastLevel(defValue, 1);
            }
            // else curIndex already completed the full range of size_t by wrapping around
        }
        return Trie!(V, Key, maxIndex, Prefix)(table);
    }
}

/**
    $(P A generic Trie data-structure for a fixed number of stages.
    The design goal is optimal speed with smallest footprint size.
    )
    $(P It's intentionally read-only and doesn't provide constructors.
     To construct one use a special builder,
     see $(LREF TrieBuilder) and $(LREF buildTrie).
    )

*/
@trusted private struct Trie(Value, Key, Args...)
if (isValidPrefixForTrie!(Key, Args)
    || (isValidPrefixForTrie!(Key, Args[1..$])
    && is(typeof(Args[0]) : size_t)))
{
    import std.range.primitives : isOutputRange;
    static if (is(typeof(Args[0]) : size_t))
    {
        private enum maxIndex = Args[0];
        private enum hasBoundsCheck = true;
        private alias Prefix = Args[1..$];
    }
    else
    {
        private enum hasBoundsCheck = false;
        private alias Prefix = Args;
    }

    private this()(typeof(_table) table)
    {
        _table = table;
    }

    // only for constant Tries constructed from precompiled tables
    private this()(const(size_t)[] offsets, const(size_t)[] sizes,
        const(size_t)[] data) const
    {
        _table = typeof(_table)(offsets, sizes, data);
    }

    /**
        $(P Lookup the `key` in this `Trie`. )

        $(P The lookup always succeeds if key fits the domain
        provided during construction. The whole domain defined
        is covered so instead of not found condition
        the sentinel (filler) value could be used. )

        $(P See $(LREF buildTrie), $(LREF TrieBuilder) for how to
        define a domain of `Trie` keys and the sentinel value. )

        Note:
        Domain range-checking is only enabled in debug builds
        and results in assertion failure.
    */
    TypeOfBitPacked!Value opIndex()(Key key) const
    {
        static if (hasBoundsCheck)
            assert(mapTrieIndex!Prefix(key) < maxIndex);
        size_t idx;
        alias p = Prefix;
        idx = cast(size_t) p[0](key);
        foreach (i, v; p[0..$-1])
            idx = cast(size_t)((_table.ptr!i[idx]<<p[i+1].bitSize) + p[i+1](key));
        return _table.ptr!(p.length-1)[idx];
    }

    ///
    @property size_t bytes(size_t n=size_t.max)() const
    {
        return _table.bytes!n;
    }

    ///
    @property size_t pages(size_t n)() const
    {
        return (bytes!n+2^^(Prefix[n].bitSize-1))
                /2^^Prefix[n].bitSize;
    }

    ///
    void store(OutRange)(scope OutRange sink) const
    if (isOutputRange!(OutRange, char))
    {
        _table.store(sink);
    }

private:
    MultiArray!(idxTypes!(Key, fullBitSize!(Prefix), Prefix[0..$]), Value) _table;
}

// create a tuple of 'sliceBits' that slice the 'top' of bits into pieces of sizes 'sizes'
// left-to-right, the most significant bits first
template GetBitSlicing(size_t top, sizes...)
{
    static if (sizes.length > 0)
        alias GetBitSlicing =
            AliasSeq!(sliceBits!(top - sizes[0], top),
                      GetBitSlicing!(top - sizes[0], sizes[1..$]));
    else
        alias GetBitSlicing = AliasSeq!();
}

template callableWith(T)
{
    template callableWith(alias Pred)
    {
        static if (!is(typeof(Pred(T.init))))
            enum callableWith = false;
        else
        {
            alias Result = typeof(Pred(T.init));
            enum callableWith = isBitPackableType!(TypeOfBitPacked!(Result));
        }
    }
}

/*
    Check if `Prefix` is a valid set of predicates
    for `Trie` template having `Key` as the type of keys.
    This requires all predicates to be callable, take
    single argument of type `Key` and return unsigned value.
*/
template isValidPrefixForTrie(Key, Prefix...)
{
    import std.meta : allSatisfy;
    enum isValidPrefixForTrie = allSatisfy!(callableWith!Key, Prefix); // TODO: tighten the screws
}

/*
    Check if `Args` is a set of maximum key value followed by valid predicates
    for `Trie` template having `Key` as the type of keys.
*/
template isValidArgsForTrie(Key, Args...)
{
    static if (Args.length > 1)
    {
        enum isValidArgsForTrie = isValidPrefixForTrie!(Key, Args)
            || (isValidPrefixForTrie!(Key, Args[1..$]) && is(typeof(Args[0]) : Key));
    }
    else
        enum isValidArgsForTrie = isValidPrefixForTrie!Args;
}

@property size_t sumOfIntegerTuple(ints...)()
{
    size_t count=0;
    foreach (v; ints)
        count += v;
    return count;
}

/**
    A shorthand for creating a custom multi-level fixed Trie
    from a `CodepointSet`. `sizes` are numbers of bits per level,
    with the most significant bits used first.

    Note: The sum of `sizes` must be equal 21.

    See_Also: $(LREF toTrie), which is even simpler.

    Example:
    ---
    {
        import std.stdio;
        auto set = unicode("Number");
        auto trie = codepointSetTrie!(8, 5, 8)(set);
        writeln("Input code points to test:");
        foreach (line; stdin.byLine)
        {
            int count=0;
            foreach (dchar ch; line)
                if (trie[ch])// is number
                    count++;
            writefln("Contains %d number code points.", count);
        }
    }
    ---
*/
public template codepointSetTrie(sizes...)
if (sumOfIntegerTuple!sizes == 21)
{
    auto codepointSetTrie(Set)(Set set)
    if (isCodepointSet!Set)
    {
        auto builder = TrieBuilder!(bool, dchar, lastDchar+1, GetBitSlicing!(21, sizes))(false);
        foreach (ival; set.byInterval)
            builder.putRange(ival[0], ival[1], true);
        return builder.build();
    }
}

/// Type of Trie generated by codepointSetTrie function.
public template CodepointSetTrie(sizes...)
if (sumOfIntegerTuple!sizes == 21)
{
    alias Prefix = GetBitSlicing!(21, sizes);
    alias CodepointSetTrie = typeof(TrieBuilder!(bool, dchar, lastDchar+1, Prefix)(false).build());
}

/**
    A slightly more general tool for building fixed `Trie`
    for the Unicode data.

    Specifically unlike `codepointSetTrie` it's allows creating mappings
    of `dchar` to an arbitrary type `T`.

    Note: Overload taking `CodepointSet`s will naturally convert
    only to bool mapping `Trie`s.

    CodepointTrie is the type of Trie as generated by codepointTrie function.
*/
public template codepointTrie(T, sizes...)
if (sumOfIntegerTuple!sizes == 21)
{
    alias Prefix = GetBitSlicing!(21, sizes);

    static if (is(TypeOfBitPacked!T == bool))
    {
        auto codepointTrie(Set)(const scope Set set)
        if (isCodepointSet!Set)
        {
            return codepointSetTrie(set);
        }
    }

    ///
    auto codepointTrie()(T[dchar] map, T defValue=T.init)
    {
        return buildTrie!(T, dchar, Prefix)(map, defValue);
    }

    // unsorted range of pairs
    ///
    auto codepointTrie(R)(R range, T defValue=T.init)
    if (isInputRange!R
        && is(typeof(ElementType!R.init[0]) : T)
        && is(typeof(ElementType!R.init[1]) : dchar))
    {
        // build from unsorted array of pairs
        // TODO: expose index sorting functions for Trie
        return buildTrie!(T, dchar, Prefix)(range, defValue, true);
    }
}

@system pure unittest
{
    import std.algorithm.comparison : max;
    import std.algorithm.searching : count;

    // pick characters from the Greek script
    auto set = unicode.Greek;

    // a user-defined property (or an expensive function)
    // that we want to look up
    static uint luckFactor(dchar ch)
    {
        // here we consider a character lucky
        // if its code point has a lot of identical hex-digits
        // e.g. arabic letter DDAL (\u0688) has a "luck factor" of 2
        ubyte[6] nibbles; // 6 4-bit chunks of code point
        uint value = ch;
        foreach (i; 0 .. 6)
        {
            nibbles[i] = value & 0xF;
            value >>= 4;
        }
        uint luck;
        foreach (n; nibbles)
            luck = cast(uint) max(luck, count(nibbles[], n));
        return luck;
    }

    // only unsigned built-ins are supported at the moment
    alias LuckFactor = BitPacked!(uint, 3);

    // create a temporary associative array (AA)
    LuckFactor[dchar] map;
    foreach (ch; set.byCodepoint)
        map[ch] = LuckFactor(luckFactor(ch));

    // bits per stage are chosen randomly, fell free to optimize
    auto trie = codepointTrie!(LuckFactor, 8, 5, 8)(map);

    // from now on the AA is not needed
    foreach (ch; set.byCodepoint)
        assert(trie[ch] == luckFactor(ch)); // verify
    // CJK is not Greek, thus it has the default value
    assert(trie['\u4444'] == 0);
    // and here is a couple of quite lucky Greek characters:
    // Greek small letter epsilon with dasia
    assert(trie['\u1F11'] == 3);
    // Ancient Greek metretes sign
    assert(trie['\U00010181'] == 3);

}

/// ditto
public template CodepointTrie(T, sizes...)
if (sumOfIntegerTuple!sizes == 21)
{
    alias Prefix = GetBitSlicing!(21, sizes);
    alias CodepointTrie = typeof(TrieBuilder!(T, dchar, lastDchar+1, Prefix)(T.init).build());
}

package(std) template cmpK0(alias Pred)
{
    import std.typecons : Tuple;
    static bool cmpK0(Value, Key)
        (Tuple!(Value, Key) a, Tuple!(Value, Key) b)
    {
        return Pred(a[1]) < Pred(b[1]);
    }
}

/**
    The most general utility for construction of `Trie`s
    short of using `TrieBuilder` directly.

    Provides a number of convenience overloads.
    `Args` is tuple of maximum key value followed by
    predicates to construct index from key.

    Alternatively if the first argument is not a value convertible to `Key`
    then the whole tuple of `Args` is treated as predicates
    and the maximum Key is deduced from predicates.
*/
private template buildTrie(Value, Key, Args...)
if (isValidArgsForTrie!(Key, Args))
{
    static if (is(typeof(Args[0]) : Key)) // prefix starts with upper bound on Key
    {
        alias Prefix = Args[1..$];
    }
    else
        alias Prefix = Args;

    alias getIndex = mapTrieIndex!(Prefix);

    // for multi-sort
    template GetComparators(size_t n)
    {
        static if (n > 0)
            alias GetComparators =
                AliasSeq!(GetComparators!(n-1), cmpK0!(Prefix[n-1]));
        else
            alias GetComparators = AliasSeq!();
    }

    /*
        Build `Trie` from a range of a Key-Value pairs,
        assuming it is sorted by Key as defined by the following lambda:
        ------
        (a, b) => mapTrieIndex!(Prefix)(a) < mapTrieIndex!(Prefix)(b)
        ------
        Exception is thrown if it's detected that the above order doesn't hold.

        In other words $(LREF mapTrieIndex) should be a
        monotonically increasing function that maps `Key` to an integer.

        See_Also: $(REF sort, std,_algorithm),
        $(REF SortedRange, std,range),
        $(REF setUnion, std,_algorithm).
    */
    auto buildTrie(Range)(Range range, Value filler=Value.init)
    if (isInputRange!Range && is(typeof(Range.init.front[0]) : Value)
        && is(typeof(Range.init.front[1]) : Key))
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach (v; range)
            builder.putValue(v[1], v[0]);
        return builder.build();
    }

    /*
        If `Value` is bool (or BitPacked!(bool, x)) then it's possible
        to build `Trie` from a range of open-right intervals of `Key`s.
        The requirement  on the ordering of keys (and the behavior on the
        violation of it) is the same as for Key-Value range overload.

        Intervals denote ranges of !`filler` i.e. the opposite of filler.
        If no filler provided keys inside of the intervals map to true,
        and `filler` is false.
    */
    auto buildTrie(Range)(Range range, Value filler=Value.init)
    if (is(TypeOfBitPacked!Value ==  bool)
        && isInputRange!Range && is(typeof(Range.init.front[0]) : Key)
        && is(typeof(Range.init.front[1]) : Key))
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach (ival; range)
            builder.putRange(ival[0], ival[1], !filler);
        return builder.build();
    }

    auto buildTrie(Range)(Range range, Value filler, bool unsorted)
    if (isInputRange!Range
        && is(typeof(Range.init.front[0]) : Value)
        && is(typeof(Range.init.front[1]) : Key))
    {
        import std.algorithm.sorting : multiSort;
        alias Comps = GetComparators!(Prefix.length);
        if (unsorted)
            multiSort!(Comps)(range);
        return buildTrie(range, filler);
    }

    /*
        If `Value` is bool (or BitPacked!(bool, x)) then it's possible
        to build `Trie` simply from an input range of `Key`s.
        The requirement  on the ordering of keys (and the behavior on the
        violation of it) is the same as for Key-Value range overload.

        Keys found in range denote !`filler` i.e. the opposite of filler.
        If no filler provided keys map to true, and `filler` is false.
    */
    auto buildTrie(Range)(Range range, Value filler=Value.init)
    if (is(TypeOfBitPacked!Value ==  bool)
        && isInputRange!Range && is(typeof(Range.init.front) : Key))
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach (v; range)
            builder.putValue(v, !filler);
        return builder.build();
    }

    /*
        If `Key` is unsigned integer `Trie` could be constructed from array
        of values where array index serves as key.
    */
    auto buildTrie()(Value[] array, Value filler=Value.init)
    if (isUnsigned!Key)
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach (idx, v; array)
            builder.putValue(idx, v);
        return builder.build();
    }

    /*
        Builds `Trie` from associative array.
    */
    auto buildTrie(Key, Value)(Value[Key] map, Value filler=Value.init)
    {
        import std.array : array;
        import std.range : zip;
        auto range = array(zip(map.values, map.keys));
        return buildTrie(range, filler, true); // sort it
    }
}

// helper in place of assumeSize to
//reduce mangled name & help DMD inline Trie functors
struct clamp(size_t bits)
{
    static size_t opCall(T)(T arg){ return arg; }
    enum bitSize = bits;
}

struct clampIdx(size_t idx, size_t bits)
{
    static size_t opCall(T)(T arg){ return arg[idx]; }
    enum bitSize = bits;
}

/**
    Conceptual type that outlines the common properties of all UTF Matchers.

    Note: For illustration purposes only, every method
    call results in assertion failure.
    Use $(LREF utfMatcher) to obtain a concrete matcher
    for UTF-8 or UTF-16 encodings.
*/
public struct MatcherConcept
{
    /**
        $(P Perform a semantic equivalent 2 operations:
        decoding a $(CODEPOINT) at front of `inp` and testing if
        it belongs to the set of $(CODEPOINTS) of this matcher. )

        $(P The effect on `inp` depends on the kind of function called:)

        $(P Match. If the codepoint is found in the set then range `inp`
        is advanced by its size in $(S_LINK Code unit, code units),
        otherwise the range is not modifed.)

        $(P Skip. The range is always advanced by the size
        of the tested $(CODEPOINT) regardless of the result of test.)

        $(P Test. The range is left unaffected regardless
        of the result of test.)
    */
    public bool match(Range)(ref Range inp)
    if (isRandomAccessRange!Range && is(ElementType!Range : char))
    {
       assert(false);
    }

    ///ditto
    public bool skip(Range)(ref Range inp)
    if (isRandomAccessRange!Range && is(ElementType!Range : char))
    {
        assert(false);
    }

    ///ditto
    public bool test(Range)(ref Range inp)
    if (isRandomAccessRange!Range && is(ElementType!Range : char))
    {
        assert(false);
    }
    ///
    pure @safe unittest
    {
        string truth = "2² = 4";
        auto m = utfMatcher!char(unicode.Number);
        assert(m.match(truth)); // '2' is a number all right
        assert(truth == "² = 4"); // skips on match
        assert(m.match(truth)); // so is the superscript '2'
        assert(!m.match(truth)); // space is not a number
        assert(truth == " = 4"); // unaffected on no match
        assert(!m.skip(truth)); // same test ...
        assert(truth == "= 4"); // but skips a codepoint regardless
        assert(!m.test(truth)); // '=' is not a number
        assert(truth == "= 4"); // test never affects argument
    }

    /**
        Advanced feature - provide direct access to a subset of matcher based a
        set of known encoding lengths. Lengths are provided in
        $(S_LINK Code unit, code units). The sub-matcher then may do less
        operations per any `test`/`match`.

        Use with care as the sub-matcher won't match
        any $(CODEPOINTS) that have encoded length that doesn't belong
        to the selected set of lengths. Also the sub-matcher object references
        the parent matcher and must not be used past the liftetime
        of the latter.

        Another caveat of using sub-matcher is that skip is not available
        preciesly because sub-matcher doesn't detect all lengths.
    */
    @property auto subMatcher(Lengths...)()
    {
        assert(0);
        return this;
    }

    pure @safe unittest
    {
        auto m = utfMatcher!char(unicode.Number);
        string square = "2²";
        // about sub-matchers
        assert(!m.subMatcher!(2,3,4).test(square)); // ASCII no covered
        assert(m.subMatcher!1.match(square)); // ASCII-only, works
        assert(!m.subMatcher!1.test(square)); // unicode '²'
        assert(m.subMatcher!(2,3,4).match(square));  //
        assert(square == "");
        wstring wsquare = "2²";
        auto m16 = utfMatcher!wchar(unicode.Number);
        // may keep ref, but the orignal (m16) must be kept alive
        auto bmp = m16.subMatcher!1;
        assert(bmp.match(wsquare)); // Okay, in basic multilingual plan
        assert(bmp.match(wsquare)); // And '²' too
    }
}

/**
    Test if `M` is an UTF Matcher for ranges of `Char`.
*/
public enum isUtfMatcher(M, C) = __traits(compiles, (){
    C[] s;
    auto d = s.decoder;
    M m;
    assert(is(typeof(m.match(d)) == bool));
    assert(is(typeof(m.test(d)) == bool));
    static if (is(typeof(m.skip(d))))
    {
        assert(is(typeof(m.skip(d)) == bool));
        assert(is(typeof(m.skip(s)) == bool));
    }
    assert(is(typeof(m.match(s)) == bool));
    assert(is(typeof(m.test(s)) == bool));
});

pure @safe unittest
{
    alias CharMatcher = typeof(utfMatcher!char(CodepointSet.init));
    alias WcharMatcher = typeof(utfMatcher!wchar(CodepointSet.init));
    static assert(isUtfMatcher!(CharMatcher, char));
    static assert(isUtfMatcher!(CharMatcher, immutable(char)));
    static assert(isUtfMatcher!(WcharMatcher, wchar));
    static assert(isUtfMatcher!(WcharMatcher, immutable(wchar)));
}

enum Mode {
    alwaysSkip,
    neverSkip,
    skipOnMatch
}

mixin template ForwardStrings()
{
    private bool fwdStr(string fn, C)(ref C[] str) const @trusted
    {
        import std.utf : byCodeUnit;
        alias type = typeof(byCodeUnit(str));
        return mixin(fn~"(*cast(type*)&str)");
    }
}

template Utf8Matcher()
{
    enum validSize(int sz) = sz >= 1 && sz <= 4;

    void badEncoding() pure @safe
    {
        import std.utf : UTFException;
        throw new UTFException("Invalid UTF-8 sequence");
    }

    //for 1-stage ASCII
    alias AsciiSpec = AliasSeq!(bool, char, clamp!7);
    //for 2-stage lookup of 2 byte UTF-8 sequences
    alias Utf8Spec2 = AliasSeq!(bool, char[2],
        clampIdx!(0, 5), clampIdx!(1, 6));
    //ditto for 3 byte
    alias Utf8Spec3 = AliasSeq!(bool, char[3],
        clampIdx!(0, 4),
        clampIdx!(1, 6),
        clampIdx!(2, 6)
    );
    //ditto for 4 byte
    alias Utf8Spec4 = AliasSeq!(bool, char[4],
        clampIdx!(0, 3), clampIdx!(1, 6),
        clampIdx!(2, 6), clampIdx!(3, 6)
    );
    alias Tables = AliasSeq!(
        typeof(TrieBuilder!(AsciiSpec)(false).build()),
        typeof(TrieBuilder!(Utf8Spec2)(false).build()),
        typeof(TrieBuilder!(Utf8Spec3)(false).build()),
        typeof(TrieBuilder!(Utf8Spec4)(false).build())
    );
    alias Table(int size) = Tables[size-1];

    enum leadMask(size_t size) = (cast(size_t) 1<<(7 - size))-1;
    enum encMask(size_t size) = ((1 << size)-1)<<(8-size);

    char truncate()(char ch) pure @safe
    {
        ch -= 0x80;
        if (ch < 0x40)
        {
            return ch;
        }
        else
        {
            badEncoding();
            return cast(char) 0;
        }
    }

    static auto encode(size_t sz)(dchar ch)
    if (sz > 1)
    {
        import std.utf : encodeUTF = encode;
        char[4] buf;
        encodeUTF(buf, ch);
        char[sz] ret;
        buf[0] &= leadMask!sz;
        foreach (n; 1 .. sz)
            buf[n] = buf[n] & 0x3f; //keep 6 lower bits
        ret[] = buf[0 .. sz];
        return ret;
    }

    auto build(Set)(Set set)
    {
        import std.algorithm.iteration : map;
        auto ascii = set & unicode.ASCII;
        auto utf8_2 = set & CodepointSet(0x80, 0x800);
        auto utf8_3 = set & CodepointSet(0x800, 0x1_0000);
        auto utf8_4 = set & CodepointSet(0x1_0000, lastDchar+1);
        auto asciiT = ascii.byCodepoint.map!(x=>cast(char) x).buildTrie!(AsciiSpec);
        auto utf8_2T = utf8_2.byCodepoint.map!(x=>encode!2(x)).buildTrie!(Utf8Spec2);
        auto utf8_3T = utf8_3.byCodepoint.map!(x=>encode!3(x)).buildTrie!(Utf8Spec3);
        auto utf8_4T = utf8_4.byCodepoint.map!(x=>encode!4(x)).buildTrie!(Utf8Spec4);
        alias Ret = Impl!(1,2,3,4);
        return Ret(asciiT, utf8_2T, utf8_3T, utf8_4T);
    }

    // Bootstrap UTF-8 static matcher interface
    // from 3 primitives: tab!(size), lookup and Sizes
    mixin template DefMatcher()
    {
        import std.format : format;
        import std.meta : Erase, staticIndexOf;
        enum hasASCII = staticIndexOf!(1, Sizes) >= 0;
        alias UniSizes = Erase!(1, Sizes);

        //generate dispatch code sequence for unicode parts
        static auto genDispatch()
        {
            string code;
            foreach (size; UniSizes)
                code ~= format(q{
                    if ((ch & ~leadMask!%d) == encMask!(%d))
                        return lookup!(%d, mode)(inp);
                    else
                }, size, size, size);
            static if (Sizes.length == 4) //covers all code unit cases
                code ~= "{ badEncoding(); return false; }";
            else
                code ~= "return false;"; //may be just fine but not covered
            return code;
        }
        enum dispatch = genDispatch();

        public bool match(Range)(ref Range inp) const
        if (isRandomAccessRange!Range && is(ElementType!Range : char) &&
            !isDynamicArray!Range)
        {
            enum mode = Mode.skipOnMatch;
            assert(!inp.empty);
            immutable ch = inp[0];
            static if (hasASCII)
            {
                if (ch < 0x80)
                {
                    immutable r = tab!1[ch];
                    if (r)
                        inp.popFront();
                    return r;
                }
                else
                    mixin(dispatch);
            }
            else
                mixin(dispatch);
        }

        static if (Sizes.length == 4) // can skip iff can detect all encodings
        {
            public bool skip(Range)(ref Range inp) const
            if (isRandomAccessRange!Range && is(ElementType!Range : char) &&
                !isDynamicArray!Range)
            {
                enum mode = Mode.alwaysSkip;
                assert(!inp.empty);
                auto ch = inp[0];
                static if (hasASCII)
                {
                    if (ch < 0x80)
                    {
                        inp.popFront();
                        return tab!1[ch];
                    }
                    else
                        mixin(dispatch);
                }
                else
                    mixin(dispatch);
            }
        }

        public bool test(Range)(ref Range inp) const
        if (isRandomAccessRange!Range && is(ElementType!Range : char) &&
            !isDynamicArray!Range)
        {
            enum mode = Mode.neverSkip;
            assert(!inp.empty);
            auto ch = inp[0];

            static if (hasASCII)
            {
                if (ch < 0x80)
                    return tab!1[ch];
                else
                    mixin(dispatch);
            }
            else
                mixin(dispatch);
        }

        bool match(C)(ref C[] str) const
        if (isSomeChar!C)
        {
            return fwdStr!"match"(str);
        }

        bool skip(C)(ref C[] str) const
        if (isSomeChar!C)
        {
            return fwdStr!"skip"(str);
        }

        bool test(C)(ref C[] str) const
        if (isSomeChar!C)
        {
            return fwdStr!"test"(str);
        }

        mixin ForwardStrings;
    }

    struct Impl(Sizes...)
    {
        import std.meta : allSatisfy, staticMap;
        static assert(allSatisfy!(validSize, Sizes),
            "Only lengths of 1, 2, 3 and 4 code unit are possible for UTF-8");
    private:
        //pick tables for chosen sizes
        alias OurTabs = staticMap!(Table, Sizes);
        OurTabs tables;
        mixin DefMatcher;
        //static disptach helper UTF size ==> table
        alias tab(int i) = tables[i - 1];

        package(std) @property CherryPick!(Impl, SizesToPick) subMatcher(SizesToPick...)()
        {
            return CherryPick!(Impl, SizesToPick)(&this);
        }

        bool lookup(int size, Mode mode, Range)(ref Range inp) const
        {
            import std.range : popFrontN;
            if (inp.length < size)
            {
                badEncoding();
                return false;
            }
            char[size] needle = void;
            needle[0] = leadMask!size & inp[0];
            static foreach (i; 1 .. size)
            {
                needle[i] = truncate(inp[i]);
            }
            //overlong encoding checks
            static if (size == 2)
            {
                //0x80-0x7FF
                //got 6 bits in needle[1], must use at least 8 bits
                //must use at least 2 bits in needle[1]
                if (needle[0] < 2) badEncoding();
            }
            else static if (size == 3)
            {
                //0x800-0xFFFF
                //got 6 bits in needle[2], must use at least 12bits
                //must use 6 bits in needle[1] or anything in needle[0]
                if (needle[0] == 0 && needle[1] < 0x20) badEncoding();
            }
            else static if (size == 4)
            {
                //0x800-0xFFFF
                //got 2x6=12 bits in needle[2 .. 3] must use at least 17bits
                //must use 5 bits (or above) in needle[1] or anything in needle[0]
                if (needle[0] == 0 && needle[1] < 0x10) badEncoding();
            }
            static if (mode == Mode.alwaysSkip)
            {
                inp.popFrontN(size);
                return tab!size[needle];
            }
            else static if (mode == Mode.neverSkip)
            {
                return tab!size[needle];
            }
            else
            {
                static assert(mode == Mode.skipOnMatch);

                if (tab!size[needle])
                {
                    inp.popFrontN(size);
                    return true;
                }
                else
                    return false;
            }
        }
    }

    struct CherryPick(I, Sizes...)
    {
        import std.meta : allSatisfy;
        static assert(allSatisfy!(validSize, Sizes),
            "Only lengths of 1, 2, 3 and 4 code unit are possible for UTF-8");
    private:
        I* m;
        @property auto tab(int i)() const { return m.tables[i - 1]; }
        bool lookup(int size, Mode mode, Range)(ref Range inp) const
        {
            return m.lookup!(size, mode)(inp);
        }
        mixin DefMatcher;
    }
}

template Utf16Matcher()
{
    enum validSize(int sz) = sz >= 1 && sz <= 2;

    void badEncoding() pure @safe
    {
        import std.utf : UTFException;
        throw new UTFException("Invalid UTF-16 sequence");
    }

    // 1-stage ASCII
    alias AsciiSpec = AliasSeq!(bool, wchar, clamp!7);
    //2-stage BMP
    alias BmpSpec = AliasSeq!(bool, wchar, sliceBits!(7, 16), sliceBits!(0, 7));
    //4-stage - full Unicode
    //assume that 0xD800 & 0xDC00 bits are cleared
    //thus leaving 10 bit per wchar to worry about
    alias UniSpec = AliasSeq!(bool, wchar[2],
        assumeSize!(x=>x[0]>>4, 6), assumeSize!(x=>x[0]&0xf, 4),
        assumeSize!(x=>x[1]>>6, 4), assumeSize!(x=>x[1]&0x3f, 6),
    );
    alias Ascii = typeof(TrieBuilder!(AsciiSpec)(false).build());
    alias Bmp = typeof(TrieBuilder!(BmpSpec)(false).build());
    alias Uni = typeof(TrieBuilder!(UniSpec)(false).build());

    auto encode2(dchar ch)
    {
        ch -= 0x1_0000;
        assert(ch <= 0xF_FFFF);
        wchar[2] ret;
        //do not put surrogate bits, they are sliced off
        ret[0] = cast(wchar)(ch >> 10);
        ret[1] = (ch & 0xFFF);
        return ret;
    }

    auto build(Set)(Set set)
    {
        import std.algorithm.iteration : map;
        auto ascii = set & unicode.ASCII;
        auto bmp = (set & CodepointSet.fromIntervals(0x80, 0xFFFF+1))
            - CodepointSet.fromIntervals(0xD800, 0xDFFF+1);
        auto other = set - (bmp | ascii);
        auto asciiT = ascii.byCodepoint.map!(x=>cast(char) x).buildTrie!(AsciiSpec);
        auto bmpT = bmp.byCodepoint.map!(x=>cast(wchar) x).buildTrie!(BmpSpec);
        auto otherT = other.byCodepoint.map!(x=>encode2(x)).buildTrie!(UniSpec);
        alias Ret = Impl!(1,2);
        return Ret(asciiT, bmpT, otherT);
    }

    //bootstrap full UTF-16 matcher interace from
    //sizeFlags, lookupUni and ascii
    mixin template DefMatcher()
    {
        public bool match(Range)(ref Range inp) const
        if (isRandomAccessRange!Range && is(ElementType!Range : wchar) &&
            !isDynamicArray!Range)
        {
            enum mode = Mode.skipOnMatch;
            assert(!inp.empty);
            immutable ch = inp[0];
            static if (sizeFlags & 1)
            {
                if (ch < 0x80)
                {
                  if (ascii[ch])
                  {
                      inp.popFront();
                      return true;
                  }
                  else
                      return false;
                }
                return lookupUni!mode(inp);
            }
            else
                return lookupUni!mode(inp);
        }

        static if (Sizes.length == 2)
        {
            public bool skip(Range)(ref Range inp) const
            if (isRandomAccessRange!Range && is(ElementType!Range : wchar) &&
                !isDynamicArray!Range)
            {
                enum mode = Mode.alwaysSkip;
                assert(!inp.empty);
                immutable ch = inp[0];
                static if (sizeFlags & 1)
                {
                    if (ch < 0x80)
                    {
                        inp.popFront();
                        return ascii[ch];
                    }
                    else
                        return lookupUni!mode(inp);
                }
                else
                    return lookupUni!mode(inp);
            }
        }

        public bool test(Range)(ref Range inp) const
        if (isRandomAccessRange!Range && is(ElementType!Range : wchar) &&
            !isDynamicArray!Range)
        {
            enum mode = Mode.neverSkip;
            assert(!inp.empty);
            auto ch = inp[0];
            static if (sizeFlags & 1)
                return ch < 0x80 ? ascii[ch] : lookupUni!mode(inp);
            else
                return lookupUni!mode(inp);
        }

        bool match(C)(ref C[] str) const
        if (isSomeChar!C)
        {
            return fwdStr!"match"(str);
        }

        bool skip(C)(ref C[] str) const
        if (isSomeChar!C)
        {
            return fwdStr!"skip"(str);
        }

        bool test(C)(ref C[] str) const
        if (isSomeChar!C)
        {
            return fwdStr!"test"(str);
        }

        mixin ForwardStrings; //dispatch strings to range versions
    }

    struct Impl(Sizes...)
    if (Sizes.length >= 1 && Sizes.length <= 2)
    {
    private:
        import std.meta : allSatisfy;
        static assert(allSatisfy!(validSize, Sizes),
            "Only lengths of 1 and 2 code units are possible in UTF-16");
        static if (Sizes.length > 1)
            enum sizeFlags = Sizes[0] | Sizes[1];
        else
            enum sizeFlags = Sizes[0];

        static if (sizeFlags & 1)
        {
            Ascii ascii;
            Bmp bmp;
        }
        static if (sizeFlags & 2)
        {
            Uni uni;
        }
        mixin DefMatcher;

        package(std) @property CherryPick!(Impl, SizesToPick) subMatcher(SizesToPick...)()
        {
            return CherryPick!(Impl, SizesToPick)(&this);
        }

        bool lookupUni(Mode mode, Range)(ref Range inp) const
        {
            wchar x = cast(wchar)(inp[0] - 0xD800);
            //not a high surrogate
            if (x > 0x3FF)
            {
                //low surrogate
                if (x <= 0x7FF) badEncoding();
                static if (sizeFlags & 1)
                {
                    auto ch = inp[0];
                    static if (mode == Mode.alwaysSkip)
                        inp.popFront();
                    static if (mode == Mode.skipOnMatch)
                    {
                        if (bmp[ch])
                        {
                            inp.popFront();
                            return true;
                        }
                        else
                            return false;
                    }
                    else
                        return bmp[ch];
                }
                else //skip is not available for sub-matchers, so just false
                    return false;
            }
            else
            {
                import std.range : popFrontN;
                static if (sizeFlags & 2)
                {
                    if (inp.length < 2)
                        badEncoding();
                    wchar y = cast(wchar)(inp[1] - 0xDC00);
                    //not a low surrogate
                    if (y > 0x3FF)
                        badEncoding();
                    wchar[2] needle = [inp[0] & 0x3ff, inp[1] & 0x3ff];
                    static if (mode == Mode.alwaysSkip)
                        inp.popFrontN(2);
                    static if (mode == Mode.skipOnMatch)
                    {
                        if (uni[needle])
                        {
                            inp.popFrontN(2);
                            return true;
                        }
                        else
                            return false;
                    }
                    else
                        return uni[needle];
                }
                else //ditto
                    return false;
            }
        }
    }

    struct CherryPick(I, Sizes...)
    if (Sizes.length >= 1 && Sizes.length <= 2)
    {
    private:
        import std.meta : allSatisfy;
        I* m;
        enum sizeFlags = I.sizeFlags;

        static if (sizeFlags & 1)
        {
            @property auto ascii()() const { return m.ascii; }
        }

        bool lookupUni(Mode mode, Range)(ref Range inp) const
        {
            return m.lookupUni!mode(inp);
        }
        mixin DefMatcher;
        static assert(allSatisfy!(validSize, Sizes),
            "Only lengths of 1 and 2 code units are possible in UTF-16");
    }
}

private auto utf8Matcher(Set)(Set set)
{
    return Utf8Matcher!().build(set);
}

private auto utf16Matcher(Set)(Set set)
{
    return Utf16Matcher!().build(set);
}

/**
    Constructs a matcher object
    to classify $(CODEPOINTS) from the `set` for encoding
    that has `Char` as code unit.

    See $(LREF MatcherConcept) for API outline.
*/
public auto utfMatcher(Char, Set)(Set set)
if (isCodepointSet!Set)
{
    static if (is(Char : char))
        return utf8Matcher(set);
    else static if (is(Char : wchar))
        return utf16Matcher(set);
    else static if (is(Char : dchar))
        static assert(false, "UTF-32 needs no decoding,
            and thus not supported by utfMatcher");
    else
        static assert(false, "Only character types 'char' and 'wchar' are allowed");
}


//a range of code units, packed with index to speed up forward iteration
package(std) auto decoder(C)(C[] s, size_t offset=0)
if (is(C : wchar) || is(C : char))
{
    static struct Decoder
    {
    pure nothrow:
        C[] str;
        size_t idx;
        @property C front(){ return str[idx]; }
        @property C back(){ return str[$-1]; }
        void popFront(){ idx++; }
        void popBack(){ str = str[0..$-1]; }
        void popFrontN(size_t n){ idx += n; }
        @property bool empty(){ return idx == str.length; }
        @property auto save(){ return this; }
        auto opIndex(size_t i){ return str[idx+i]; }
        @property size_t length(){ return str.length - idx; }
        alias opDollar = length;
        auto opSlice(size_t a, size_t b){ return Decoder(str[0 .. idx+b], idx+a); }
    }
    static assert(isRandomAccessRange!Decoder);
    static assert(is(ElementType!Decoder : C));
    return Decoder(s, offset);
}

pure @safe unittest
{
    string rs = "hi! ﾈемног砀 текста";
    auto codec = rs.decoder;
    auto utf8 =  utf8Matcher(unicode.Letter);
    auto asc = utf8.subMatcher!(1);
    auto uni = utf8.subMatcher!(2,3,4);

    // h
    assert(asc.test(codec));
    assert(!uni.match(codec));
    assert(utf8.skip(codec));
    assert(codec.idx == 1);

    // i
    assert(asc.test(codec));
    assert(!uni.match(codec));
    assert(utf8.skip(codec));
    assert(codec.idx == 2);

    // !
    assert(!asc.match(codec));
    assert(!utf8.test(codec));
    assert(!utf8.skip(codec));
    assert(codec.idx == 3);

    // space
    assert(!asc.test(codec));
    assert(!utf8.test(codec));
    assert(!utf8.skip(codec));
    assert(codec.idx == 4);

    assert(utf8.test(codec));
    foreach (i; 0 .. 7)
    {
        assert(!asc.test(codec));
        assert(uni.test(codec));
        assert(utf8.skip(codec));
    }
    assert(!utf8.test(codec));
    assert(!utf8.skip(codec));

    //the same with match where applicable
    codec = rs.decoder;
    assert(utf8.match(codec));
    assert(codec.idx == 1);
    assert(utf8.match(codec));
    assert(codec.idx == 2);
    assert(!utf8.match(codec));
    assert(codec.idx == 2);
    assert(!utf8.skip(codec));
    assert(!utf8.skip(codec));

    foreach (i; 0 .. 7)
    {
        assert(!asc.test(codec));
        assert(utf8.test(codec));
        assert(utf8.match(codec));
    }
    auto i = codec.idx;
    assert(!utf8.match(codec));
    assert(codec.idx == i);
}

pure @system unittest
{
    import std.range : stride;
    static bool testAll(Matcher, Range)(ref Matcher m, ref Range r) @safe
    {
        bool t = m.test(r);
        auto save = r.idx;
        assert(t == m.match(r));
        assert(r.idx == save || t); //ether no change or was match
        r.idx = save;
        static if (is(typeof(m.skip(r))))
        {
            assert(t == m.skip(r));
            assert(r.idx != save); //always changed
            r.idx = save;
        }
        return t;
    }
    auto utf16 = utfMatcher!wchar(unicode.L);
    auto bmp = utf16.subMatcher!1;
    auto nonBmp = utf16.subMatcher!1;
    auto utf8 = utfMatcher!char(unicode.L);
    auto ascii = utf8.subMatcher!1;
    auto uni2 = utf8.subMatcher!2;
    auto uni3 = utf8.subMatcher!3;
    auto uni24 = utf8.subMatcher!(2,4);
    foreach (ch; unicode.L.byCodepoint.stride(3))
    {
        import std.utf : encode;
        char[4] buf;
        wchar[2] buf16;
        auto len = encode(buf, ch);
        auto len16 = encode(buf16, ch);
        auto c8 = buf[0 .. len].decoder;
        auto c16 = buf16[0 .. len16].decoder;
        assert(testAll(utf16, c16));
        assert(testAll(bmp, c16) || len16 != 1);
        assert(testAll(nonBmp, c16) || len16 != 2);

        assert(testAll(utf8, c8));

        //submatchers return false on out of their domain
        assert(testAll(ascii, c8) || len != 1);
        assert(testAll(uni2, c8) || len != 2);
        assert(testAll(uni3, c8) || len != 3);
        assert(testAll(uni24, c8) || (len != 2 && len != 4));
    }
}

// cover decode fail cases of Matcher
pure @safe unittest
{
    import std.algorithm.iteration : map;
    import std.exception : collectException;
    import std.format : format;
    auto utf16 = utfMatcher!wchar(unicode.L);
    auto utf8 = utfMatcher!char(unicode.L);
    //decode failure cases UTF-8
    alias fails8 = AliasSeq!("\xC1", "\x80\x00","\xC0\x00", "\xCF\x79",
        "\xFF\x00\0x00\0x00\x00", "\xC0\0x80\0x80\x80", "\x80\0x00\0x00\x00",
        "\xCF\x00\0x00\0x00\x00");
    foreach (msg; fails8)
    {
        assert(collectException((){
            auto s = msg;
            size_t idx = 0;
            utf8.test(s);
        }()), format("%( %2x %)", cast(immutable(ubyte)[]) msg));
    }
    //decode failure cases UTF-16
    alias fails16 = AliasSeq!([0xD811], [0xDC02]);
    foreach (msg; fails16)
    {
        assert(collectException((){
            auto s = msg.map!(x => cast(wchar) x);
            utf16.test(s);
        }()));
    }
}

/++
    Convenience function to construct optimal configurations for
    packed Trie from any `set` of $(CODEPOINTS).

    The parameter `level` indicates the number of trie levels to use,
    allowed values are: 1, 2, 3 or 4. Levels represent different trade-offs
    speed-size wise.

    $(P Level 1 is fastest and the most memory hungry (a bit array). )
    $(P Level 4 is the slowest and has the smallest footprint. )

    See the $(S_LINK Synopsis, Synopsis) section for example.

    Note:
    Level 4 stays very practical (being faster and more predictable)
    compared to using direct lookup on the `set` itself.


+/
public auto toTrie(size_t level, Set)(Set set)
if (isCodepointSet!Set)
{
    static if (level == 1)
        return codepointSetTrie!(21)(set);
    else static if (level == 2)
        return codepointSetTrie!(10, 11)(set);
    else static if (level == 3)
        return codepointSetTrie!(8, 5, 8)(set);
    else static if (level == 4)
         return codepointSetTrie!(6, 4, 4, 7)(set);
    else
        static assert(false,
            "Sorry, toTrie doesn't support levels > 4, use codepointSetTrie directly");
}

/**
    $(P Builds a `Trie` with typically optimal speed-size trade-off
    and wraps it into a delegate of the following type:
    $(D bool delegate(dchar ch)). )

    $(P Effectively this creates a 'tester' lambda suitable
    for algorithms like std.algorithm.find that take unary predicates. )

    See the $(S_LINK Synopsis, Synopsis) section for example.
*/
public auto toDelegate(Set)(Set set)
if (isCodepointSet!Set)
{
    // 3 is very small and is almost as fast as 2-level (due to CPU caches?)
    auto t = toTrie!3(set);
    return (dchar ch) => t[ch];
}

/**
    $(P Opaque wrapper around unsigned built-in integers and
    code unit (char/wchar/dchar) types.
    Parameter `sz` indicates that the value is confined
    to the range of [0, 2^^sz$(RPAREN). With this knowledge it can be
    packed more tightly when stored in certain
    data-structures like trie. )

    Note:
    $(P The $(D BitPacked!(T, sz)) is implicitly convertible to `T`
    but not vise-versa. Users have to ensure the value fits in
    the range required and use the `cast`
    operator to perform the conversion.)
*/
struct BitPacked(T, size_t sz)
if (isIntegral!T || is(T:dchar))
{
    enum bitSize = sz;
    T _value;
    alias _value this;
}

/*
    Depending on the form of the passed argument `bitSizeOf` returns
    the amount of bits required to represent a given type
    or a return type of a given functor.
*/
template bitSizeOf(Args...)
if (Args.length == 1)
{
    import std.traits : ReturnType;
    alias T = Args[0];
    static if (__traits(compiles, { size_t val = T.bitSize; })) //(is(typeof(T.bitSize) : size_t))
    {
        enum bitSizeOf = T.bitSize;
    }
    else static if (is(ReturnType!T dummy == BitPacked!(U, bits), U, size_t bits))
    {
        enum bitSizeOf = bitSizeOf!(ReturnType!T);
    }
    else
    {
        enum bitSizeOf = T.sizeof*8;
    }
}

/**
    Tests if `T` is some instantiation of $(LREF BitPacked)!(U, x)
    and thus suitable for packing.
*/
template isBitPacked(T)
{
    static if (is(T dummy == BitPacked!(U, bits), U, size_t bits))
        enum isBitPacked = true;
    else
        enum isBitPacked = false;
}

/**
    Gives the type `U` from $(LREF BitPacked)!(U, x)
    or `T` itself for every other type.
*/
template TypeOfBitPacked(T)
{
    static if (is(T dummy == BitPacked!(U, bits), U, size_t bits))
        alias TypeOfBitPacked = U;
    else
        alias TypeOfBitPacked = T;
}

/*
    Wrapper, used in definition of custom data structures from `Trie` template.
    Applying it to a unary lambda function indicates that the returned value always
    fits within `bits` of bits.
*/
struct assumeSize(alias Fn, size_t bits)
{
    enum bitSize = bits;
    static auto ref opCall(T)(auto ref T arg)
    {
        return Fn(arg);
    }
}

/*
    A helper for defining lambda function that yields a slice
    of certain bits from an unsigned integral value.
    The resulting lambda is wrapped in assumeSize and can be used directly
    with `Trie` template.
*/
struct sliceBits(size_t from, size_t to)
{
    //for now bypass assumeSize, DMD has trouble inlining it
    enum bitSize = to-from;
    static auto opCall(T)(T x)
    out(result)
    {
        assert(result < (1 << to-from));
    }
    do
    {
        static assert(from < to);
        static if (from == 0)
            return x & ((1 << to)-1);
        else
        return (x >> from) & ((1<<(to-from))-1);
    }
}

@safe pure nothrow @nogc uint low_8(uint x) { return x&0xFF; }
@safe pure nothrow @nogc uint midlow_8(uint x){ return (x&0xFF00)>>8; }
alias lo8 = assumeSize!(low_8, 8);
alias mlo8 = assumeSize!(midlow_8, 8);

@safe pure nothrow @nogc unittest
{
    static assert(bitSizeOf!lo8 == 8);
    static assert(bitSizeOf!(sliceBits!(4, 7)) == 3);
    static assert(bitSizeOf!(BitPacked!(uint, 2)) == 2);
}

template Sequence(size_t start, size_t end)
{
    static if (start < end)
        alias Sequence = AliasSeq!(start, Sequence!(start+1, end));
    else
        alias Sequence = AliasSeq!();
}

//---- TRIE TESTS ----
@system unittest
{
    import std.algorithm.iteration : map;
    import std.algorithm.sorting : sort;
    import std.array : array;
    import std.conv : text, to;
    import std.range : iota;
    static trieStats(TRIE)(TRIE t)
    {
        version (std_uni_stats)
        {
            import std.stdio : writefln, writeln;
            writeln("---TRIE FOOTPRINT STATS---");
            static foreach (i; 0 .. t.table.dim)
            {
                writefln("lvl%s = %s bytes;  %s pages"
                         , i, t.bytes!i, t.pages!i);
            }
            writefln("TOTAL: %s bytes", t.bytes);
            version (none)
            {
                writeln("INDEX (excluding value level):");
                static foreach (i; 0 .. t.table.dim-1)
                    writeln(t.table.slice!(i)[0 .. t.table.length!i]);
            }
            writeln("---------------------------");
        }
    }
    //@@@BUG link failure, lambdas not found by linker somehow (in case of trie2)
    // alias lo8   = assumeSize!(8, function (uint x) { return x&0xFF; });
    // alias next8 = assumeSize!(7, function (uint x) { return (x&0x7F00)>>8; });
    alias Set = CodepointSet;
    auto set = Set('A','Z','a','z');
    auto trie = buildTrie!(bool, uint, 256, lo8)(set.byInterval);// simple bool array
    for (int a='a'; a<'z';a++)
        assert(trie[a]);
    for (int a='A'; a<'Z';a++)
        assert(trie[a]);
    for (int a=0; a<'A'; a++)
        assert(!trie[a]);
    for (int a ='Z'; a<'a'; a++)
        assert(!trie[a]);
    trieStats(trie);

    auto redundant2 = Set(
        1, 18, 256+2, 256+111, 512+1, 512+18, 768+2, 768+111);
    auto trie2 = buildTrie!(bool, uint, 1024, mlo8, lo8)(redundant2.byInterval);
    trieStats(trie2);
    foreach (e; redundant2.byCodepoint)
        assert(trie2[e], text(cast(uint) e, " - ", trie2[e]));
    foreach (i; 0 .. 1024)
    {
        assert(trie2[i] == (i in redundant2));
    }


    auto redundant3 = Set(
          2,    4,    6,    8,    16,
       2+16, 4+16, 16+6, 16+8, 16+16,
       2+32, 4+32, 32+6, 32+8,
      );

    enum max3 = 256;
    // sliceBits
    auto trie3 = buildTrie!(bool, uint, max3,
            sliceBits!(6,8), sliceBits!(4,6), sliceBits!(0,4)
        )(redundant3.byInterval);
    trieStats(trie3);
    foreach (i; 0 .. max3)
        assert(trie3[i] == (i in redundant3), text(cast(uint) i));

    auto redundant4 = Set(
            10, 64, 64+10, 128, 128+10, 256, 256+10, 512,
            1000, 2000, 3000, 4000, 5000, 6000
        );
    enum max4 = 2^^16;
    auto trie4 = buildTrie!(bool, size_t, max4,
            sliceBits!(13, 16), sliceBits!(9, 13), sliceBits!(6, 9) , sliceBits!(0, 6)
        )(redundant4.byInterval);
    foreach (i; 0 .. max4)
    {
        if (i in redundant4)
            assert(trie4[i], text(cast(uint) i));
    }
    trieStats(trie4);

        alias mapToS = mapTrieIndex!(useItemAt!(0, char));
        string[] redundantS = ["tea", "start", "orange"];
        redundantS.sort!((a,b) => mapToS(a) < mapToS(b))();
        auto strie = buildTrie!(bool, string, useItemAt!(0, char))(redundantS);
        // using first char only
        assert(redundantS == ["orange", "start", "tea"]);
        assert(strie["test"], text(strie["test"]));
        assert(!strie["aea"]);
        assert(strie["s"]);

    // a bit size test
    auto a = array(map!(x => to!ubyte(x))(iota(0, 256)));
    auto bt = buildTrie!(bool, ubyte, sliceBits!(7, 8), sliceBits!(5, 7), sliceBits!(0, 5))(a);
    trieStats(bt);
    foreach (i; 0 .. 256)
        assert(bt[cast(ubyte) i]);
}

template useItemAt(size_t idx, T)
if (isIntegral!T || is(T: dchar))
{
    size_t impl(const scope T[] arr){ return arr[idx]; }
    alias useItemAt = assumeSize!(impl, 8*T.sizeof);
}

template useLastItem(T)
{
    size_t impl(const scope T[] arr){ return arr[$-1]; }
    alias useLastItem = assumeSize!(impl, 8*T.sizeof);
}

template fullBitSize(Prefix...)
{
    static if (Prefix.length > 0)
        enum fullBitSize = bitSizeOf!(Prefix[0])+fullBitSize!(Prefix[1..$]);
    else
        enum fullBitSize = 0;
}

template idxTypes(Key, size_t fullBits, Prefix...)
{
    static if (Prefix.length == 1)
    {// the last level is value level, so no index once reduced to 1-level
        alias idxTypes = AliasSeq!();
    }
    else
    {
        // Important note on bit packing
        // Each level has to hold enough of bits to address the next one
        // The bottom level is known to hold full bit width
        // thus it's size in pages is full_bit_width - size_of_last_prefix
        // Recourse on this notion
        alias idxTypes =
            AliasSeq!(
                idxTypes!(Key, fullBits - bitSizeOf!(Prefix[$-1]), Prefix[0..$-1]),
                BitPacked!(typeof(Prefix[$-2](Key.init)), fullBits - bitSizeOf!(Prefix[$-1]))
            );
    }
}

//============================================================================

@safe pure int comparePropertyName(Char1, Char2)(const(Char1)[] a, const(Char2)[] b)
if (is(Char1 : dchar) && is(Char2 : dchar))
{
    import std.algorithm.comparison : cmp;
    import std.algorithm.iteration : map, filter;
    import std.ascii : toLower;
    static bool pred(dchar c) {return !c.isWhite && c != '-' && c != '_';}
    return cmp(
        a.map!toLower.filter!pred,
        b.map!toLower.filter!pred);
}

@safe pure unittest
{
    assert(!comparePropertyName("foo-bar", "fooBar"));
}

bool propertyNameLess(Char1, Char2)(const(Char1)[] a, const(Char2)[] b) @safe pure
if (is(Char1 : dchar) && is(Char2 : dchar))
{
    return comparePropertyName(a, b) < 0;
}

//============================================================================
// Utilities for compression of Unicode code point sets
//============================================================================

@safe void compressTo(uint val, ref scope ubyte[] arr) pure nothrow
{
    // not optimized as usually done 1 time (and not public interface)
    if (val < 128)
        arr ~= cast(ubyte) val;
    else if (val < (1 << 13))
    {
        arr ~= (0b1_00 << 5) | cast(ubyte)(val >> 8);
        arr ~= val & 0xFF;
    }
    else
    {
        assert(val < (1 << 21));
        arr ~= (0b1_01 << 5) | cast(ubyte)(val >> 16);
        arr ~= (val >> 8) & 0xFF;
        arr ~= val  & 0xFF;
    }
}

@safe uint decompressFrom(scope const(ubyte)[] arr, ref size_t idx) pure
{
    import std.exception : enforce;
    immutable first = arr[idx++];
    if (!(first & 0x80)) // no top bit -> [0 .. 127]
        return first;
    immutable extra = ((first >> 5) & 1) + 1; // [1, 2]
    uint val = (first & 0x1F);
    enforce(idx + extra <= arr.length, "bad code point interval encoding");
    foreach (j; 0 .. extra)
        val = (val << 8) | arr[idx+j];
    idx += extra;
    return val;
}


package(std) ubyte[] compressIntervals(Range)(Range intervals)
if (isInputRange!Range && isIntegralPair!(ElementType!Range))
{
    ubyte[] storage;
    uint base = 0;
    // RLE encode
    foreach (val; intervals)
    {
        compressTo(val[0]-base, storage);
        base = val[0];
        if (val[1] != lastDchar+1) // till the end of the domain so don't store it
        {
            compressTo(val[1]-base, storage);
            base = val[1];
        }
    }
    return storage;
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    auto run = [tuple(80, 127), tuple(128, (1 << 10)+128)];
    ubyte[] enc = [cast(ubyte) 80, 47, 1, (0b1_00 << 5) | (1 << 2), 0];
    assert(compressIntervals(run) == enc);
    auto run2 = [tuple(0, (1 << 20)+512+1), tuple((1 << 20)+512+4, lastDchar+1)];
    ubyte[] enc2 = [cast(ubyte) 0, (0b1_01 << 5) | (1 << 4), 2, 1, 3]; // odd length-ed
    assert(compressIntervals(run2) == enc2);
    size_t  idx = 0;
    assert(decompressFrom(enc, idx) == 80);
    assert(decompressFrom(enc, idx) == 47);
    assert(decompressFrom(enc, idx) == 1);
    assert(decompressFrom(enc, idx) == (1 << 10));
    idx = 0;
    assert(decompressFrom(enc2, idx) == 0);
    assert(decompressFrom(enc2, idx) == (1 << 20)+512+1);
    assert(equal(decompressIntervals(compressIntervals(run)), run));
    assert(equal(decompressIntervals(compressIntervals(run2)), run2));
}

// Creates a range of `CodepointInterval` that lazily decodes compressed data.
@safe package(std) auto decompressIntervals(const(ubyte)[] data) pure
{
    return DecompressedIntervals(data);
}

@safe struct DecompressedIntervals
{
pure:
    const(ubyte)[] _stream;
    size_t _idx;
    CodepointInterval _front;

    this(const(ubyte)[] stream)
    {
        _stream = stream;
        popFront();
    }

    @property CodepointInterval front()
    {
        assert(!empty);
        return _front;
    }

    void popFront()
    {
        if (_idx == _stream.length)
        {
            _idx = size_t.max;
            return;
        }
        uint base = _front[1];
        _front[0] = base + decompressFrom(_stream, _idx);
        if (_idx == _stream.length)// odd length ---> till the end
            _front[1] = lastDchar+1;
        else
        {
            base = _front[0];
            _front[1] = base + decompressFrom(_stream, _idx);
        }
    }

    @property bool empty() const
    {
        return _idx == size_t.max;
    }

    @property DecompressedIntervals save() return scope { return this; }
}

@safe pure nothrow @nogc unittest
{
    static assert(isInputRange!DecompressedIntervals);
    static assert(isForwardRange!DecompressedIntervals);
}

//============================================================================

version (std_uni_bootstrap){}
else
{

// helper for looking up code point sets
ptrdiff_t findUnicodeSet(alias table, C)(const scope C[] name)
{
    import std.algorithm.iteration : map;
    import std.range : assumeSorted;
    auto range = assumeSorted!((a,b) => propertyNameLess(a,b))
        (table.map!"a.name"());
    size_t idx = range.lowerBound(name).length;
    if (idx < range.length && comparePropertyName(range[idx], name) == 0)
        return idx;
    return -1;
}

// another one that loads it
bool loadUnicodeSet(alias table, Set, C)(const scope C[] name, ref Set dest)
{
    auto idx = findUnicodeSet!table(name);
    if (idx >= 0)
    {
        dest = Set(asSet(table[idx].compressed));
        return true;
    }
    return false;
}

bool loadProperty(Set=CodepointSet, C)
    (const scope C[] name, ref Set target) pure
{
    import std.internal.unicode_tables : uniProps; // generated file
    alias ucmp = comparePropertyName;
    // conjure cumulative properties by hand
    if (ucmp(name, "L") == 0 || ucmp(name, "Letter") == 0)
    {
        target = asSet(uniProps.Lu);
        target |= asSet(uniProps.Ll);
        target |= asSet(uniProps.Lt);
        target |= asSet(uniProps.Lo);
        target |= asSet(uniProps.Lm);
    }
    else if (ucmp(name,"LC") == 0 || ucmp(name,"Cased Letter")==0)
    {
        target = asSet(uniProps.Ll);
        target |= asSet(uniProps.Lu);
        target |= asSet(uniProps.Lt);// Title case
    }
    else if (ucmp(name, "M") == 0 || ucmp(name, "Mark") == 0)
    {
        target = asSet(uniProps.Mn);
        target |= asSet(uniProps.Mc);
        target |= asSet(uniProps.Me);
    }
    else if (ucmp(name, "N") == 0 || ucmp(name, "Number") == 0)
    {
        target = asSet(uniProps.Nd);
        target |= asSet(uniProps.Nl);
        target |= asSet(uniProps.No);
    }
    else if (ucmp(name, "P") == 0 || ucmp(name, "Punctuation") == 0)
    {
        target = asSet(uniProps.Pc);
        target |= asSet(uniProps.Pd);
        target |= asSet(uniProps.Ps);
        target |= asSet(uniProps.Pe);
        target |= asSet(uniProps.Pi);
        target |= asSet(uniProps.Pf);
        target |= asSet(uniProps.Po);
    }
    else if (ucmp(name, "S") == 0 || ucmp(name, "Symbol") == 0)
    {
        target = asSet(uniProps.Sm);
        target |= asSet(uniProps.Sc);
        target |= asSet(uniProps.Sk);
        target |= asSet(uniProps.So);
    }
    else if (ucmp(name, "Z") == 0 || ucmp(name, "Separator") == 0)
    {
        target = asSet(uniProps.Zs);
        target |= asSet(uniProps.Zl);
        target |= asSet(uniProps.Zp);
    }
    else if (ucmp(name, "C") == 0 || ucmp(name, "Other") == 0)
    {
        target = asSet(uniProps.Cc);
        target |= asSet(uniProps.Cf);
        target |= asSet(uniProps.Cs);
        target |= asSet(uniProps.Co);
        target |= asSet(uniProps.Cn);
    }
    else if (ucmp(name, "graphical") == 0)
    {
        target = asSet(uniProps.Alphabetic);

        target |= asSet(uniProps.Mn);
        target |= asSet(uniProps.Mc);
        target |= asSet(uniProps.Me);

        target |= asSet(uniProps.Nd);
        target |= asSet(uniProps.Nl);
        target |= asSet(uniProps.No);

        target |= asSet(uniProps.Pc);
        target |= asSet(uniProps.Pd);
        target |= asSet(uniProps.Ps);
        target |= asSet(uniProps.Pe);
        target |= asSet(uniProps.Pi);
        target |= asSet(uniProps.Pf);
        target |= asSet(uniProps.Po);

        target |= asSet(uniProps.Zs);

        target |= asSet(uniProps.Sm);
        target |= asSet(uniProps.Sc);
        target |= asSet(uniProps.Sk);
        target |= asSet(uniProps.So);
    }
    else if (ucmp(name, "any") == 0)
        target = Set.fromIntervals(0, 0x110000);
    else if (ucmp(name, "ascii") == 0)
        target = Set.fromIntervals(0, 0x80);
    else
        return loadUnicodeSet!(uniProps.tab)(name, target);
    return true;
}

// CTFE-only helper for checking property names at compile-time
@safe bool isPrettyPropertyName(C)(const scope C[] name)
{
    import std.algorithm.searching : find;
    auto names = [
        "L", "Letter",
        "LC", "Cased Letter",
        "M", "Mark",
        "N", "Number",
        "P", "Punctuation",
        "S", "Symbol",
        "Z", "Separator",
        "Graphical",
        "any",
        "ascii"
    ];
    auto x = find!(x => comparePropertyName(x, name) == 0)(names);
    return !x.empty;
}

// ditto, CTFE-only, not optimized
@safe private static bool findSetName(alias table, C)(const scope C[] name)
{
    return findUnicodeSet!table(name) >= 0;
}

template SetSearcher(alias table, string kind)
{
    /// Run-time checked search.
    static auto opCall(C)(const scope C[] name)
    if (is(C : dchar))
    {
        import std.conv : to;
        CodepointSet set;
        if (loadUnicodeSet!table(name, set))
            return set;
        throw new Exception("No unicode set for "~kind~" by name "
            ~name.to!string()~" was found.");
    }
    /// Compile-time checked search.
    static @property auto opDispatch(string name)()
    {
        static if (findSetName!table(name))
        {
            CodepointSet set;
            loadUnicodeSet!table(name, set);
            return set;
        }
        else
            static assert(false, "No unicode set for "~kind~" by name "
                ~name~" was found.");
    }
}

// Characters that need escaping in string posed as regular expressions
package(std) alias Escapables = AliasSeq!('[', ']', '\\', '^', '$', '.', '|', '?', ',', '-',
    ';', ':', '#', '&', '%', '/', '<', '>', '`',  '*', '+', '(', ')', '{', '}',  '~');

package(std) CodepointSet memoizeExpr(string expr)()
{
    if (__ctfe)
        return mixin(expr);
    alias T = typeof(mixin(expr));
    static T slot;
    static bool initialized;
    if (!initialized)
    {
        slot =  mixin(expr);
        initialized = true;
    }
    return slot;
}

//property for \w character class
package(std) @property CodepointSet wordCharacter() @safe
{
    return memoizeExpr!("unicode.Alphabetic | unicode.Mn | unicode.Mc
        | unicode.Me | unicode.Nd | unicode.Pc")();
}

//basic stack, just in case it gets used anywhere else then Parser
package(std) struct Stack(T)
{
@safe:
    T[] data;
    @property bool empty(){ return data.empty; }

    @property size_t length(){ return data.length; }

    void push(T val){ data ~= val;  }

    @trusted T pop()
    {
        assert(!empty);
        auto val = data[$ - 1];
        data = data[0 .. $ - 1];
        if (!__ctfe)
            cast(void) data.assumeSafeAppend();
        return val;
    }

    @property ref T top()
    {
        assert(!empty);
        return data[$ - 1];
    }
}

//test if a given string starts with hex number of maxDigit that's a valid codepoint
//returns it's value and skips these maxDigit chars on success, throws on failure
package(std) dchar parseUniHex(Range)(ref Range str, size_t maxDigit)
{
    import std.exception : enforce;
    //std.conv.parse is both @system and bogus
    uint val;
    for (int k = 0; k < maxDigit; k++)
    {
        enforce(!str.empty, "incomplete escape sequence");
        //accepts ascii only, so it's OK to index directly
        immutable current = str.front;
        if ('0' <= current && current <= '9')
            val = val * 16 + current - '0';
        else if ('a' <= current && current <= 'f')
            val = val * 16 + current -'a' + 10;
        else if ('A' <= current && current <= 'F')
            val = val * 16 + current - 'A' + 10;
        else
            throw new Exception("invalid escape sequence");
        str.popFront();
    }
    enforce(val <= 0x10FFFF, "invalid codepoint");
    return val;
}

@safe unittest
{
    import std.algorithm.searching : canFind;
    import std.exception : collectException;
    string[] non_hex = [ "000j", "000z", "FffG", "0Z"];
    string[] hex = [ "01", "ff", "00af", "10FFFF" ];
    int[] value = [ 1, 0xFF, 0xAF, 0x10FFFF ];
    foreach (v; non_hex)
        assert(collectException(parseUniHex(v, v.length)).msg
          .canFind("invalid escape sequence"));
    foreach (i, v; hex)
        assert(parseUniHex(v, v.length) == value[i]);
    string over = "0011FFFF";
    assert(collectException(parseUniHex(over, over.length)).msg
      .canFind("invalid codepoint"));
}

auto caseEnclose(CodepointSet set)
{
    auto cased = set & unicode.LC;
    foreach (dchar ch; cased.byCodepoint)
    {
        foreach (c; simpleCaseFoldings(ch))
            set |= c;
    }
    return set;
}

/+
    fetch codepoint set corresponding to a name (InBlock or binary property)
+/
CodepointSet getUnicodeSet(const scope char[] name, bool negated,  bool casefold) @safe
{
    CodepointSet s = unicode(name);
    //FIXME: caseEnclose for new uni as Set | CaseEnclose(SET && LC)
    if (casefold)
       s = caseEnclose(s);
    if (negated)
        s = s.inverted;
    return s;
}

struct UnicodeSetParser(Range)
{
    import std.exception : enforce;
    import std.typecons : tuple, Tuple;
    Range range;
    bool casefold_;

    @property bool empty(){ return range.empty; }
    @property dchar front(){ return range.front; }
    void popFront(){ range.popFront(); }

    //CodepointSet operations relatively in order of priority
    enum Operator:uint {
        Open = 0, Negate,  Difference, SymDifference, Intersection, Union, None
    }

    //parse unit of CodepointSet spec, most notably escape sequences and char ranges
    //also fetches next set operation
    Tuple!(CodepointSet,Operator) parseCharTerm()
    {
        import std.range : drop;
        enum privateUseStart = '\U000F0000', privateUseEnd ='\U000FFFFD';
        enum State{ Start, Char, Escape, CharDash, CharDashEscape,
            PotentialTwinSymbolOperator }
        Operator op = Operator.None;
        dchar last;
        CodepointSet set;
        State state = State.Start;

        void addWithFlags(ref CodepointSet set, uint ch)
        {
            if (casefold_)
            {
                auto foldings = simpleCaseFoldings(ch);
                foreach (v; foldings)
                    set |= v;
            }
            else
                set |= ch;
        }

        static Operator twinSymbolOperator(dchar symbol)
        {
            switch (symbol)
            {
            case '|':
                return Operator.Union;
            case '-':
                return Operator.Difference;
            case '~':
                return Operator.SymDifference;
            case '&':
                return Operator.Intersection;
            default:
                assert(false);
            }
        }

        L_CharTermLoop:
        for (;;)
        {
            final switch (state)
            {
            case State.Start:
                switch (front)
                {
                case '|':
                case '-':
                case '~':
                case '&':
                    state = State.PotentialTwinSymbolOperator;
                    last = front;
                    break;
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    break L_CharTermLoop;
                case '\\':
                    state = State.Escape;
                    break;
                default:
                    state = State.Char;
                    last = front;
                }
                break;
            case State.Char:
                // xxx last front xxx
                switch (front)
                {
                case '|':
                case '~':
                case '&':
                    // then last is treated as normal char and added as implicit union
                    state = State.PotentialTwinSymbolOperator;
                    addWithFlags(set, last);
                    last = front;
                    break;
                case '-': // still need more info
                    state = State.CharDash;
                    break;
                case '\\':
                    set |= last;
                    state = State.Escape;
                    break;
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    addWithFlags(set, last);
                    break L_CharTermLoop;
                default:
                    state = State.Char;
                    addWithFlags(set, last);
                    last = front;
                }
                break;
            case State.PotentialTwinSymbolOperator:
                // xxx last front xxxx
                // where last = [|-&~]
                if (front == last)
                {
                    op = twinSymbolOperator(last);
                    popFront();//skip second twin char
                    break L_CharTermLoop;
                }
                goto case State.Char;
            case State.Escape:
                // xxx \ front xxx
                switch (front)
                {
                case 'f':
                    last = '\f';
                    state = State.Char;
                    break;
                case 'n':
                    last = '\n';
                    state = State.Char;
                    break;
                case 'r':
                    last = '\r';
                    state = State.Char;
                    break;
                case 't':
                    last = '\t';
                    state = State.Char;
                    break;
                case 'v':
                    last = '\v';
                    state = State.Char;
                    break;
                case 'c':
                    last = unicode.parseControlCode(this);
                    state = State.Char;
                    break;
                foreach (val; Escapables)
                {
                case val:
                }
                    last = front;
                    state = State.Char;
                    break;
                case 'p':
                    set.add(unicode.parsePropertySpec(this, false, casefold_));
                    state = State.Start;
                    continue L_CharTermLoop; //next char already fetched
                case 'P':
                    set.add(unicode.parsePropertySpec(this, true, casefold_));
                    state = State.Start;
                    continue L_CharTermLoop; //next char already fetched
                case 'x':
                    popFront();
                    last = parseUniHex(this, 2);
                    state = State.Char;
                    continue L_CharTermLoop;
                case 'u':
                    popFront();
                    last = parseUniHex(this, 4);
                    state = State.Char;
                    continue L_CharTermLoop;
                case 'U':
                    popFront();
                    last = parseUniHex(this, 8);
                    state = State.Char;
                    continue L_CharTermLoop;
                case 'd':
                    set.add(unicode.Nd);
                    state = State.Start;
                    break;
                case 'D':
                    set.add(unicode.Nd.inverted);
                    state = State.Start;
                    break;
                case 's':
                    set.add(unicode.White_Space);
                    state = State.Start;
                    break;
                case 'S':
                    set.add(unicode.White_Space.inverted);
                    state = State.Start;
                    break;
                case 'w':
                    set.add(wordCharacter);
                    state = State.Start;
                    break;
                case 'W':
                    set.add(wordCharacter.inverted);
                    state = State.Start;
                    break;
                default:
                    if (front >= privateUseStart && front <= privateUseEnd)
                        enforce(false, "no matching ']' found while parsing character class");
                    enforce(false, "invalid escape sequence");
                }
                break;
            case State.CharDash:
                // xxx last - front xxx
                switch (front)
                {
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    //means dash is a single char not an interval specifier
                    addWithFlags(set, last);
                    addWithFlags(set, '-');
                    break L_CharTermLoop;
                 case '-'://set Difference again
                    addWithFlags(set, last);
                    op = Operator.Difference;
                    popFront();//skip '-'
                    break L_CharTermLoop;
                case '\\':
                    state = State.CharDashEscape;
                    break;
                default:
                    enforce(last <= front, "inverted range");
                    if (casefold_)
                    {
                        for (uint ch = last; ch <= front; ch++)
                            addWithFlags(set, ch);
                    }
                    else
                        set.add(last, front + 1);
                    state = State.Start;
                }
                break;
            case State.CharDashEscape:
            //xxx last - \ front xxx
                uint end;
                switch (front)
                {
                case 'f':
                    end = '\f';
                    break;
                case 'n':
                    end = '\n';
                    break;
                case 'r':
                    end = '\r';
                    break;
                case 't':
                    end = '\t';
                    break;
                case 'v':
                    end = '\v';
                    break;
                foreach (val; Escapables)
                {
                case val:
                }
                    end = front;
                    break;
                case 'c':
                    end = unicode.parseControlCode(this);
                    break;
                case 'x':
                    popFront();
                    end = parseUniHex(this, 2);
                    enforce(last <= end,"inverted range");
                    set.add(last, end + 1);
                    state = State.Start;
                    continue L_CharTermLoop;
                case 'u':
                    popFront();
                    end = parseUniHex(this, 4);
                    enforce(last <= end,"inverted range");
                    set.add(last, end + 1);
                    state = State.Start;
                    continue L_CharTermLoop;
                case 'U':
                    popFront();
                    end = parseUniHex(this, 8);
                    enforce(last <= end,"inverted range");
                    set.add(last, end + 1);
                    state = State.Start;
                    continue L_CharTermLoop;
                default:
                    if (front >= privateUseStart && front <= privateUseEnd)
                        enforce(false, "no matching ']' found while parsing character class");
                    enforce(false, "invalid escape sequence");
                }
                // Lookahead to check if it's a \T
                // where T is sub-pattern terminator in multi-pattern scheme
                auto lookahead = range.save.drop(1);
                if (end == '\\' && !lookahead.empty)
                {
                    if (lookahead.front >= privateUseStart && lookahead.front <= privateUseEnd)
                        enforce(false, "no matching ']' found while parsing character class");
                }
                enforce(last <= end,"inverted range");
                set.add(last, end + 1);
                state = State.Start;
                break;
            }
            popFront();
            enforce(!empty, "unexpected end of CodepointSet");
        }
        return tuple(set, op);
    }

    alias ValStack = Stack!(CodepointSet);
    alias OpStack = Stack!(Operator);

    CodepointSet parseSet()
    {
        ValStack vstack;
        OpStack opstack;
        import std.functional : unaryFun;
        enforce(!empty, "unexpected end of input");
        enforce(front == '[', "expected '[' at the start of unicode set");
        //
        static bool apply(Operator op, ref ValStack stack)
        {
            switch (op)
            {
            case Operator.Negate:
                enforce(!stack.empty, "no operand for '^'");
                stack.top = stack.top.inverted;
                break;
            case Operator.Union:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '||'");
                stack.top.add(s);
                break;
            case Operator.Difference:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '--'");
                stack.top.sub(s);
                break;
            case Operator.SymDifference:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '~~'");
                stack.top ~= s;
                break;
            case Operator.Intersection:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '&&'");
                stack.top.intersect(s);
                break;
            default:
                return false;
            }
            return true;
        }
        static bool unrollWhile(alias cond)(ref ValStack vstack, ref OpStack opstack)
        {
            while (cond(opstack.top))
            {
                if (!apply(opstack.pop(),vstack))
                    return false;//syntax error
                if (opstack.empty)
                    return false;
            }
            return true;
        }

        L_CharsetLoop:
        do
        {
            switch (front)
            {
            case '[':
                opstack.push(Operator.Open);
                popFront();
                enforce(!empty, "unexpected end of character class");
                if (front == '^')
                {
                    opstack.push(Operator.Negate);
                    popFront();
                    enforce(!empty, "unexpected end of character class");
                }
                else if (front == ']') // []...] is special cased
                {
                    popFront();
                    enforce(!empty, "wrong character set");
                    auto pair = parseCharTerm();
                    pair[0].add(']', ']'+1);
                    if (pair[1] != Operator.None)
                    {
                        if (opstack.top == Operator.Union)
                            unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                        opstack.push(pair[1]);
                    }
                    vstack.push(pair[0]);
                }
                break;
            case ']':
                enforce(unrollWhile!(unaryFun!"a != a.Open")(vstack, opstack),
                    "character class syntax error");
                enforce(!opstack.empty, "unmatched ']'");
                opstack.pop();
                popFront();
                if (opstack.empty)
                    break L_CharsetLoop;
                auto pair  = parseCharTerm();
                if (!pair[0].empty)//not only operator e.g. -- or ~~
                {
                    vstack.top.add(pair[0]);//apply union
                }
                if (pair[1] != Operator.None)
                {
                    if (opstack.top == Operator.Union)
                        unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                    opstack.push(pair[1]);
                }
                break;
            //
            default://yet another pair of term(op)?
                auto pair = parseCharTerm();
                if (pair[1] != Operator.None)
                {
                    if (opstack.top == Operator.Union)
                        unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                    opstack.push(pair[1]);
                }
                vstack.push(pair[0]);
            }

        }while (!empty || !opstack.empty);
        while (!opstack.empty)
            apply(opstack.pop(),vstack);
        assert(vstack.length == 1);
        return vstack.top;
    }
}

/**
    A single entry point to lookup Unicode $(CODEPOINT) sets by name or alias of
    a block, script or general category.

    It uses well defined standard rules of property name lookup.
    This includes fuzzy matching of names, so that
    'White_Space', 'white-SpAce' and 'whitespace' are all considered equal
    and yield the same set of white space $(CHARACTERS).
*/
@safe public struct unicode
{
    import std.exception : enforce;
    /**
        Performs the lookup of set of $(CODEPOINTS)
        with compile-time correctness checking.
        This short-cut version combines 3 searches:
        across blocks, scripts, and common binary properties.

        Note that since scripts and blocks overlap the
        usual trick to disambiguate is used - to get a block use
        `unicode.InBlockName`, to search a script
        use `unicode.ScriptName`.

        See_Also: $(LREF block), $(LREF script)
        and (not included in this search) $(LREF hangulSyllableType).
    */

    static @property auto opDispatch(string name)() pure
    {
        static if (findAny(name))
            return loadAny(name);
        else
            static assert(false, "No unicode set by name "~name~" was found.");
    }

    ///
    @safe unittest
    {
        import std.exception : collectException;
        auto ascii = unicode.ASCII;
        assert(ascii['A']);
        assert(ascii['~']);
        assert(!ascii['\u00e0']);
        // matching is case-insensitive
        assert(ascii == unicode.ascII);
        assert(!ascii['à']);
        // underscores, '-' and whitespace in names are ignored too
        auto latin = unicode.in_latin1_Supplement;
        assert(latin['à']);
        assert(!latin['$']);
        // BTW Latin 1 Supplement is a block, hence "In" prefix
        assert(latin == unicode("In Latin 1 Supplement"));
        // run-time look up throws if no such set is found
        assert(collectException(unicode("InCyrilliac")));
    }

    /**
        The same lookup across blocks, scripts, or binary properties,
        but performed at run-time.
        This version is provided for cases where `name`
        is not known beforehand; otherwise compile-time
        checked $(LREF opDispatch) is typically a better choice.

        See the $(S_LINK Unicode properties, table of properties) for available
        sets.
    */
    static auto opCall(C)(const scope C[] name)
    if (is(C : dchar))
    {
        return loadAny(name);
    }

    /**
        Narrows down the search for sets of $(CODEPOINTS) to all Unicode blocks.

        Note:
        Here block names are unambiguous as no scripts are searched
        and thus to search use simply `unicode.block.BlockName` notation.

        See $(S_LINK Unicode properties, table of properties) for available sets.
        See_Also: $(S_LINK Unicode properties, table of properties).
    */
    struct block
    {
        import std.internal.unicode_tables : blocks; // generated file
        mixin SetSearcher!(blocks.tab, "block");
    }

    ///
    @safe unittest
    {
        // use .block for explicitness
        assert(unicode.block.Greek_and_Coptic == unicode.InGreek_and_Coptic);
    }

    /**
        Narrows down the search for sets of $(CODEPOINTS) to all Unicode scripts.

        See the $(S_LINK Unicode properties, table of properties) for available
        sets.
    */
    struct script
    {
        import std.internal.unicode_tables : scripts; // generated file
        mixin SetSearcher!(scripts.tab, "script");
    }

    ///
    @safe unittest
    {
        auto arabicScript = unicode.script.arabic;
        auto arabicBlock = unicode.block.arabic;
        // there is an intersection between script and block
        assert(arabicBlock['؁']);
        assert(arabicScript['؁']);
        // but they are different
        assert(arabicBlock != arabicScript);
        assert(arabicBlock == unicode.inArabic);
        assert(arabicScript == unicode.arabic);
    }

    /**
        Fetch a set of $(CODEPOINTS) that have the given hangul syllable type.

        Other non-binary properties (once supported) follow the same
        notation - `unicode.propertyName.propertyValue` for compile-time
        checked access and `unicode.propertyName(propertyValue)`
        for run-time checked one.

        See the $(S_LINK Unicode properties, table of properties) for available
        sets.
    */
    struct hangulSyllableType
    {
        import std.internal.unicode_tables : hangul; // generated file
        mixin SetSearcher!(hangul.tab, "hangul syllable type");
    }

    ///
    @safe unittest
    {
        // L here is syllable type not Letter as in unicode.L short-cut
        auto leadingVowel = unicode.hangulSyllableType("L");
        // check that some leading vowels are present
        foreach (vowel; '\u1110'..'\u115F')
            assert(leadingVowel[vowel]);
        assert(leadingVowel == unicode.hangulSyllableType.L);
    }

    //parse control code of form \cXXX, c assumed to be the current symbol
    static package(std) dchar parseControlCode(Parser)(ref Parser p)
    {
        with(p)
        {
            popFront();
            enforce(!empty, "Unfinished escape sequence");
            enforce(('a' <= front && front <= 'z')
                || ('A' <= front && front <= 'Z'),
            "Only letters are allowed after \\c");
            return front & 0x1f;
        }
    }

    //parse and return a CodepointSet for \p{...Property...} and \P{...Property..},
    //\ - assumed to be processed, p - is current
    static package(std) CodepointSet parsePropertySpec(Range)(ref Range p,
        bool negated, bool casefold)
    {
        static import std.ascii;
        with(p)
        {
            enum MAX_PROPERTY = 128;
            char[MAX_PROPERTY] result;
            uint k = 0;
            popFront();
            enforce(!empty, "eof parsing unicode property spec");
            if (front == '{')
            {
                popFront();
                while (k < MAX_PROPERTY && !empty && front !='}'
                    && front !=':')
                {
                    if (front != '-' && front != ' ' && front != '_')
                        result[k++] = cast(char) std.ascii.toLower(front);
                    popFront();
                }
                enforce(k != MAX_PROPERTY, "invalid property name");
                enforce(front == '}', "} expected ");
            }
            else
            {//single char properties e.g.: \pL, \pN ...
                enforce(front < 0x80, "invalid property name");
                result[k++] = cast(char) front;
            }
            auto s = getUnicodeSet(result[0 .. k], negated, casefold);
            enforce(!s.empty, "unrecognized unicode property spec");
            popFront();
            return s;
        }
    }

    /**
        Parse unicode codepoint set from given `range` using standard regex
        syntax '[...]'. The range is advanced skiping over regex set definition.
        `casefold` parameter determines if the set should be casefolded - that is
        include both lower and upper case versions for any letters in the set.
    */
    static CodepointSet parseSet(Range)(ref Range range, bool casefold=false)
    if (isInputRange!Range && is(ElementType!Range : dchar))
    {
        auto usParser = UnicodeSetParser!Range(range, casefold);
        auto set = usParser.parseSet();
        range = usParser.range;
        return set;
    }

    ///
    @safe unittest
    {
        import std.uni : unicode;
        string pat = "[a-zA-Z0-9]hello";
        auto set = unicode.parseSet(pat);
        // check some of the codepoints
        assert(set['a'] && set['A'] && set['9']);
        assert(pat == "hello");
    }

private:
    alias ucmp = comparePropertyName;

    static bool findAny(string name)
    {
        import std.internal.unicode_tables : blocks, scripts, uniProps; // generated file
        return isPrettyPropertyName(name)
            || findSetName!(uniProps.tab)(name) || findSetName!(scripts.tab)(name)
            || (ucmp(name[0 .. 2],"In") == 0 && findSetName!(blocks.tab)(name[2..$]));
    }

    static auto loadAny(Set=CodepointSet, C)(const scope C[] name) pure
    {
        import std.conv : to;
        import std.internal.unicode_tables : blocks, scripts; // generated file
        Set set;
        immutable loaded = loadProperty(name, set) || loadUnicodeSet!(scripts.tab)(name, set)
            || (name.length > 2 && ucmp(name[0 .. 2],"In") == 0
                && loadUnicodeSet!(blocks.tab)(name[2..$], set));
        if (loaded)
            return set;
        throw new Exception("No unicode set by name "~name.to!string()~" was found.");
    }

    // FIXME: re-disable once the compiler is fixed
    // Disabled to prevent the mistake of creating instances of this pseudo-struct.
    //@disable ~this();
}

@safe unittest
{
    import std.internal.unicode_tables : blocks, uniProps; // generated file
    assert(unicode("InHebrew") == asSet(blocks.Hebrew));
    assert(unicode("separator") == (asSet(uniProps.Zs) | asSet(uniProps.Zl) | asSet(uniProps.Zp)));
    assert(unicode("In-Kharoshthi") == asSet(blocks.Kharoshthi));
}

enum EMPTY_CASE_TRIE = ushort.max;// from what gen_uni uses internally

// TODO: redo the most of hangul stuff algorithmically in case of Graphemes too
// Use combined trie instead of checking for '\r' | '\n' | ccTrie,
//   or extend | '\u200D' separately

private static bool isRegionalIndicator(dchar ch) @safe pure @nogc nothrow
{
    return ch >= '\U0001F1E6' && ch <= '\U0001F1FF';
}

// Our grapheme decoder is a state machine, this is list of all possible
// states before each code point.
private enum GraphemeState
{
    Start,
    CR,
    RI,
    L,
    V,
    LVT,
    Emoji,
    EmojiZWJ,
    Prepend,
    End
}

// Message values whether end of grapheme is reached
private enum TransformRes
{
    // No, unless the source range ends here
    // (GB2 - break at end of text, unless text is empty)
    goOn,
    redo, // Run last character again with new state
    retInclude, // Yes, after the just iterated character
    retExclude // Yes, before the just iterated character
}

// The logic of the grapheme decoding is all here
// GB# means Grapheme Breaking rule number # - see Unicode standard annex #29
// Note, getting GB1 (break at start of text, unless text is empty) right
// relies on the user starting grapheme walking from beginning of the text, and
// not attempting to walk an empty text.
private immutable TransformRes
    function(ref GraphemeState, dchar) @safe pure nothrow @nogc [] graphemeTransforms =
[
    GraphemeState.Start: (ref state, ch)
    {
        // GB4. Break after controls.
        if (graphemeControlTrie[ch] || ch == '\n')
            return TransformRes.retInclude;

        with (GraphemeState) state =
            ch == '\r' ? CR :
            isRegionalIndicator(ch) ? RI :
            isHangL(ch) ? L :
            hangLV[ch] || isHangV(ch) ? V :
            hangLVT[ch] || isHangT(ch) ? LVT :
            prependTrie[ch] ? Prepend :
            xpictoTrie[ch] ? Emoji :
            End;

        // No matter what we encountered, we always include the
        // first code point in the grapheme.
        return TransformRes.goOn;
    },

    // GB3, GB4. Do not break between a CR and LF.
    // Otherwise, break after controls.
    GraphemeState.CR: (ref state, ch) => ch == '\n' ?
        TransformRes.retInclude :
        TransformRes.retExclude,

    // GB12 - GB13. Do not break within emoji flag sequences.
    // That is, do not break between regional indicator (RI) symbols if
    // there is an odd number of RI characters before the break point.
    // This state applies if one and only one RI code point has been
    // encountered.
    GraphemeState.RI: (ref state, ch)
    {
        state = GraphemeState.End;

        return isRegionalIndicator(ch) ?
            TransformRes.goOn :
            TransformRes.redo;
    },

    // GB6. Do not break Hangul syllable sequences.
    GraphemeState.L: (ref state, ch)
    {
        if (isHangL(ch))
            return TransformRes.goOn;
        else if (isHangV(ch) || hangLV[ch])
        {
            state = GraphemeState.V;
            return TransformRes.goOn;
        }
        else if (hangLVT[ch])
        {
            state = GraphemeState.LVT;
            return TransformRes.goOn;
        }

        state = GraphemeState.End;
        return TransformRes.redo;
    },

    // GB7. Do not break Hangul syllable sequences.
    GraphemeState.V: (ref state, ch)
    {
        if (isHangV(ch))
            return TransformRes.goOn;
        else if (isHangT(ch))
        {
            state = GraphemeState.LVT;
            return TransformRes.goOn;
        }

        state = GraphemeState.End;
        return TransformRes.redo;
    },

    // GB8. Do not break Hangul syllable sequences.
    GraphemeState.LVT: (ref state, ch)
    {
        if (isHangT(ch))
            return TransformRes.goOn;

        state = GraphemeState.End;
        return TransformRes.redo;
    },

    // GB11. Do not break within emoji modifier sequences or emoji
    // zwj sequences. This state applies when the last code point was
    // NOT a ZWJ.
    GraphemeState.Emoji: (ref state, ch)
    {
        if (graphemeExtendTrie[ch])
            return TransformRes.goOn;

        static assert(!graphemeExtendTrie['\u200D']);

        if (ch == '\u200D')
        {
            state = GraphemeState.EmojiZWJ;
            return TransformRes.goOn;
        }

        state = GraphemeState.End;
        // There might still be spacing marks are
        // at the end, which are not allowed in
        // middle of emoji sequences
        return TransformRes.redo;
    },

    // GB11. Do not break within emoji modifier sequences or emoji
    // zwj sequences. This state applies when the last code point was
    // a ZWJ.
    GraphemeState.EmojiZWJ: (ref state, ch)
    {
        state = GraphemeState.Emoji;
        if (xpictoTrie[ch])
            return TransformRes.goOn;
        return TransformRes.redo;
    },

    // GB9b. Do not break after Prepend characters.
    GraphemeState.Prepend: (ref state, ch)
    {
        // GB5. Break before controls.
        if (graphemeControlTrie[ch] || ch == '\r' || ch == '\n')
            return TransformRes.retExclude;

        state = GraphemeState.Start;
        return TransformRes.redo;
    },

    // GB9, GB9a. Do not break before extending characters, ZWJ
    // or SpacingMarks.
    // GB999. Otherwise, break everywhere.
    GraphemeState.End: (ref state, ch)
        => !graphemeExtendTrie[ch] && !spacingMarkTrie[ch] && ch != '\u200D' ?
            TransformRes.retExclude :
            TransformRes.goOn
];

enum GraphemeRet { none, step, value }

template genericDecodeGrapheme(GraphemeRet retType)
{   alias Ret = GraphemeRet;

    static if (retType == Ret.value)
        alias Value = Grapheme;
    else static if (retType == Ret.step)
        alias Value = size_t;
    else static if (retType == Ret.none)
        alias Value = void;

    Value genericDecodeGrapheme(Input)(ref Input range)
    {
        static if (retType == Ret.value)
            Grapheme result;
        else static if (retType == Ret.step)
            size_t result = 0;

        auto state = GraphemeState.Start;
        dchar ch;

        assert(!range.empty, "Attempting to decode grapheme from an empty " ~ Input.stringof);
    outer:
        while (!range.empty)
        {
            ch = range.front;

        rerun:
            final switch (graphemeTransforms[state](state, ch))
                with(TransformRes)
            {
            case goOn:
                static if (retType == Ret.value)
                    result ~= ch;
                else static if (retType == Ret.step)
                    result++;
                range.popFront();
                continue;

            case redo:
                goto rerun;

            case retInclude:
                static if (retType == Ret.value)
                    result ~= ch;
                else static if (retType == Ret.step)
                    result++;
                range.popFront();
                break outer;

            case retExclude:
                break outer;
            }
        }

        static if (retType != Ret.none)
            return result;
    }
}

public: // Public API continues

/++
    Computes the length of grapheme cluster starting at `index`.
    Both the resulting length and the `index` are measured
    in $(S_LINK Code unit, code units).

    Params:
        C = type that is implicitly convertible to `dchars`
        input = array of grapheme clusters
        index = starting index into `input[]`

    Returns:
        length of grapheme cluster
+/
size_t graphemeStride(C)(const scope C[] input, size_t index) @safe pure
if (is(C : dchar))
{
    auto src = input[index..$];
    auto n = src.length;
    genericDecodeGrapheme!(GraphemeRet.none)(src);
    return n - src.length;
}

///
@safe unittest
{
    assert(graphemeStride("  ", 1) == 1);
    // A + combing ring above
    string city = "A\u030Arhus";
    size_t first = graphemeStride(city, 0);
    assert(first == 3); //\u030A has 2 UTF-8 code units
    assert(city[0 .. first] == "A\u030A");
    assert(city[first..$] == "rhus");
}

@safe unittest
{
    // Ensure that graphemeStride is usable from CTFE.
    enum c1 = graphemeStride("A", 0);
    static assert(c1 == 1);

    enum c2 = graphemeStride("A\u0301", 0);
    static assert(c2 == 3); // \u0301 has 2 UTF-8 code units
}

@safe pure nothrow @nogc unittest
{
    // grinning face ~ emoji modifier fitzpatrick type-5 ~ grinning face
    assert(graphemeStride("\U0001F600\U0001f3FE\U0001F600"d, 0) == 2);
    // skier ~ female sign ~ '€'
    assert(graphemeStride("\u26F7\u2640€"d, 0) == 1);
    // skier ~ emoji modifier fitzpatrick type-5 ~ female sign ~ '€'
    assert(graphemeStride("\u26F7\U0001f3FE\u2640€"d, 0) == 2);
    // skier ~ zero-width joiner ~ female sign ~ '€'
    assert(graphemeStride("\u26F7\u200D\u2640€"d, 0) == 3);
    // skier ~ emoji modifier fitzpatrick type-5 ~ zero-width joiner
    // ~ female sign ~ '€'
    assert(graphemeStride("\u26F7\U0001f3FE\u200D\u2640€"d, 0) == 4);
    // skier ~ zero-width joiner ~ '€'
    assert(graphemeStride("\u26F7\u200D€"d, 0) == 2);
    //'€' ~ zero-width joiner ~ skier
    assert(graphemeStride("€\u200D\u26F7"d, 0) == 2);
    // Kaithi number sign ~ Devanagari digit four ~ Devanagari digit two
    assert(graphemeStride("\U000110BD\u096A\u0968"d, 0) == 2);
    // Kaithi number sign ~ null
    assert(graphemeStride("\U000110BD\0"d, 0) == 1);
}

/++
    Reads one full grapheme cluster from an
    $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of dchar `inp`.

    For examples see the $(LREF Grapheme) below.

    Note:
    This function modifies `inp` and thus `inp`
    must be an L-value.
+/
Grapheme decodeGrapheme(Input)(ref Input inp)
if (isInputRange!Input && is(immutable ElementType!Input == immutable dchar))
{
    return genericDecodeGrapheme!(GraphemeRet.value)(inp);
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    Grapheme gr;
    string s = " \u0020\u0308 ";
    gr = decodeGrapheme(s);
    assert(gr.length == 1 && gr[0] == ' ');
    gr = decodeGrapheme(s);
    assert(gr.length == 2 && equal(gr[0 .. 2], " \u0308"));
    s = "\u0300\u0308\u1100";
    assert(equal(decodeGrapheme(s)[], "\u0300\u0308"));
    assert(equal(decodeGrapheme(s)[], "\u1100"));
    s = "\u11A8\u0308\uAC01";
    assert(equal(decodeGrapheme(s)[], "\u11A8\u0308"));
    assert(equal(decodeGrapheme(s)[], "\uAC01"));

    // Two Union Jacks of the Great Britain
    s = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";
    assert(equal(decodeGrapheme(s)[], "\U0001F1EC\U0001F1E7"));
}

/++
    Reads one full grapheme cluster from an
    $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of dchar `inp`,
    but doesn't return it. Instead returns the number of code units read.
    This differs from number of code points read only if `input` is an
    autodecodable string.

    Note:
    This function modifies `inp` and thus `inp`
    must be an L-value.
+/
size_t popGrapheme(Input)(ref Input inp)
if (isInputRange!Input && is(immutable ElementType!Input == immutable dchar))
{
    static if (isAutodecodableString!Input || hasLength!Input)
    {
        // Why count each step in the decoder when you can just
        // measure the grapheme in one go?
        auto n = inp.length;
        genericDecodeGrapheme!(GraphemeRet.none)(inp);
        return n - inp.length;
    }
    else return genericDecodeGrapheme!(GraphemeRet.step)(inp);
}

///
@safe pure unittest
{
    // Two Union Jacks of the Great Britain in each
    string s = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";
    wstring ws = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";
    dstring ds = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";

    // String pop length in code units, not points.
    assert(s.popGrapheme() == 8);
    assert(ws.popGrapheme() == 4);
    assert(ds.popGrapheme() == 2);

    assert(s == "\U0001F1EC\U0001F1E7");
    assert(ws == "\U0001F1EC\U0001F1E7");
    assert(ds == "\U0001F1EC\U0001F1E7");

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    // Also works for non-random access ranges as long as the
    // character type is 32-bit.
    auto testPiece = "\r\nhello!"d.filter!(x => !x.isAlpha);
    // Windows-style line ending is two code points in a single grapheme.
    assert(testPiece.popGrapheme() == 2);
    assert(testPiece.equal("!"d));
}

// Attribute compliance test. Should be nothrow `@nogc` when
// no autodecoding needed.
@safe pure nothrow @nogc unittest
{
    import std.algorithm.iteration : filter;

    auto str = "abcdef"d;
    assert(str.popGrapheme() == 1);

    // also test with non-random access
    auto filtered = "abcdef"d.filter!(x => x%2);
    assert(filtered.popGrapheme() == 1);
}

/++
    $(P Iterate a string by $(LREF Grapheme).)

    $(P Useful for doing string manipulation that needs to be aware
    of graphemes.)

    See_Also:
        $(LREF byCodePoint)
+/
auto byGrapheme(Range)(Range range)
if (isInputRange!Range && is(immutable ElementType!Range == immutable dchar))
{
    // TODO: Bidirectional access
    static struct Result(R)
    {
        private R _range;
        private Grapheme _front;

        bool empty() @property
        {
            return _front.length == 0;
        }

        Grapheme front() @property
        {
            return _front;
        }

        void popFront()
        {
            _front = _range.empty ? Grapheme.init : _range.decodeGrapheme();
        }

        static if (isForwardRange!R)
        {
            Result save() @property
            {
                return Result(_range.save, _front);
            }
        }
    }

    auto result = Result!(Range)(range);
    result.popFront();
    return result;
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : walkLength;
    import std.range : take, drop;
    auto text = "noe\u0308l"; // noël using e + combining diaeresis
    assert(text.walkLength == 5); // 5 code points

    auto gText = text.byGrapheme;
    assert(gText.walkLength == 4); // 4 graphemes

    assert(gText.take(3).equal("noe\u0308".byGrapheme));
    assert(gText.drop(3).equal("l".byGrapheme));
}

// For testing non-forward-range input ranges
version (StdUnittest)
private static @safe struct InputRangeString
{
    private string s;

    bool empty() @property { return s.empty; }
    dchar front() @property { return s.front; }
    void popFront() { s.popFront(); }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.array : array;
    import std.range : retro;
    import std.range.primitives : walkLength;
    assert("".byGrapheme.walkLength == 0);

    auto reverse = "le\u0308on";
    assert(reverse.walkLength == 5);

    auto gReverse = reverse.byGrapheme;
    assert(gReverse.walkLength == 4);

    static foreach (text; AliasSeq!("noe\u0308l"c, "noe\u0308l"w, "noe\u0308l"d))
    {{
        assert(text.walkLength == 5);
        static assert(isForwardRange!(typeof(text)));

        auto gText = text.byGrapheme;
        static assert(isForwardRange!(typeof(gText)));
        assert(gText.walkLength == 4);
        assert(gText.array.retro.equal(gReverse));
    }}

    auto nonForwardRange = InputRangeString("noe\u0308l").byGrapheme;
    static assert(!isForwardRange!(typeof(nonForwardRange)));
    assert(nonForwardRange.walkLength == 4);
}

// Issue 23474
@safe pure unittest
{
    import std.range.primitives : walkLength;
    assert(byGrapheme("\r\u0308").walkLength == 2);
}

/++
    $(P Lazily transform a range of $(LREF Grapheme)s to a range of code points.)

    $(P Useful for converting the result to a string after doing operations
    on graphemes.)

    $(P If passed in a range of code points, returns a range with equivalent capabilities.)
+/
auto byCodePoint(Range)(Range range)
if (isInputRange!Range && is(immutable ElementType!Range == immutable Grapheme))
{
    // TODO: Propagate bidirectional access
    static struct Result
    {
        private Range _range;
        private size_t i = 0;

        bool empty() @property
        {
            return _range.empty;
        }

        dchar front() @property
        {
            return _range.front[i];
        }

        void popFront()
        {
            ++i;

            if (i >= _range.front.length)
            {
                _range.popFront();
                i = 0;
            }
        }

        static if (isForwardRange!Range)
        {
            Result save() @property
            {
                return Result(_range.save, i);
            }
        }
    }

    return Result(range);
}

/// Ditto
auto byCodePoint(Range)(Range range)
if (isInputRange!Range && is(immutable ElementType!Range == immutable dchar))
{
    import std.range.primitives : isBidirectionalRange, popBack;
    import std.traits : isNarrowString;
    static if (isNarrowString!Range)
    {
        static struct Result
        {
            private Range _range;
            @property bool empty() { return _range.empty; }
            @property dchar front(){ return _range.front; }
            void popFront(){ _range.popFront; }
            @property auto save() { return Result(_range.save); }
            @property dchar back(){ return _range.back; }
            void popBack(){ _range.popBack; }
        }
        static assert(isBidirectionalRange!(Result));
        return Result(range);
    }
    else
        return range;
}

///
@safe unittest
{
    import std.array : array;
    import std.conv : text;
    import std.range : retro;

    string s = "noe\u0308l"; // noël

    // reverse it and convert the result to a string
    string reverse = s.byGrapheme
        .array
        .retro
        .byCodePoint
        .text;

    assert(reverse == "le\u0308on"); // lëon
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : walkLength;
    import std.range : retro;
    assert("".byGrapheme.byCodePoint.equal(""));

    string text = "noe\u0308l";
    static assert(!__traits(compiles, "noe\u0308l".byCodePoint.length));

    auto gText = InputRangeString(text).byGrapheme;
    static assert(!isForwardRange!(typeof(gText)));

    auto cpText = gText.byCodePoint;
    static assert(!isForwardRange!(typeof(cpText)));

    assert(cpText.walkLength == text.walkLength);

    auto plainCp = text.byCodePoint;
    static assert(isForwardRange!(typeof(plainCp)));
    assert(equal(plainCp, text));
    assert(equal(retro(plainCp.save), retro(text.save)));
    // Check that we still have length for dstring
    assert("абвгд"d.byCodePoint.length == 5);
}

/++
    $(P A structure designed to effectively pack $(CHARACTERS)
    of a $(CLUSTER).
    )

    $(P `Grapheme` has value semantics so 2 copies of a `Grapheme`
    always refer to distinct objects. In most actual scenarios a `Grapheme`
    fits on the stack and avoids memory allocation overhead for all but quite
    long clusters.
    )

    See_Also: $(LREF decodeGrapheme), $(LREF graphemeStride)
+/
@safe struct Grapheme
{
    import std.exception : enforce;
    import std.traits : isDynamicArray;

public:
    /// Ctor
    this(C)(const scope C[] chars...)
    if (is(C : dchar))
    {
        this ~= chars;
    }

    ///ditto
    this(Input)(Input seq)
    if (!isDynamicArray!Input
        && isInputRange!Input && is(ElementType!Input : dchar))
    {
        this ~= seq;
    }

    /// Gets a $(CODEPOINT) at the given index in this cluster.
    dchar opIndex(size_t index) const @nogc nothrow pure @trusted
    {
        assert(index < length);
        return read24(isBig ? ptr_ : small_.ptr, index);
    }

    /++
        Writes a $(CODEPOINT) `ch` at given index in this cluster.

        Warning:
        Use of this facility may invalidate grapheme cluster,
        see also $(LREF Grapheme.valid).
    +/
    void opIndexAssign(dchar ch, size_t index) @nogc nothrow pure @trusted
    {
        assert(index < length);
        write24(isBig ? ptr_ : small_.ptr, ch, index);
    }

    ///
    @safe unittest
    {
        auto g = Grapheme("A\u0302");
        assert(g[0] == 'A');
        assert(g.valid);
        g[1] = '~'; // ASCII tilda is not a combining mark
        assert(g[1] == '~');
        assert(!g.valid);
    }

    /++
        Random-access range over Grapheme's $(CHARACTERS).

        Warning: Invalidates when this Grapheme leaves the scope,
        attempts to use it then would lead to memory corruption.
    +/
    SliceOverIndexed!Grapheme opSlice(size_t a, size_t b) @nogc nothrow pure return
    {
        return sliceOverIndexed(a, b, &this);
    }

    /// ditto
    SliceOverIndexed!Grapheme opSlice() @nogc nothrow pure return
    {
        return sliceOverIndexed(0, length, &this);
    }

    /// Grapheme cluster length in $(CODEPOINTS).
    @property size_t length() const @nogc nothrow pure
    {
        return isBig ? len_ : slen_ & 0x7F;
    }

    /++
        Append $(CHARACTER) `ch` to this grapheme.
        Warning:
        Use of this facility may invalidate grapheme cluster,
        see also `valid`.

        See_Also: $(LREF Grapheme.valid)
    +/
    ref opOpAssign(string op)(dchar ch) @trusted
    {
        static if (op == "~")
        {
            import std.internal.memory : enforceRealloc;
            if (!isBig)
            {
                if (slen_ == small_cap)
                    convertToBig();// & fallthrough to "big" branch
                else
                {
                    write24(small_.ptr, ch, smallLength);
                    slen_++;
                    return this;
                }
            }

            assert(isBig);
            if (len_ == cap_)
            {
                import core.checkedint : addu, mulu;
                bool overflow;
                cap_ = addu(cap_, grow, overflow);
                auto nelems = mulu(3, addu(cap_, 1, overflow), overflow);
                if (overflow) assert(0);
                ptr_ = cast(ubyte*) enforceRealloc(ptr_, nelems);
            }
            write24(ptr_, ch, len_++);
            return this;
        }
        else
            static assert(false, "No operation "~op~" defined for Grapheme");
    }

    ///
    @safe unittest
    {
        import std.algorithm.comparison : equal;
        auto g = Grapheme("A");
        assert(g.valid);
        g ~= '\u0301';
        assert(g[].equal("A\u0301"));
        assert(g.valid);
        g ~= "B";
        // not a valid grapheme cluster anymore
        assert(!g.valid);
        // still could be useful though
        assert(g[].equal("A\u0301B"));
    }

    /// Append all $(CHARACTERS) from the input range `inp` to this Grapheme.
    ref opOpAssign(string op, Input)(scope Input inp)
    if (isInputRange!Input && is(ElementType!Input : dchar))
    {
        static if (op == "~")
        {
            foreach (dchar ch; inp)
                this ~= ch;
            return this;
        }
        else
            static assert(false, "No operation "~op~" defined for Grapheme");
    }

    // This is not a good `opEquals`, but formerly the automatically generated
    // opEquals was used, which was inferred `@safe` because of bugzilla 20655:
    // https://issues.dlang.org/show_bug.cgi?id=20655
    // This `@trusted opEquals` is only here to prevent breakage.
    bool opEquals(R)(const auto ref R other) const @trusted
    {
        return this.tupleof == other.tupleof;
    }

    // Define a default toHash to allow AA usage
    size_t toHash() const @trusted
    {
        return hashOf(slen_, hashOf(small_));
    }

    /++
        True if this object contains valid extended grapheme cluster.
        Decoding primitives of this module always return a valid `Grapheme`.

        Appending to and direct manipulation of grapheme's $(CHARACTERS) may
        render it no longer valid. Certain applications may chose to use
        Grapheme as a "small string" of any $(CODEPOINTS) and ignore this property
        entirely.
    +/
    @property bool valid()() /*const*/
    {
        auto r = this[];
        genericDecodeGrapheme!(GraphemeRet.none)(r);
        return r.length == 0;
    }

    this(this) @nogc nothrow pure @trusted
    {
        import std.internal.memory : enforceMalloc;
        if (isBig)
        {// dup it
            import core.checkedint : addu, mulu;
            bool overflow;
            auto raw_cap = mulu(3, addu(cap_, 1, overflow), overflow);
            if (overflow) assert(0);

            auto p = cast(ubyte*) enforceMalloc(raw_cap);
            p[0 .. raw_cap] = ptr_[0 .. raw_cap];
            ptr_ = p;
        }
    }

    ~this() @nogc nothrow pure @trusted
    {
        import core.memory : pureFree;
        if (isBig)
        {
            pureFree(ptr_);
        }
    }


private:
    enum small_bytes = ((ubyte*).sizeof+3*size_t.sizeof-1);
    // "out of the blue" grow rate, needs testing
    // (though graphemes are typically small < 9)
    enum grow = 20;
    enum small_cap = small_bytes/3;
    enum small_flag = 0x80, small_mask = 0x7F;
    // 16 bytes in 32bits, should be enough for the majority of cases
    union
    {
        struct
        {
            ubyte* ptr_;
            size_t cap_;
            size_t len_;
            size_t padding_;
        }
        struct
        {
            ubyte[small_bytes] small_;
            ubyte slen_;
        }
    }

    void convertToBig() @nogc nothrow pure @trusted
    {
        import std.internal.memory : enforceMalloc;
        static assert(grow.max / 3 - 1 >= grow);
        enum nbytes = 3 * (grow + 1);
        size_t k = smallLength;
        ubyte* p = cast(ubyte*) enforceMalloc(nbytes);
        for (int i=0; i<k; i++)
            write24(p, read24(small_.ptr, i), i);
        // now we can overwrite small array data
        ptr_ = p;
        len_ = slen_;
        assert(grow > len_);
        cap_ = grow;
        setBig();
    }

    void setBig() @nogc nothrow pure { slen_ |= small_flag; }

    @property size_t smallLength() const @nogc nothrow pure
    {
        return slen_ & small_mask;
    }
    @property ubyte isBig() const @nogc nothrow pure
    {
        return slen_ & small_flag;
    }
}

static assert(Grapheme.sizeof == size_t.sizeof*4);


@safe pure /*nothrow @nogc*/ unittest // TODO: string .front is GC and throw
{
    import std.algorithm.comparison : equal;
    Grapheme[3] data = [Grapheme("Ю"), Grapheme("У"), Grapheme("З")];
    assert(byGrapheme("ЮУЗ").equal(data[]));
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;
    import std.range : isRandomAccessRange;

    string bold = "ku\u0308hn";

    // note that decodeGrapheme takes parameter by ref
    auto first = decodeGrapheme(bold);

    assert(first.length == 1);
    assert(first[0] == 'k');

    // the next grapheme is 2 characters long
    auto wideOne = decodeGrapheme(bold);
    // slicing a grapheme yields a random-access range of dchar
    assert(wideOne[].equal("u\u0308"));
    assert(wideOne.length == 2);
    static assert(isRandomAccessRange!(typeof(wideOne[])));

    // all of the usual range manipulation is possible
    assert(wideOne[].filter!isMark().equal("\u0308"));

    auto g = Grapheme("A");
    assert(g.valid);
    g ~= '\u0301';
    assert(g[].equal("A\u0301"));
    assert(g.valid);
    g ~= "B";
    // not a valid grapheme cluster anymore
    assert(!g.valid);
    // still could be useful though
    assert(g[].equal("A\u0301B"));
}

@safe unittest
{
    auto g = Grapheme("A\u0302");
    assert(g[0] == 'A');
    assert(g.valid);
    g[1] = '~'; // ASCII tilda is not a combining mark
    assert(g[1] == '~');
    assert(!g.valid);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.conv : text;
    import std.range : iota;

    // not valid clusters (but it just a test)
    auto g  = Grapheme('a', 'b', 'c', 'd', 'e');
    assert(g[0] == 'a');
    assert(g[1] == 'b');
    assert(g[2] == 'c');
    assert(g[3] == 'd');
    assert(g[4] == 'e');
    g[3] = 'Й';
    assert(g[2] == 'c');
    assert(g[3] == 'Й', text(g[3], " vs ", 'Й'));
    assert(g[4] == 'e');
    assert(!g.valid);

    g ~= 'ц';
    g ~= '~';
    assert(g[0] == 'a');
    assert(g[1] == 'b');
    assert(g[2] == 'c');
    assert(g[3] == 'Й');
    assert(g[4] == 'e');
    assert(g[5] == 'ц');
    assert(g[6] == '~');
    assert(!g.valid);

    Grapheme copy = g;
    copy[0] = 'X';
    copy[1] = '-';
    assert(g[0] == 'a' && copy[0] == 'X');
    assert(g[1] == 'b' && copy[1] == '-');
    assert(equal(g[2 .. g.length], copy[2 .. copy.length]));
    copy = Grapheme("АБВГДЕЁЖЗИКЛМ");
    assert(equal(copy[0 .. 8], "АБВГДЕЁЖ"), text(copy[0 .. 8]));
    copy ~= "xyz";
    assert(equal(copy[13 .. 15], "xy"), text(copy[13 .. 15]));
    assert(!copy.valid);

    Grapheme h;
    foreach (dchar v; iota(cast(int)'A', cast(int)'Z'+1).map!"cast(dchar)a"())
        h ~= v;
    assert(equal(h[], iota(cast(int)'A', cast(int)'Z'+1)));
}

// ensure Grapheme can be used as an AA key.
@safe unittest
{
    int[Grapheme] aa;
}

/++
    $(P Does basic case-insensitive comparison of `r1` and `r2`.
    This function uses simpler comparison rule thus achieving better performance
    than $(LREF icmp). However keep in mind the warning below.)

    Params:
        r1 = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of characters
        r2 = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of characters

    Returns:
        An `int` that is 0 if the strings match,
        &lt;0 if `r1` is lexicographically "less" than `r2`,
        &gt;0 if `r1` is lexicographically "greater" than `r2`

    Warning:
    This function only handles 1:1 $(CODEPOINT) mapping
    and thus is not sufficient for certain alphabets
    like German, Greek and few others.

    See_Also:
        $(LREF icmp)
        $(REF cmp, std,algorithm,comparison)
+/
int sicmp(S1, S2)(scope S1 r1, scope S2 r2)
if (isInputRange!S1 && isSomeChar!(ElementEncodingType!S1)
    && isInputRange!S2 && isSomeChar!(ElementEncodingType!S2))
{
    import std.internal.unicode_tables : sTable = simpleCaseTable; // generated file
    import std.range.primitives : isInfinite;
    import std.utf : decodeFront;
    import std.traits : isDynamicArray;
    import std.typecons : Yes;
    static import std.ascii;

    static if ((isDynamicArray!S1 || isRandomAccessRange!S1)
        && (isDynamicArray!S2 || isRandomAccessRange!S2)
        && !(isInfinite!S1 && isInfinite!S2)
        && __traits(compiles,
            {
                size_t s = size_t.sizeof / 2;
                r1 = r1[s .. $];
                r2 = r2[s .. $];
            }))
    {{
        // ASCII optimization for dynamic arrays & similar.
        size_t i = 0;
        static if (isInfinite!S1)
            immutable end = r2.length;
        else static if (isInfinite!S2)
            immutable end = r1.length;
        else
            immutable end = r1.length > r2.length ? r2.length : r1.length;
        for (; i < end; ++i)
        {
            auto lhs = r1[i];
            auto rhs = r2[i];
            if ((lhs | rhs) >= 0x80) goto NonAsciiPath;
            if (lhs == rhs) continue;
            auto lowDiff = std.ascii.toLower(lhs) - std.ascii.toLower(rhs);
            if (lowDiff) return lowDiff;
        }
        static if (isInfinite!S1)
            return 1;
        else static if (isInfinite!S2)
            return -1;
        else
            return (r1.length > r2.length) - (r2.length > r1.length);

    NonAsciiPath:
        r1 = r1[i .. $];
        r2 = r2[i .. $];
        // Fall through to standard case.
    }}

    while (!r1.empty)
    {
        immutable lhs = decodeFront!(Yes.useReplacementDchar)(r1);
        if (r2.empty)
            return 1;
        immutable rhs = decodeFront!(Yes.useReplacementDchar)(r2);
        int diff = lhs - rhs;
        if (!diff)
            continue;
        if ((lhs | rhs) < 0x80)
        {
            immutable d = std.ascii.toLower(lhs) - std.ascii.toLower(rhs);
            if (!d) continue;
            return d;
        }
        size_t idx = simpleCaseTrie[lhs];
        size_t idx2 = simpleCaseTrie[rhs];
        // simpleCaseTrie is packed index table
        if (idx != EMPTY_CASE_TRIE)
        {
            if (idx2 != EMPTY_CASE_TRIE)
            {// both cased chars
                // adjust idx --> start of bucket
                idx = idx - sTable(idx).n;
                idx2 = idx2 - sTable(idx2).n;
                if (idx == idx2)// one bucket, equivalent chars
                    continue;
                else//  not the same bucket
                    diff = sTable(idx).ch - sTable(idx2).ch;
            }
            else
                diff = sTable(idx - sTable(idx).n).ch - rhs;
        }
        else if (idx2 != EMPTY_CASE_TRIE)
        {
            diff = lhs - sTable(idx2 - sTable(idx2).n).ch;
        }
        // one of chars is not cased at all
        return diff;
    }
    return int(r2.empty) - 1;
}

///
@safe @nogc pure nothrow unittest
{
    assert(sicmp("Август", "авгусТ") == 0);
    // Greek also works as long as there is no 1:M mapping in sight
    assert(sicmp("ΌΎ", "όύ") == 0);
    // things like the following won't get matched as equal
    // Greek small letter iota with dialytika and tonos
    assert(sicmp("ΐ", "\u03B9\u0308\u0301") != 0);

    // while icmp has no problem with that
    assert(icmp("ΐ", "\u03B9\u0308\u0301") == 0);
    assert(icmp("ΌΎ", "όύ") == 0);
}

// overloads for the most common cases to reduce compile time
@safe @nogc pure nothrow
{
    int sicmp(scope const(char)[] str1, scope const(char)[] str2)
    { return sicmp!(const(char)[], const(char)[])(str1, str2); }

    int sicmp(scope const(wchar)[] str1, scope const(wchar)[] str2)
    { return sicmp!(const(wchar)[], const(wchar)[])(str1, str2); }

    int sicmp(scope const(dchar)[] str1, scope const(dchar)[] str2)
    { return sicmp!(const(dchar)[], const(dchar)[])(str1, str2); }
}

private int fullCasedCmp(Range)(dchar lhs, dchar rhs, ref Range rtail)
{
    import std.algorithm.searching : skipOver;
    import std.internal.unicode_tables : fullCaseTable; // generated file
    alias fTable = fullCaseTable;
    size_t idx = fullCaseTrie[lhs];
    // fullCaseTrie is packed index table
    if (idx == EMPTY_CASE_TRIE)
        return lhs;
    immutable start = idx - fTable(idx).n;
    immutable end = fTable(idx).size + start;
    assert(fTable(start).entry_len == 1);
    for (idx=start; idx<end; idx++)
    {
        const entryLen = fTable(idx).entry_len;
        if (entryLen == 1)
        {
            if (fTable(idx).seq[0] == rhs)
            {
                return 0;
            }
        }
        else
        {// OK it's a long chunk, like 'ss' for German
            dchar[3] arr = fTable(idx).seq;
            const dchar[] seq = arr[0 .. entryLen];
            if (rhs == seq[0]
                && rtail.skipOver(seq[1..$]))
            {
                // note that this path modifies rtail
                // iff we managed to get there
                return 0;
            }
        }
    }
    return fTable(start).seq[0]; // new remapped character for accurate diffs
}

/++
    Does case insensitive comparison of `r1` and `r2`.
    Follows the rules of full case-folding mapping.
    This includes matching as equal german ß with "ss" and
    other 1:M $(CODEPOINT) mappings unlike $(LREF sicmp).
    The cost of `icmp` being pedantically correct is
    slightly worse performance.

    Params:
        r1 = a forward range of characters
        r2 = a forward range of characters

    Returns:
        An `int` that is 0 if the strings match,
        &lt;0 if `str1` is lexicographically "less" than `str2`,
        &gt;0 if `str1` is lexicographically "greater" than `str2`

    See_Also:
        $(LREF sicmp)
        $(REF cmp, std,algorithm,comparison)
+/
int icmp(S1, S2)(S1 r1, S2 r2)
if (isForwardRange!S1 && isSomeChar!(ElementEncodingType!S1)
    && isForwardRange!S2 && isSomeChar!(ElementEncodingType!S2))
{
    import std.range.primitives : isInfinite;
    import std.traits : isDynamicArray;
    import std.utf : byDchar;
    static import std.ascii;

    static if ((isDynamicArray!S1 || isRandomAccessRange!S1)
        && (isDynamicArray!S2 || isRandomAccessRange!S2)
        && !(isInfinite!S1 && isInfinite!S2)
        && __traits(compiles,
            {
                size_t s = size_t.max / 2;
                r1 = r1[s .. $];
                r2 = r2[s .. $];
            }))
    {{
        // ASCII optimization for dynamic arrays & similar.
        size_t i = 0;
        static if (isInfinite!S1)
            immutable end = r2.length;
        else static if (isInfinite!S2)
            immutable end = r1.length;
        else
            immutable end = r1.length > r2.length ? r2.length : r1.length;
        for (; i < end; ++i)
        {
            auto lhs = r1[i];
            auto rhs = r2[i];
            if ((lhs | rhs) >= 0x80) goto NonAsciiPath;
            if (lhs == rhs) continue;
            auto lowDiff = std.ascii.toLower(lhs) - std.ascii.toLower(rhs);
            if (lowDiff) return lowDiff;
        }
        static if (isInfinite!S1)
            return 1;
        else static if (isInfinite!S2)
            return -1;
        else
            return (r1.length > r2.length) - (r2.length > r1.length);

    NonAsciiPath:
        r1 = r1[i .. $];
        r2 = r2[i .. $];
        // Fall through to standard case.
    }}

    auto str1 = r1.byDchar;
    auto str2 = r2.byDchar;

    for (;;)
    {
        if (str1.empty)
            return str2.empty ? 0 : -1;
        immutable lhs = str1.front;
        if (str2.empty)
            return 1;
        immutable rhs = str2.front;
        str1.popFront();
        str2.popFront();
        if (!(lhs - rhs))
            continue;
        // first try to match lhs to <rhs,right-tail> sequence
        immutable cmpLR = fullCasedCmp(lhs, rhs, str2);
        if (!cmpLR)
            continue;
        // then rhs to <lhs,left-tail> sequence
        immutable cmpRL = fullCasedCmp(rhs, lhs, str1);
        if (!cmpRL)
            continue;
        // cmpXX contain remapped codepoints
        // to obtain stable ordering of icmp
        return cmpLR - cmpRL;
    }
}

///
@safe @nogc pure nothrow unittest
{
    assert(icmp("Rußland", "Russland") == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9", "\u1F61\u03B9 -> ᾲ") == 0);
}

/**
 * By using $(REF byUTF, std,utf) and its aliases, GC allocations via auto-decoding
 * and thrown exceptions can be avoided, making `icmp` `@safe @nogc nothrow pure`.
 */
@safe @nogc nothrow pure unittest
{
    import std.utf : byDchar;

    assert(icmp("Rußland".byDchar, "Russland".byDchar) == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9".byDchar, "\u1F61\u03B9 -> ᾲ".byDchar) == 0);
}

// test different character types
@safe unittest
{
    assert(icmp("Rußland", "Russland") == 0);
    assert(icmp("Rußland"w, "Russland") == 0);
    assert(icmp("Rußland", "Russland"w) == 0);
    assert(icmp("Rußland"w, "Russland"w) == 0);
    assert(icmp("Rußland"d, "Russland"w) == 0);
    assert(icmp("Rußland"w, "Russland"d) == 0);
}

// overloads for the most common cases to reduce compile time
@safe @nogc pure nothrow
{
    int icmp(const(char)[] str1, const(char)[] str2)
    { return icmp!(const(char)[], const(char)[])(str1, str2); }
    int icmp(const(wchar)[] str1, const(wchar)[] str2)
    { return icmp!(const(wchar)[], const(wchar)[])(str1, str2); }
    int icmp(const(dchar)[] str1, const(dchar)[] str2)
    { return icmp!(const(dchar)[], const(dchar)[])(str1, str2); }
}

@safe unittest
{
    import std.algorithm.sorting : sort;
    import std.conv : to;
    import std.exception : assertCTFEable;
    assertCTFEable!(
    {
    static foreach (cfunc; AliasSeq!(icmp, sicmp))
    {{
        static foreach (S1; AliasSeq!(string, wstring, dstring))
        static foreach (S2; AliasSeq!(string, wstring, dstring))
        {
            assert(cfunc("".to!S1(), "".to!S2()) == 0);
            assert(cfunc("A".to!S1(), "".to!S2()) > 0);
            assert(cfunc("".to!S1(), "0".to!S2()) < 0);
            assert(cfunc("abc".to!S1(), "abc".to!S2()) == 0);
            assert(cfunc("abcd".to!S1(), "abc".to!S2()) > 0);
            assert(cfunc("abc".to!S1(), "abcd".to!S2()) < 0);
            assert(cfunc("Abc".to!S1(), "aBc".to!S2()) == 0);
            assert(cfunc("авГуст".to!S1(), "АВгУСТ".to!S2()) == 0);
            // Check example:
            assert(cfunc("Август".to!S1(), "авгусТ".to!S2()) == 0);
            assert(cfunc("ΌΎ".to!S1(), "όύ".to!S2()) == 0);
        }
        // check that the order is properly agnostic to the case
        auto strs = [ "Apple", "ORANGE",  "orAcle", "amp", "banana"];
        sort!((a,b) => cfunc(a,b) < 0)(strs);
        assert(strs == ["amp", "Apple",  "banana", "orAcle", "ORANGE"]);
    }}
    assert(icmp("ßb", "ssa") > 0);
    // Check example:
    assert(icmp("Russland", "Rußland") == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9", "\u1F61\u03B9 -> ᾲ") == 0);
    assert(icmp("ΐ"w, "\u03B9\u0308\u0301") == 0);
    assert(sicmp("ΐ", "\u03B9\u0308\u0301") != 0);
    // https://issues.dlang.org/show_bug.cgi?id=11057
    assert( icmp("K", "L") < 0 );
    });
}

// https://issues.dlang.org/show_bug.cgi?id=17372
@safe pure unittest
{
    import std.algorithm.iteration : joiner, map;
    import std.algorithm.sorting : sort;
    import std.array : array;
    auto a = [["foo", "bar"], ["baz"]].map!(line => line.joiner(" ")).array.sort!((a, b) => icmp(a, b) < 0);
}

// This is package(std) for the moment to be used as a support tool for std.regex
// It needs a better API
/*
    Return a range of all $(CODEPOINTS) that casefold to
    and from this `ch`.
*/
package(std) auto simpleCaseFoldings(dchar ch) @safe
{
    import std.internal.unicode_tables : simpleCaseTable; // generated file
    alias sTable = simpleCaseTable;
    static struct Range
    {
    @safe pure nothrow:
        uint idx; //if == uint.max, then read c.
        union
        {
            dchar c; // == 0 - empty range
            uint len;
        }
        @property bool isSmall() const { return idx == uint.max; }

        this(dchar ch)
        {
            idx = uint.max;
            c = ch;
        }

        this(uint start, uint size)
        {
            idx = start;
            len = size;
        }

        @property dchar front() const
        {
            assert(!empty);
            if (isSmall)
            {
                return c;
            }
            auto ch = sTable(idx).ch;
            return ch;
        }

        @property bool empty() const
        {
            if (isSmall)
            {
                return c == 0;
            }
            return len == 0;
        }

        @property size_t length() const
        {
            if (isSmall)
            {
                return c == 0 ? 0 : 1;
            }
            return len;
        }

        void popFront()
        {
            if (isSmall)
                c = 0;
            else
            {
                idx++;
                len--;
            }
        }
    }
    immutable idx = simpleCaseTrie[ch];
    if (idx == EMPTY_CASE_TRIE)
        return Range(ch);
    auto entry = sTable(idx);
    immutable start = idx - entry.n;
    return Range(start, entry.size);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.searching : canFind;
    import std.array : array;
    import std.exception : assertCTFEable;
    assertCTFEable!((){
        auto r = simpleCaseFoldings('Э').array;
        assert(r.length == 2);
        assert(r.canFind('э') && r.canFind('Э'));
        auto sr = simpleCaseFoldings('~');
        assert(sr.equal("~"));
        //A with ring above - casefolds to the same bucket as Angstrom sign
        sr = simpleCaseFoldings('Å');
        assert(sr.length == 3);
        assert(sr.canFind('å') && sr.canFind('Å') && sr.canFind('\u212B'));
    });
}

/++
    $(P Returns the $(S_LINK Combining class, combining class) of `ch`.)
+/
ubyte combiningClass(dchar ch) @safe pure nothrow @nogc
{
    return combiningClassTrie[ch];
}

///
@safe unittest
{
    // shorten the code
    alias CC = combiningClass;

    // combining tilda
    assert(CC('\u0303') == 230);
    // combining ring below
    assert(CC('\u0325') == 220);
    // the simple consequence is that  "tilda" should be
    // placed after a "ring below" in a sequence
}

@safe pure nothrow @nogc unittest
{
    foreach (ch; 0 .. 0x80)
        assert(combiningClass(ch) == 0);
    assert(combiningClass('\u05BD') == 22);
    assert(combiningClass('\u0300') == 230);
    assert(combiningClass('\u0317') == 220);
    assert(combiningClass('\u1939') == 222);
}

/// Unicode character decomposition type.
enum UnicodeDecomposition {
    /// Canonical decomposition. The result is canonically equivalent sequence.
    Canonical,
    /**
         Compatibility decomposition. The result is compatibility equivalent sequence.
         Note: Compatibility decomposition is a $(B lossy) conversion,
         typically suitable only for fuzzy matching and internal processing.
    */
    Compatibility
}

/**
    Shorthand aliases for character decomposition type, passed as a
    template parameter to $(LREF decompose).
*/
enum {
    Canonical = UnicodeDecomposition.Canonical,
    Compatibility = UnicodeDecomposition.Compatibility
}

/++
    Try to canonically compose 2 $(CHARACTERS).
    Returns the composed $(CHARACTER) if they do compose and dchar.init otherwise.

    The assumption is that `first` comes before `second` in the original text,
    usually meaning that the first is a starter.

    Note: Hangul syllables are not covered by this function.
    See `composeJamo` below.
+/
public dchar compose(dchar first, dchar second) pure nothrow @safe
{
    import std.algorithm.iteration : map;
    import std.internal.unicode_comp : compositionTable, composeCntShift, composeIdxMask;
    import std.range : assumeSorted, stride;
    immutable packed = compositionJumpTrie[first];
    if (packed == ushort.max)
        return dchar.init;
    // unpack offset and length
    immutable idx = packed & composeIdxMask, cnt = packed >> composeCntShift;
    // TODO: optimize this micro binary search (no more then 4-5 steps)
    auto r = compositionTable.stride(2)[idx .. idx+cnt].assumeSorted();
    immutable target = r.lowerBound(second).length;
    if (target == cnt)
        return dchar.init;
    immutable entry = compositionTable[(idx+target)*2];
    if (entry != second)
        return dchar.init;
    return compositionTable[(idx+target)*2 + 1];
}

///
@safe unittest
{
    assert(compose('A','\u0308') == '\u00C4');
    assert(compose('A', 'B') == dchar.init);
    assert(compose('C', '\u0301') == '\u0106');
    // note that the starter is the first one
    // thus the following doesn't compose
    assert(compose('\u0308', 'A') == dchar.init);
}

/++
    Returns a full $(S_LINK Canonical decomposition, Canonical)
    (by default) or $(S_LINK Compatibility decomposition, Compatibility)
    decomposition of $(CHARACTER) `ch`.
    If no decomposition is available returns a $(LREF Grapheme)
    with the `ch` itself.

    Note:
    This function also decomposes hangul syllables
    as prescribed by the standard.

    See_Also: $(LREF decomposeHangul) for a restricted version
    that takes into account only hangul syllables  but
    no other decompositions.
+/
public Grapheme decompose(UnicodeDecomposition decompType=Canonical)(dchar ch) @safe
{
    import std.algorithm.searching : until;
    import std.internal.unicode_decomp : decompCompatTable, decompCanonTable;
    static if (decompType == Canonical)
    {
        alias table = decompCanonTable;
        alias mapping = canonMappingTrie;
    }
    else static if (decompType == Compatibility)
    {
        alias table = decompCompatTable;
        alias mapping = compatMappingTrie;
    }
    immutable idx = mapping[ch];
    if (!idx) // not found, check hangul arithmetic decomposition
        return decomposeHangul(ch);
    auto decomp = table[idx..$].until(0);
    return Grapheme(decomp);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(compose('A','\u0308') == '\u00C4');
    assert(compose('A', 'B') == dchar.init);
    assert(compose('C', '\u0301') == '\u0106');
    // note that the starter is the first one
    // thus the following doesn't compose
    assert(compose('\u0308', 'A') == dchar.init);

    assert(decompose('Ĉ')[].equal("C\u0302"));
    assert(decompose('D')[].equal("D"));
    assert(decompose('\uD4DC')[].equal("\u1111\u1171\u11B7"));
    assert(decompose!Compatibility('¹')[].equal("1"));
}

//----------------------------------------------------------------------------
// Hangul specific composition/decomposition
enum jamoSBase = 0xAC00;
enum jamoLBase = 0x1100;
enum jamoVBase = 0x1161;
enum jamoTBase = 0x11A7;
enum jamoLCount = 19, jamoVCount = 21, jamoTCount = 28;
enum jamoNCount = jamoVCount * jamoTCount;
enum jamoSCount = jamoLCount * jamoNCount;

// Tests if `ch` is a Hangul leading consonant jamo.
bool isJamoL(dchar ch) pure nothrow @nogc @safe
{
    // first cmp rejects ~ 1M code points above leading jamo range
    return ch < jamoLBase+jamoLCount && ch >= jamoLBase;
}

// Tests if `ch` is a Hangul vowel jamo.
bool isJamoT(dchar ch) pure nothrow @nogc @safe
{
    // first cmp rejects ~ 1M code points above trailing jamo range
    // Note: ch == jamoTBase doesn't indicate trailing jamo (TIndex must be > 0)
    return ch < jamoTBase+jamoTCount && ch > jamoTBase;
}

// Tests if `ch` is a Hangul trailnig consonant jamo.
bool isJamoV(dchar ch) pure nothrow @nogc @safe
{
    // first cmp rejects ~ 1M code points above vowel range
    return  ch < jamoVBase+jamoVCount && ch >= jamoVBase;
}

int hangulSyllableIndex(dchar ch) pure nothrow @nogc @safe
{
    int idxS = cast(int) ch - jamoSBase;
    return idxS >= 0 && idxS < jamoSCount ? idxS : -1;
}

// internal helper: compose hangul syllables leaving dchar.init in holes
void hangulRecompose(scope dchar[] seq) pure nothrow @nogc @safe
{
    for (size_t idx = 0; idx + 1 < seq.length; )
    {
        if (isJamoL(seq[idx]) && isJamoV(seq[idx+1]))
        {
            immutable int indexL = seq[idx] - jamoLBase;
            immutable int indexV = seq[idx+1] - jamoVBase;
            immutable int indexLV = indexL * jamoNCount + indexV * jamoTCount;
            if (idx + 2 < seq.length && isJamoT(seq[idx+2]))
            {
                seq[idx] = jamoSBase + indexLV + seq[idx+2] - jamoTBase;
                seq[idx+1] = dchar.init;
                seq[idx+2] = dchar.init;
                idx += 3;
            }
            else
            {
                seq[idx] = jamoSBase + indexLV;
                seq[idx+1] = dchar.init;
                idx += 2;
            }
        }
        else
            idx++;
    }
}

//----------------------------------------------------------------------------
public:

/**
    Decomposes a Hangul syllable. If `ch` is not a composed syllable
    then this function returns $(LREF Grapheme) containing only `ch` as is.
*/
Grapheme decomposeHangul(dchar ch) nothrow pure @safe
{
    immutable idxS = cast(int) ch - jamoSBase;
    if (idxS < 0 || idxS >= jamoSCount) return Grapheme(ch);
    immutable idxL = idxS / jamoNCount;
    immutable idxV = (idxS % jamoNCount) / jamoTCount;
    immutable idxT = idxS % jamoTCount;

    immutable partL = jamoLBase + idxL;
    immutable partV = jamoVBase + idxV;
    if (idxT > 0) // there is a trailling consonant (T); <L,V,T> decomposition
        return Grapheme(partL, partV, jamoTBase + idxT);
    else // <L, V> decomposition
        return Grapheme(partL, partV);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    assert(decomposeHangul('\uD4DB')[].equal("\u1111\u1171\u11B6"));
}

/++
    Try to compose hangul syllable out of a leading consonant (`lead`),
    a `vowel` and optional `trailing` consonant jamos.

    On success returns the composed LV or LVT hangul syllable.

    If any of `lead` and `vowel` are not a valid hangul jamo
    of the respective $(CHARACTER) class returns dchar.init.
+/
dchar composeJamo(dchar lead, dchar vowel, dchar trailing=dchar.init) pure nothrow @nogc @safe
{
    if (!isJamoL(lead))
        return dchar.init;
    immutable indexL = lead - jamoLBase;
    if (!isJamoV(vowel))
        return dchar.init;
    immutable indexV = vowel - jamoVBase;
    immutable indexLV = indexL * jamoNCount + indexV * jamoTCount;
    immutable dchar syllable = jamoSBase + indexLV;
    return isJamoT(trailing) ? syllable + (trailing - jamoTBase) : syllable;
}

///
@safe unittest
{
    assert(composeJamo('\u1111', '\u1171', '\u11B6') == '\uD4DB');
    // leaving out T-vowel, or passing any codepoint
    // that is not trailing consonant composes an LV-syllable
    assert(composeJamo('\u1111', '\u1171') == '\uD4CC');
    assert(composeJamo('\u1111', '\u1171', ' ') == '\uD4CC');
    assert(composeJamo('\u1111', 'A') == dchar.init);
    assert(composeJamo('A', '\u1171') == dchar.init);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text;

    static void testDecomp(UnicodeDecomposition T)(dchar ch, string r)
    {
        Grapheme g = decompose!T(ch);
        assert(equal(g[], r), text(g[], " vs ", r));
    }
    testDecomp!Canonical('\u1FF4', "\u03C9\u0301\u0345");
    testDecomp!Canonical('\uF907', "\u9F9C");
    testDecomp!Compatibility('\u33FF', "\u0067\u0061\u006C");
    testDecomp!Compatibility('\uA7F9', "\u0153");

    // check examples
    assert(decomposeHangul('\uD4DB')[].equal("\u1111\u1171\u11B6"));
    assert(composeJamo('\u1111', '\u1171', '\u11B6') == '\uD4DB');
    assert(composeJamo('\u1111', '\u1171') == '\uD4CC'); // leave out T-vowel
    assert(composeJamo('\u1111', '\u1171', ' ') == '\uD4CC');
    assert(composeJamo('\u1111', 'A') == dchar.init);
    assert(composeJamo('A', '\u1171') == dchar.init);
}

/**
    Enumeration type for normalization forms,
    passed as template parameter for functions like $(LREF normalize).
*/
enum NormalizationForm {
    NFC,
    NFD,
    NFKC,
    NFKD
}


enum {
    /**
        Shorthand aliases from values indicating normalization forms.
    */
    NFC = NormalizationForm.NFC,
    ///ditto
    NFD = NormalizationForm.NFD,
    ///ditto
    NFKC = NormalizationForm.NFKC,
    ///ditto
    NFKD = NormalizationForm.NFKD
}

/++
    Returns `input` string normalized to the chosen form.
    Form C is used by default.

    For more information on normalization forms see
    the $(S_LINK Normalization, normalization section).

    Note:
    In cases where the string in question is already normalized,
    it is returned unmodified and no memory allocation happens.
+/
/*
    WARNING: @trusted lambda inside - handle with same care as @trusted
        functions

    Despite being a template, the attributes do no harm since this doesn't work
    with user-defined range or character types anyway.
*/
pure @safe inout(C)[] normalize(NormalizationForm norm=NFC, C)
    (return scope inout(C)[] input)
{
    import std.algorithm.mutation : SwapStrategy;
    import std.algorithm.sorting : sort;
    import std.array : appender;
    import std.range : zip;

    auto anchors = splitNormalized!norm(input);
    if (anchors[0] == input.length && anchors[1] == input.length)
        return input;
    dchar[] decomposed;
    decomposed.reserve(31);
    ubyte[] ccc;
    ccc.reserve(31);
    auto app = appender!(C[])();
    do
    {
        app.put(input[0 .. anchors[0]]);
        foreach (dchar ch; input[anchors[0]..anchors[1]])
            static if (norm == NFD || norm == NFC)
            {
                foreach (dchar c; decompose!Canonical(ch)[])
                    decomposed ~= c;
            }
            else // NFKD & NFKC
            {
                foreach (dchar c; decompose!Compatibility(ch)[])
                    decomposed ~= c;
            }
        ccc.length = decomposed.length;
        size_t firstNonStable = 0;
        ubyte lastClazz = 0;

        foreach (idx, dchar ch; decomposed)
        {
            immutable clazz = combiningClass(ch);
            ccc[idx] = clazz;
            if (clazz == 0 && lastClazz != 0)
            {
                // found a stable code point after unstable ones
                sort!("a[0] < b[0]", SwapStrategy.stable)
                    (zip(ccc[firstNonStable .. idx], decomposed[firstNonStable .. idx]));
                firstNonStable = decomposed.length;
            }
            else if (clazz != 0 && lastClazz == 0)
            {
                // found first unstable code point after stable ones
                firstNonStable = idx;
            }
            lastClazz = clazz;
        }
        sort!("a[0] < b[0]", SwapStrategy.stable)
            (zip(ccc[firstNonStable..$], decomposed[firstNonStable..$]));
        static if (norm == NFC || norm == NFKC)
        {
            import std.algorithm.searching : countUntil;
            auto first = countUntil(ccc, 0);
            if (first >= 0) // no starters?? no recomposition
            {
                for (;;)
                {
                    immutable second = recompose(first, decomposed, ccc);
                    if (second == decomposed.length)
                        break;
                    first = second;
                }
                // 2nd pass for hangul syllables
                hangulRecompose(decomposed);
            }
        }
        static if (norm == NFD || norm == NFKD)
            app.put(decomposed);
        else
        {
            import std.algorithm.mutation : remove;
            auto clean = remove!("a == dchar.init", SwapStrategy.stable)(decomposed);
            app.put(decomposed[0 .. clean.length]);
        }
        // reset variables
        decomposed.length = 0;
        () @trusted {
            // assumeSafeAppend isn't considered pure as of writing, hence the
            // cast. It isn't pure in the sense that the elements after
            // the array in question are affected, but we don't use those
            // making the call pure for our purposes.
            (cast(void delegate() pure nothrow) {decomposed.assumeSafeAppend();})();
            ccc.length = 0;
            (cast(void delegate() pure nothrow) {ccc.assumeSafeAppend();})();
        } ();
        input = input[anchors[1]..$];
        // and move on
        anchors = splitNormalized!norm(input);
    } while (anchors[0] != input.length);
    app.put(input[0 .. anchors[0]]);
    return () @trusted inout { return cast(inout(C)[]) app.data; } ();
}

///
@safe pure unittest
{
    // any encoding works
    wstring greet = "Hello world";
    assert(normalize(greet) is greet); // the same exact slice

    // An example of a character with all 4 forms being different:
    // Greek upsilon with acute and hook symbol (code point 0x03D3)
    assert(normalize!NFC("ϓ") == "\u03D3");
    assert(normalize!NFD("ϓ") == "\u03D2\u0301");
    assert(normalize!NFKC("ϓ") == "\u038E");
    assert(normalize!NFKD("ϓ") == "\u03A5\u0301");
}

@safe pure unittest
{
    import std.conv : text;

    assert(normalize!NFD("abc\uF904def") == "abc\u6ED1def", text(normalize!NFD("abc\uF904def")));
    assert(normalize!NFKD("2¹⁰") == "210", normalize!NFKD("2¹⁰"));
    assert(normalize!NFD("Äffin") == "A\u0308ffin");

    // test with dstring
    dstring greet = "Hello world";
    assert(normalize(greet) is greet); // the same exact slice
}

// canonically recompose given slice of code points, works in-place and mutates data
private size_t recompose(size_t start, scope dchar[] input, scope ubyte[] ccc) pure nothrow @safe
{
    assert(input.length == ccc.length);
    int accumCC = -1;// so that it's out of 0 .. 255 range
    // writefln("recomposing %( %04x %)", input);
    // first one is always a starter thus we start at i == 1
    size_t i = start+1;
    for (; ; )
    {
        if (i == input.length)
            break;
        immutable curCC = ccc[i];
        // In any character sequence beginning with a starter S
        // a character C is blocked from S if and only if there
        // is some character B between S and C, and either B
        // is a starter or it has the same or higher combining class as C.
        //------------------------
        // Applying to our case:
        // S is input[0]
        // accumCC is the maximum CCC of characters between C and S,
        //     as ccc are sorted
        // C is input[i]

        if (curCC > accumCC)
        {
            immutable comp = compose(input[start], input[i]);
            if (comp != dchar.init)
            {
                input[start] = comp;
                input[i] = dchar.init;// put a sentinel
                // current was merged so its CCC shouldn't affect
                // composing with the next one
            }
            else
            {
                // if it was a starter then accumCC is now 0, end of loop
                accumCC = curCC;
                if (accumCC == 0)
                    break;
            }
        }
        else
        {
            // ditto here
            accumCC = curCC;
            if (accumCC == 0)
                break;
        }
        i++;
    }
    return i;
}

// returns tuple of 2 indexes that delimit:
// normalized text, piece that needs normalization and
// the rest of input starting with stable code point
private auto splitNormalized(NormalizationForm norm, C)(scope const(C)[] input)
{
    import std.typecons : tuple;
    ubyte lastCC = 0;

    foreach (idx, dchar ch; input)
    {
        static if (norm == NFC)
            if (ch < 0x0300)
            {
                lastCC = 0;
                continue;
            }
        immutable ubyte CC = combiningClass(ch);
        if (lastCC > CC && CC != 0)
        {
            return seekStable!norm(idx, input);
        }

        if (notAllowedIn!norm(ch))
        {
           return seekStable!norm(idx, input);
        }
        lastCC = CC;
    }
    return tuple(input.length, input.length);
}

private auto seekStable(NormalizationForm norm, C)(size_t idx, const scope C[] input)
{
    import std.typecons : tuple;
    import std.utf : codeLength;

    auto br = input[0 .. idx];
    size_t region_start = 0;// default
    for (;;)
    {
        if (br.empty)// start is 0
            break;
        dchar ch = br.back;
        if (combiningClass(ch) == 0 && allowedIn!norm(ch))
        {
            region_start = br.length - codeLength!C(ch);
            break;
        }
        br.popFront();
    }
    ///@@@BUG@@@ can't use find: " find is a nested function and can't be used..."
    size_t region_end=input.length;// end is $ by default
    foreach (i, dchar ch; input[idx..$])
    {
        if (combiningClass(ch) == 0 && allowedIn!norm(ch))
        {
            region_end = i+idx;
            break;
        }
    }
    // writeln("Region to normalize: ", input[region_start .. region_end]);
    return tuple(region_start, region_end);
}

/**
    Tests if dchar `ch` is always allowed (Quick_Check=YES) in normalization
    form `norm`.
*/
public bool allowedIn(NormalizationForm norm)(dchar ch)
{
    return !notAllowedIn!norm(ch);
}

///
@safe unittest
{
    // e.g. Cyrillic is always allowed, so is ASCII
    assert(allowedIn!NFC('я'));
    assert(allowedIn!NFD('я'));
    assert(allowedIn!NFKC('я'));
    assert(allowedIn!NFKD('я'));
    assert(allowedIn!NFC('Z'));
}

// not user friendly name but more direct
private bool notAllowedIn(NormalizationForm norm)(dchar ch)
{
    static if (norm == NFC)
        alias qcTrie = nfcQCTrie;
    else static if (norm == NFD)
        alias qcTrie = nfdQCTrie;
    else static if (norm == NFKC)
        alias qcTrie = nfkcQCTrie;
    else static if (norm == NFKD)
        alias qcTrie = nfkdQCTrie;
    else
        static assert("Unknown normalization form "~norm);
    return qcTrie[ch];
}

@safe unittest
{
    assert(allowedIn!NFC('я'));
    assert(allowedIn!NFD('я'));
    assert(allowedIn!NFKC('я'));
    assert(allowedIn!NFKD('я'));
    assert(allowedIn!NFC('Z'));
}

}

version (std_uni_bootstrap)
{
    // old version used for bootstrapping of gen_uni.d that generates
    // up to date optimal versions of all of isXXX functions
    @safe pure nothrow @nogc public bool isWhite(dchar c)
    {
        import std.ascii : isWhite;
        return isWhite(c) ||
               c == lineSep || c == paraSep ||
               c == '\u0085' || c == '\u00A0' || c == '\u1680' || c == '\u180E' ||
               (c >= '\u2000' && c <= '\u200A') ||
               c == '\u202F' || c == '\u205F' || c == '\u3000';
    }
}
else
{

// trusted -> avoid bounds check
@trusted pure nothrow @nogc private
{
    import std.internal.unicode_tables; // : toLowerTable, toTitleTable, toUpperTable; // generated file

    // hide template instances behind functions
    // https://issues.dlang.org/show_bug.cgi?id=13232
    ushort toLowerIndex(dchar c) { return toLowerIndexTrie[c]; }
    ushort toLowerSimpleIndex(dchar c) { return toLowerSimpleIndexTrie[c]; }
    dchar toLowerTab(size_t idx) { return toLowerTable[idx]; }

    ushort toTitleIndex(dchar c) { return toTitleIndexTrie[c]; }
    ushort toTitleSimpleIndex(dchar c) { return toTitleSimpleIndexTrie[c]; }
    dchar toTitleTab(size_t idx) { return toTitleTable[idx]; }

    ushort toUpperIndex(dchar c) { return toUpperIndexTrie[c]; }
    ushort toUpperSimpleIndex(dchar c) { return toUpperSimpleIndexTrie[c]; }
    dchar toUpperTab(size_t idx) { return toUpperTable[idx]; }
}

public:

/++
    Whether or not `c` is a Unicode whitespace $(CHARACTER).
    (general Unicode category: Part of C0(tab, vertical tab, form feed,
    carriage return, and linefeed characters), Zs, Zl, Zp, and NEL(U+0085))
+/
@safe pure nothrow @nogc
public bool isWhite(dchar c)
{
    import std.internal.unicode_tables : isWhiteGen; // generated file
    return isWhiteGen(c); // call pregenerated binary search
}

/++
    Return whether `c` is a Unicode lowercase $(CHARACTER).
+/
@safe pure nothrow @nogc
bool isLower(dchar c)
{
    import std.ascii : isLower, isASCII;
    if (isASCII(c))
        return isLower(c);
    return lowerCaseTrie[c];
}

@safe unittest
{
    import std.ascii : isLower;
    foreach (v; 0 .. 0x80)
        assert(isLower(v) == .isLower(v));
    assert(.isLower('я'));
    assert(.isLower('й'));
    assert(!.isLower('Ж'));
    // Greek HETA
    assert(!.isLower('\u0370'));
    assert(.isLower('\u0371'));
    assert(!.isLower('\u039C')); // capital MU
    assert(.isLower('\u03B2')); // beta
    // from extended Greek
    assert(!.isLower('\u1F18'));
    assert(.isLower('\u1F00'));
    foreach (v; unicode.lowerCase.byCodepoint)
        assert(.isLower(v) && !isUpper(v));
}


/++
    Return whether `c` is a Unicode uppercase $(CHARACTER).
+/
@safe pure nothrow @nogc
bool isUpper(dchar c)
{
    import std.ascii : isUpper, isASCII;
    if (isASCII(c))
        return isUpper(c);
    return upperCaseTrie[c];
}

@safe unittest
{
    import std.ascii : isLower;
    foreach (v; 0 .. 0x80)
        assert(isLower(v) == .isLower(v));
    assert(!isUpper('й'));
    assert(isUpper('Ж'));
    // Greek HETA
    assert(isUpper('\u0370'));
    assert(!isUpper('\u0371'));
    assert(isUpper('\u039C')); // capital MU
    assert(!isUpper('\u03B2')); // beta
    // from extended Greek
    assert(!isUpper('\u1F00'));
    assert(isUpper('\u1F18'));
    foreach (v; unicode.upperCase.byCodepoint)
        assert(isUpper(v) && !.isLower(v));
}


//TODO: Hidden for now, needs better API.
//Other transforms could use better API as well, but this one is a new primitive.
@safe pure nothrow @nogc
private dchar toTitlecase(dchar c)
{
    // optimize ASCII case
    if (c < 0xAA)
    {
        if (c < 'a')
            return c;
        if (c <= 'z')
            return c - 32;
        return c;
    }
    size_t idx = toTitleSimpleIndex(c);
    if (idx != ushort.max)
    {
        return toTitleTab(idx);
    }
    return c;
}

private alias UpperTriple = AliasSeq!(toUpperIndex, MAX_SIMPLE_UPPER, toUpperTab);
private alias LowerTriple = AliasSeq!(toLowerIndex, MAX_SIMPLE_LOWER, toLowerTab);

// generic toUpper/toLower on whole string, creates new or returns as is
private ElementEncodingType!S[] toCase(alias indexFn, uint maxIdx, alias tableFn, alias asciiConvert, S)(S s)
if (isSomeString!S || (isRandomAccessRange!S && hasLength!S && hasSlicing!S && isSomeChar!(ElementType!S)))
{
    import std.array : appender, array;
    import std.ascii : isASCII;
    import std.utf : byDchar, codeLength;

    alias C = ElementEncodingType!S;

    auto r = s.byDchar;
    for (size_t i; !r.empty; i += r.front.codeLength!C , r.popFront())
    {
        auto cOuter = r.front;
        ushort idx = indexFn(cOuter);
        if (idx == ushort.max)
            continue;
        auto result = appender!(C[])();
        result.reserve(s.length);
        result.put(s[0 .. i]);
        foreach (dchar c; s[i .. $].byDchar)
        {
            if (c.isASCII)
            {
                result.put(asciiConvert(c));
            }
            else
            {
                idx = indexFn(c);
                if (idx == ushort.max)
                    result.put(c);
                else if (idx < maxIdx)
                {
                    c = tableFn(idx);
                    result.put(c);
                }
                else
                {
                    auto val = tableFn(idx);
                    // unpack length + codepoint
                    immutable uint len = val >> 24;
                    result.put(cast(dchar)(val & 0xFF_FFFF));
                    foreach (j; idx+1 .. idx+len)
                        result.put(tableFn(j));
                }
            }
        }
        return result.data;
    }

    static if (isSomeString!S)
        return s;
    else
        return s.array;
}

// https://issues.dlang.org/show_bug.cgi?id=12428
@safe unittest
{
    import std.array : replicate;
    auto s = "abcdefghij".replicate(300);
    s = s[0 .. 10];

    toUpper(s);

    assert(s == "abcdefghij");
}

// https://issues.dlang.org/show_bug.cgi?id=18993
@safe unittest
{
    static assert(`몬스터/A`.toLower.length == `몬스터/a`.toLower.length);
}


// generic toUpper/toLower on whole range, returns range
private auto toCaser(alias indexFn, uint maxIdx, alias tableFn, alias asciiConvert, Range)(Range str)
    // Accept range of dchar's
if (isInputRange!Range &&
    isSomeChar!(ElementEncodingType!Range) &&
    ElementEncodingType!Range.sizeof == dchar.sizeof)
{
    static struct ToCaserImpl
    {
        @property bool empty()
        {
            return !nLeft && r.empty;
        }

        @property auto front()
        {
            import std.ascii : isASCII;

            if (!nLeft)
            {
                dchar c = r.front;
                if (c.isASCII)
                {
                    buf[0] = asciiConvert(c);
                    nLeft = 1;
                }
                else
                {
                    const idx = indexFn(c);
                    if (idx == ushort.max)
                    {
                        buf[0] = c;
                        nLeft = 1;
                    }
                    else if (idx < maxIdx)
                    {
                        buf[0] = tableFn(idx);
                        nLeft = 1;
                    }
                    else
                    {
                        immutable val = tableFn(idx);
                        // unpack length + codepoint
                        nLeft = val >> 24;
                        if (nLeft == 0)
                            nLeft = 1;
                        assert(nLeft <= buf.length);
                        buf[nLeft - 1] = cast(dchar)(val & 0xFF_FFFF);
                        foreach (j; 1 .. nLeft)
                            buf[nLeft - j - 1] = tableFn(idx + j);
                    }
                }
            }
            return buf[nLeft - 1];
        }

        void popFront()
        {
            if (!nLeft)
                front;
            assert(nLeft);
            --nLeft;
            if (!nLeft)
                r.popFront();
        }

        static if (isForwardRange!Range)
        {
            @property auto save()
            {
                auto ret = this;
                ret.r = r.save;
                return ret;
            }
        }

      private:
        Range r;
        uint nLeft;
        dchar[3] buf = void;
    }

    return ToCaserImpl(str);
}

/*********************
 * Convert an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
 * or a string to upper or lower case.
 *
 * Does not allocate memory.
 * Characters in UTF-8 or UTF-16 format that cannot be decoded
 * are treated as $(REF replacementDchar, std,utf).
 *
 * Params:
 *      str = string or range of characters
 *
 * Returns:
 *      an input range of `dchar`s
 *
 * See_Also:
 *      $(LREF toUpper), $(LREF toLower)
 */

auto asLowerCase(Range)(Range str)
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    static if (ElementEncodingType!Range.sizeof < dchar.sizeof)
    {
        import std.utf : byDchar;

        // Decode first
        return asLowerCase(str.byDchar);
    }
    else
    {
        static import std.ascii;
        return toCaser!(LowerTriple, std.ascii.toLower)(str);
    }
}

/// ditto
auto asUpperCase(Range)(Range str)
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    static if (ElementEncodingType!Range.sizeof < dchar.sizeof)
    {
        import std.utf : byDchar;

        // Decode first
        return asUpperCase(str.byDchar);
    }
    else
    {
        static import std.ascii;
        return toCaser!(UpperTriple, std.ascii.toUpper)(str);
    }
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert("hEllo".asUpperCase.equal("HELLO"));
}

// explicitly undocumented
auto asLowerCase(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    import std.traits : StringTypeOf;
    return asLowerCase!(StringTypeOf!Range)(str);
}

// explicitly undocumented
auto asUpperCase(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    import std.traits : StringTypeOf;
    return asUpperCase!(StringTypeOf!Range)(str);
}

@safe unittest
{
    static struct TestAliasedString
    {
        string get() @safe @nogc pure nothrow { return _s; }
        alias get this;
        @disable this(this);
        string _s;
    }

    static bool testAliasedString(alias func, Args...)(string s, Args args)
    {
        import std.algorithm.comparison : equal;
        auto a = func(TestAliasedString(s), args);
        auto b = func(s, args);
        static if (is(typeof(equal(a, b))))
        {
            // For ranges, compare contents instead of object identity.
            return equal(a, b);
        }
        else
        {
            return a == b;
        }
    }
    assert(testAliasedString!asLowerCase("hEllo"));
    assert(testAliasedString!asUpperCase("hEllo"));
    assert(testAliasedString!asCapitalized("hEllo"));
}

@safe unittest
{
    import std.array : array;

    auto a = "HELLo".asLowerCase;
    auto savea = a.save;
    auto s = a.array;
    assert(s == "hello");
    s = savea.array;
    assert(s == "hello");

    string[] lower = ["123", "abcфеж", "\u0131\u023f\u03c9", "i\u0307\u1Fe2"];
    string[] upper = ["123", "ABCФЕЖ", "I\u2c7e\u2126", "\u0130\u03A5\u0308\u0300"];

    foreach (i, slwr; lower)
    {
        import std.utf : byChar;

        auto sx = slwr.asUpperCase.byChar.array;
        assert(sx == toUpper(slwr));
        auto sy = upper[i].asLowerCase.byChar.array;
        assert(sy == toLower(upper[i]));
    }

    // Not necessary to call r.front
    for (auto r = lower[3].asUpperCase; !r.empty; r.popFront())
    {
    }

    import std.algorithm.comparison : equal;

    "HELLo"w.asLowerCase.equal("hello"d);
    "HELLo"w.asUpperCase.equal("HELLO"d);
    "HELLo"d.asLowerCase.equal("hello"d);
    "HELLo"d.asUpperCase.equal("HELLO"d);

    import std.utf : byChar;
    assert(toLower("\u1Fe2") == asLowerCase("\u1Fe2").byChar.array);
}

// generic capitalizer on whole range, returns range
private auto toCapitalizer(alias indexFnUpper, uint maxIdxUpper, alias tableFnUpper,
                           Range)(Range str)
    // Accept range of dchar's
if (isInputRange!Range &&
    isSomeChar!(ElementEncodingType!Range) &&
    ElementEncodingType!Range.sizeof == dchar.sizeof)
{
    static struct ToCapitalizerImpl
    {
        @property bool empty()
        {
            return lower ? lwr.empty : !nLeft && r.empty;
        }

        @property auto front()
        {
            if (lower)
                return lwr.front;

            if (!nLeft)
            {
                immutable dchar c = r.front;
                const idx = indexFnUpper(c);
                if (idx == ushort.max)
                {
                    buf[0] = c;
                    nLeft = 1;
                }
                else if (idx < maxIdxUpper)
                {
                    buf[0] = tableFnUpper(idx);
                    nLeft = 1;
                }
                else
                {
                    immutable val = tableFnUpper(idx);
                    // unpack length + codepoint
                    nLeft = val >> 24;
                    if (nLeft == 0)
                        nLeft = 1;
                    assert(nLeft <= buf.length);
                    buf[nLeft - 1] = cast(dchar)(val & 0xFF_FFFF);
                    foreach (j; 1 .. nLeft)
                        buf[nLeft - j - 1] = tableFnUpper(idx + j);
                }
            }
            return buf[nLeft - 1];
        }

        void popFront()
        {
            if (lower)
                lwr.popFront();
            else
            {
                if (!nLeft)
                    front;
                assert(nLeft);
                --nLeft;
                if (!nLeft)
                {
                    r.popFront();
                    lwr = r.asLowerCase();
                    lower = true;
                }
            }
        }

        static if (isForwardRange!Range)
        {
            @property auto save()
            {
                auto ret = this;
                ret.r = r.save;
                ret.lwr = lwr.save;
                return ret;
            }
        }

      private:
        Range r;
        typeof(r.asLowerCase) lwr; // range representing the lower case rest of string
        bool lower = false;     // false for first character, true for rest of string
        dchar[3] buf = void;
        uint nLeft = 0;
    }

    return ToCapitalizerImpl(str);
}

/*********************
 * Capitalize an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
 * or string, meaning convert the first
 * character to upper case and subsequent characters to lower case.
 *
 * Does not allocate memory.
 * Characters in UTF-8 or UTF-16 format that cannot be decoded
 * are treated as $(REF replacementDchar, std,utf).
 *
 * Params:
 *      str = string or range of characters
 *
 * Returns:
 *      an InputRange of dchars
 *
 * See_Also:
 *      $(LREF toUpper), $(LREF toLower)
 *      $(LREF asUpperCase), $(LREF asLowerCase)
 */

auto asCapitalized(Range)(Range str)
if (isInputRange!Range && isSomeChar!(ElementEncodingType!Range) &&
    !isConvertibleToString!Range)
{
    static if (ElementEncodingType!Range.sizeof < dchar.sizeof)
    {
        import std.utf : byDchar;

        // Decode first
        return toCapitalizer!UpperTriple(str.byDchar);
    }
    else
    {
        return toCapitalizer!UpperTriple(str);
    }
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert("hEllo".asCapitalized.equal("Hello"));
}

auto asCapitalized(Range)(auto ref Range str)
if (isConvertibleToString!Range)
{
    import std.traits : StringTypeOf;
    return asCapitalized!(StringTypeOf!Range)(str);
}

@safe pure nothrow @nogc unittest
{
    auto r = "hEllo".asCapitalized();
    assert(r.front == 'H');
}

@safe unittest
{
    import std.array : array;

    auto a = "hELLo".asCapitalized;
    auto savea = a.save;
    auto s = a.array;
    assert(s == "Hello");
    s = savea.array;
    assert(s == "Hello");

    string[2][] cases =
    [
        ["", ""],
        ["h", "H"],
        ["H", "H"],
        ["3", "3"],
        ["123", "123"],
        ["h123A", "H123a"],
        ["феж", "Феж"],
        ["\u1Fe2", "\u03a5\u0308\u0300"],
    ];

    foreach (i; 0 .. cases.length)
    {
        import std.utf : byChar;

        auto r = cases[i][0].asCapitalized.byChar.array;
        auto result = cases[i][1];
        assert(r == result);
    }

    // Don't call r.front
    for (auto r = "\u1Fe2".asCapitalized; !r.empty; r.popFront())
    {
    }

    import std.algorithm.comparison : equal;

    "HELLo"w.asCapitalized.equal("Hello"d);
    "hElLO"w.asCapitalized.equal("Hello"d);
    "hello"d.asCapitalized.equal("Hello"d);
    "HELLO"d.asCapitalized.equal("Hello"d);

    import std.utf : byChar;
    assert(asCapitalized("\u0130").byChar.array == asUpperCase("\u0130").byChar.array);
}

// TODO: helper, I wish std.utf was more flexible (and stright)
private size_t encodeTo(scope char[] buf, size_t idx, dchar c) @trusted pure nothrow @nogc
{
    if (c <= 0x7F)
    {
        buf[idx] = cast(char) c;
        idx++;
    }
    else if (c <= 0x7FF)
    {
        buf[idx] = cast(char)(0xC0 | (c >> 6));
        buf[idx+1] = cast(char)(0x80 | (c & 0x3F));
        idx += 2;
    }
    else if (c <= 0xFFFF)
    {
        buf[idx] = cast(char)(0xE0 | (c >> 12));
        buf[idx+1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[idx+2] = cast(char)(0x80 | (c & 0x3F));
        idx += 3;
    }
    else if (c <= 0x10FFFF)
    {
        buf[idx] = cast(char)(0xF0 | (c >> 18));
        buf[idx+1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        buf[idx+2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[idx+3] = cast(char)(0x80 | (c & 0x3F));
        idx += 4;
    }
    else
        assert(0);
    return idx;
}

@safe unittest
{
    char[] s = "abcd".dup;
    size_t i = 0;
    i = encodeTo(s, i, 'X');
    assert(s == "Xbcd");

    i = encodeTo(s, i, cast(dchar)'\u00A9');
    assert(s == "X\xC2\xA9d");
}

// TODO: helper, I wish std.utf was more flexible (and stright)
private size_t encodeTo(scope wchar[] buf, size_t idx, dchar c) @trusted pure
{
    import std.utf : UTFException;
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            throw (new UTFException("Encoding an isolated surrogate code point in UTF-16")).setSequence(c);
        buf[idx] = cast(wchar) c;
        idx++;
    }
    else if (c <= 0x10FFFF)
    {
        buf[idx] = cast(wchar)((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[idx+1] = cast(wchar)(((c - 0x10000) & 0x3FF) + 0xDC00);
        idx += 2;
    }
    else
        assert(0);
    return idx;
}

private size_t encodeTo(scope dchar[] buf, size_t idx, dchar c) @trusted pure nothrow @nogc
{
    buf[idx] = c;
    idx++;
    return idx;
}

private void toCaseInPlace(alias indexFn, uint maxIdx, alias tableFn, C)(ref C[] s) @trusted pure
if (is(C == char) || is(C == wchar)  || is(C == dchar))
{
    import std.utf : decode, codeLength;
    size_t curIdx = 0;
    size_t destIdx = 0;
    alias slowToCase = toCaseInPlaceAlloc!(indexFn, maxIdx, tableFn);
    size_t lastUnchanged = 0;
    // in-buffer move of bytes to a new start index
    // the trick is that it may not need to copy at all
    static size_t moveTo(C[] str, size_t dest, size_t from, size_t to)
    {
        // Interestingly we may just bump pointer for a while
        // then have to copy if a re-cased char was smaller the original
        // later we may regain pace with char that got bigger
        // In the end it sometimes flip-flops between the 2 cases below
        if (dest == from)
            return to;
        // got to copy
        foreach (C c; str[from .. to])
            str[dest++] = c;
        return dest;
    }
    while (curIdx != s.length)
    {
        size_t startIdx = curIdx;
        immutable ch = decode(s, curIdx);
        // TODO: special case for ASCII
        immutable caseIndex = indexFn(ch);
        if (caseIndex == ushort.max) // unchanged, skip over
        {
            continue;
        }
        else if (caseIndex < maxIdx)  // 1:1 codepoint mapping
        {
            // previous cased chars had the same length as uncased ones
            // thus can just adjust pointer
            destIdx = moveTo(s, destIdx, lastUnchanged, startIdx);
            lastUnchanged = curIdx;
            immutable cased = tableFn(caseIndex);
            immutable casedLen = codeLength!C(cased);
            if (casedLen + destIdx > curIdx) // no place to fit cased char
            {
                // switch to slow codepath, where we allocate
                return slowToCase(s, startIdx, destIdx);
            }
            else
            {
                destIdx = encodeTo(s, destIdx, cased);
            }
        }
        else  // 1:m codepoint mapping, slow codepath
        {
            destIdx = moveTo(s, destIdx, lastUnchanged, startIdx);
            lastUnchanged = curIdx;
            return slowToCase(s, startIdx, destIdx);
        }
        assert(destIdx <= curIdx);
    }
    if (lastUnchanged != s.length)
    {
        destIdx = moveTo(s, destIdx, lastUnchanged, s.length);
    }
    s = s[0 .. destIdx];
}

// helper to precalculate size of case-converted string
private template toCaseLength(alias indexFn, uint maxIdx, alias tableFn)
{
    size_t toCaseLength(C)(const scope C[] str)
    {
        import std.utf : decode, codeLength;
        size_t codeLen = 0;
        size_t lastNonTrivial = 0;
        size_t curIdx = 0;
        while (curIdx != str.length)
        {
            immutable startIdx = curIdx;
            immutable ch = decode(str, curIdx);
            immutable ushort caseIndex = indexFn(ch);
            if (caseIndex == ushort.max)
                continue;
            else if (caseIndex < maxIdx)
            {
                codeLen += startIdx - lastNonTrivial;
                lastNonTrivial = curIdx;
                immutable cased = tableFn(caseIndex);
                codeLen += codeLength!C(cased);
            }
            else
            {
                codeLen += startIdx - lastNonTrivial;
                lastNonTrivial = curIdx;
                immutable val = tableFn(caseIndex);
                immutable len = val >> 24;
                immutable dchar cased = val & 0xFF_FFFF;
                codeLen += codeLength!C(cased);
                foreach (j; caseIndex+1 .. caseIndex+len)
                    codeLen += codeLength!C(tableFn(j));
            }
        }
        if (lastNonTrivial != str.length)
            codeLen += str.length - lastNonTrivial;
        return codeLen;
    }
}

@safe unittest
{
    alias toLowerLength = toCaseLength!(LowerTriple);
    assert(toLowerLength("abcd") == 4);
    assert(toLowerLength("аБВгд456") == 10+3);
}

// slower code path that preallocates and then copies
// case-converted stuf to the new string
private template toCaseInPlaceAlloc(alias indexFn, uint maxIdx, alias tableFn)
{
    void toCaseInPlaceAlloc(C)(ref C[] s, size_t curIdx,
        size_t destIdx) @trusted pure
    if (is(C == char) || is(C == wchar) || is(C == dchar))
    {
        import std.utf : decode;
        alias caseLength = toCaseLength!(indexFn, maxIdx, tableFn);
        auto trueLength = destIdx + caseLength(s[curIdx..$]);
        C[] ns = new C[trueLength];
        ns[0 .. destIdx] = s[0 .. destIdx];
        size_t lastUnchanged = curIdx;
        while (curIdx != s.length)
        {
            immutable startIdx = curIdx; // start of current codepoint
            immutable ch = decode(s, curIdx);
            immutable caseIndex = indexFn(ch);
            if (caseIndex == ushort.max) // skip over
            {
                continue;
            }
            else if (caseIndex < maxIdx)  // 1:1 codepoint mapping
            {
                immutable cased = tableFn(caseIndex);
                auto toCopy = startIdx - lastUnchanged;
                ns[destIdx .. destIdx+toCopy] = s[lastUnchanged .. startIdx];
                lastUnchanged = curIdx;
                destIdx += toCopy;
                destIdx = encodeTo(ns, destIdx, cased);
            }
            else  // 1:m codepoint mapping, slow codepath
            {
                auto toCopy = startIdx - lastUnchanged;
                ns[destIdx .. destIdx+toCopy] = s[lastUnchanged .. startIdx];
                lastUnchanged = curIdx;
                destIdx += toCopy;
                auto val = tableFn(caseIndex);
                // unpack length + codepoint
                immutable uint len = val >> 24;
                destIdx = encodeTo(ns, destIdx, cast(dchar)(val & 0xFF_FFFF));
                foreach (j; caseIndex+1 .. caseIndex+len)
                    destIdx = encodeTo(ns, destIdx, tableFn(j));
            }
        }
        if (lastUnchanged != s.length)
        {
            auto toCopy = s.length - lastUnchanged;
            ns[destIdx .. destIdx+toCopy] = s[lastUnchanged..$];
            destIdx += toCopy;
        }
        assert(ns.length == destIdx);
        s = ns;
    }
}

/++
    Converts `s` to lowercase (by performing Unicode lowercase mapping) in place.
    For a few characters string length may increase after the transformation,
    in such a case the function reallocates exactly once.
    If `s` does not have any uppercase characters, then `s` is unaltered.
+/
void toLowerInPlace(C)(ref C[] s) @trusted pure
if (is(C == char) || is(C == wchar) || is(C == dchar))
{
    toCaseInPlace!(LowerTriple)(s);
}
// overloads for the most common cases to reduce compile time
@safe pure /*TODO nothrow*/
{
    void toLowerInPlace(ref char[] s)
    { toLowerInPlace!char(s); }
    void toLowerInPlace(ref wchar[] s)
    { toLowerInPlace!wchar(s); }
    void toLowerInPlace(ref dchar[] s)
    { toLowerInPlace!dchar(s); }
}

/++
    Converts `s` to uppercase  (by performing Unicode uppercase mapping) in place.
    For a few characters string length may increase after the transformation,
    in such a case the function reallocates exactly once.
    If `s` does not have any lowercase characters, then `s` is unaltered.
+/
void toUpperInPlace(C)(ref C[] s) @trusted pure
if (is(C == char) || is(C == wchar) || is(C == dchar))
{
    toCaseInPlace!(UpperTriple)(s);
}
// overloads for the most common cases to reduce compile time/code size
@safe pure /*TODO nothrow*/
{
    void toUpperInPlace(ref char[] s)
    { toUpperInPlace!char(s); }
    void toUpperInPlace(ref wchar[] s)
    { toUpperInPlace!wchar(s); }
    void toUpperInPlace(ref dchar[] s)
    { toUpperInPlace!dchar(s); }
}

/++
    If `c` is a Unicode uppercase $(CHARACTER), then its lowercase equivalent
    is returned. Otherwise `c` is returned.

    Warning: certain alphabets like German and Greek have no 1:1
    upper-lower mapping. Use overload of toLower which takes full string instead.
+/
@safe pure nothrow @nogc
dchar toLower(dchar c)
{
     // optimize ASCII case
    if (c < 0xAA)
    {
        if (c < 'A')
            return c;
        if (c <= 'Z')
            return c + 32;
        return c;
    }
    size_t idx = toLowerSimpleIndex(c);
    if (idx != ushort.max)
    {
        return toLowerTab(idx);
    }
    return c;
}

/++
    Creates a new array which is identical to `s` except that all of its
    characters are converted to lowercase (by performing Unicode lowercase mapping).
    If none of `s` characters were affected, then `s` itself is returned if `s` is a
    `string`-like type.

    Params:
        s = A $(REF_ALTTEXT random access range, isRandomAccessRange, std,range,primitives)
        of characters
    Returns:
        An array with the same element type as `s`.
+/
ElementEncodingType!S[] toLower(S)(return scope S s) @trusted
if (isSomeString!S)
{
    static import std.ascii;
    return toCase!(LowerTriple, std.ascii.toLower)(s);
}

/// ditto
ElementEncodingType!S[] toLower(S)(S s)
if (!isSomeString!S && (isRandomAccessRange!S && hasLength!S && hasSlicing!S && isSomeChar!(ElementType!S)))
{
    static import std.ascii;
    return toCase!(LowerTriple, std.ascii.toLower)(s);
}

// overloads for the most common cases to reduce compile time
@safe pure /*TODO nothrow*/
{
    string toLower(return scope string s)
    { return toLower!string(s); }
    wstring toLower(return scope wstring s)
    { return toLower!wstring(s); }
    dstring toLower(return scope dstring s)
    { return toLower!dstring(s); }

    @safe unittest
    {
        // https://issues.dlang.org/show_bug.cgi?id=16663

        static struct String
        {
            string data;
            alias data this;
        }

        void foo()
        {
            auto u = toLower(String(""));
        }
    }
}


@safe unittest
{
    static import std.ascii;
    import std.format : format;
    foreach (ch; 0 .. 0x80)
        assert(std.ascii.toLower(ch) == toLower(ch));
    assert(toLower('Я') == 'я');
    assert(toLower('Δ') == 'δ');
    foreach (ch; unicode.upperCase.byCodepoint)
    {
        dchar low = ch.toLower();
        assert(low == ch || isLower(low), format("%s -> %s", ch, low));
    }
    assert(toLower("АЯ") == "ая");

    assert("\u1E9E".toLower == "\u00df");
    assert("\u00df".toUpper == "SS");
}

// https://issues.dlang.org/show_bug.cgi?id=9629
@safe unittest
{
    wchar[] test = "hello þ world"w.dup;
    auto piece = test[6 .. 7];
    toUpperInPlace(piece);
    assert(test == "hello Þ world");
}


@safe unittest
{
    import std.algorithm.comparison : cmp;
    string s1 = "FoL";
    string s2 = toLower(s1);
    assert(cmp(s2, "fol") == 0, s2);
    assert(s2 != s1);

    char[] s3 = s1.dup;
    toLowerInPlace(s3);
    assert(s3 == s2);

    s1 = "A\u0100B\u0101d";
    s2 = toLower(s1);
    s3 = s1.dup;
    assert(cmp(s2, "a\u0101b\u0101d") == 0);
    assert(s2 !is s1);
    toLowerInPlace(s3);
    assert(s3 == s2);

    s1 = "A\u0460B\u0461d";
    s2 = toLower(s1);
    s3 = s1.dup;
    assert(cmp(s2, "a\u0461b\u0461d") == 0);
    assert(s2 !is s1);
    toLowerInPlace(s3);
    assert(s3 == s2);

    s1 = "\u0130";
    s2 = toLower(s1);
    s3 = s1.dup;
    assert(s2 == "i\u0307");
    assert(s2 !is s1);
    toLowerInPlace(s3);
    assert(s3 == s2);

    // Test on wchar and dchar strings.
    assert(toLower("Some String"w) == "some string"w);
    assert(toLower("Some String"d) == "some string"d);

    // https://issues.dlang.org/show_bug.cgi?id=12455
    dchar c = 'İ'; // '\U0130' LATIN CAPITAL LETTER I WITH DOT ABOVE
    assert(isUpper(c));
    assert(toLower(c) == 'i');
    // extends on https://issues.dlang.org/show_bug.cgi?id=12455 report
    // check simple-case toUpper too
    c = '\u1f87';
    assert(isLower(c));
    assert(toUpper(c) == '\u1F8F');
}

@safe pure unittest
{
    import std.algorithm.comparison : cmp, equal;
    import std.utf : byCodeUnit;
    auto r1 = "FoL".byCodeUnit;
    assert(r1.toLower.cmp("fol") == 0);
    auto r2 = "A\u0460B\u0461d".byCodeUnit;
    assert(r2.toLower.cmp("a\u0461b\u0461d") == 0);
}

/++
    If `c` is a Unicode lowercase $(CHARACTER), then its uppercase equivalent
    is returned. Otherwise `c` is returned.

    Warning:
    Certain alphabets like German and Greek have no 1:1
    upper-lower mapping. Use overload of toUpper which takes full string instead.

    toUpper can be used as an argument to $(REF map, std,algorithm,iteration)
    to produce an algorithm that can convert a range of characters to upper case
    without allocating memory.
    A string can then be produced by using $(REF copy, std,algorithm,mutation)
    to send it to an $(REF appender, std,array).
+/
@safe pure nothrow @nogc
dchar toUpper(dchar c)
{
    // optimize ASCII case
    if (c < 0xAA)
    {
        if (c < 'a')
            return c;
        if (c <= 'z')
            return c - 32;
        return c;
    }
    size_t idx = toUpperSimpleIndex(c);
    if (idx != ushort.max)
    {
        return toUpperTab(idx);
    }
    return c;
}

///
@safe unittest
{
    import std.algorithm.iteration : map;
    import std.algorithm.mutation : copy;
    import std.array : appender;

    auto abuf = appender!(char[])();
    "hello".map!toUpper.copy(abuf);
    assert(abuf.data == "HELLO");
}

@safe unittest
{
    static import std.ascii;
    import std.format : format;
    foreach (ch; 0 .. 0x80)
        assert(std.ascii.toUpper(ch) == toUpper(ch));
    assert(toUpper('я') == 'Я');
    assert(toUpper('δ') == 'Δ');
    auto title = unicode.Titlecase_Letter;
    foreach (ch; unicode.lowerCase.byCodepoint)
    {
        dchar up = ch.toUpper();
        assert(up == ch || isUpper(up) || title[up],
            format("%x -> %x", ch, up));
    }
}

/++
    Allocates a new array which is identical to `s` except that all of its
    characters are converted to uppercase (by performing Unicode uppercase mapping).
    If none of `s` characters were affected, then `s` itself is returned if `s`
    is a `string`-like type.

    Params:
        s = A $(REF_ALTTEXT random access range, isRandomAccessRange, std,range,primitives)
        of characters
    Returns:
        An new array with the same element type as `s`.
+/
ElementEncodingType!S[] toUpper(S)(return scope S s) @trusted
if (isSomeString!S)
{
    static import std.ascii;
    return toCase!(UpperTriple, std.ascii.toUpper)(s);
}

/// ditto
ElementEncodingType!S[] toUpper(S)(S s)
if (!isSomeString!S && (isRandomAccessRange!S && hasLength!S && hasSlicing!S && isSomeChar!(ElementType!S)))
{
    static import std.ascii;
    return toCase!(UpperTriple, std.ascii.toUpper)(s);
}

// overloads for the most common cases to reduce compile time
@safe pure /*TODO nothrow*/
{
    string toUpper(return scope string s)
    { return toUpper!string(s); }
    wstring toUpper(return scope wstring s)
    { return toUpper!wstring(s); }
    dstring toUpper(return scope dstring s)
    { return toUpper!dstring(s); }

    @safe unittest
    {
        // https://issues.dlang.org/show_bug.cgi?id=16663

        static struct String
        {
            string data;
            alias data this;
        }

        void foo()
        {
            auto u = toUpper(String(""));
        }
    }
}

@safe unittest
{
    import std.algorithm.comparison : cmp;

    string s1 = "FoL";
    string s2;
    char[] s3;

    s2 = toUpper(s1);
    s3 = s1.dup; toUpperInPlace(s3);
    assert(s3 == s2, s3);
    assert(cmp(s2, "FOL") == 0);
    assert(s2 !is s1);

    s1 = "a\u0100B\u0101d";
    s2 = toUpper(s1);
    s3 = s1.dup; toUpperInPlace(s3);
    assert(s3 == s2);
    assert(cmp(s2, "A\u0100B\u0100D") == 0);
    assert(s2 !is s1);

    s1 = "a\u0460B\u0461d";
    s2 = toUpper(s1);
    s3 = s1.dup; toUpperInPlace(s3);
    assert(s3 == s2);
    assert(cmp(s2, "A\u0460B\u0460D") == 0);
    assert(s2 !is s1);
}

@safe unittest
{
    static void doTest(C)(const(C)[] s, const(C)[] trueUp, const(C)[] trueLow)
    {
        import std.format : format;
        string diff = "src: %( %x %)\nres: %( %x %)\ntru: %( %x %)";
        auto low = s.toLower() , up = s.toUpper();
        auto lowInp = s.dup, upInp = s.dup;
        lowInp.toLowerInPlace();
        upInp.toUpperInPlace();
        assert(low == trueLow, format(diff, low, trueLow));
        assert(up == trueUp,  format(diff, up, trueUp));
        assert(lowInp == trueLow,
            format(diff, cast(const(ubyte)[]) s, cast(const(ubyte)[]) lowInp, cast(const(ubyte)[]) trueLow));
        assert(upInp == trueUp,
            format(diff, cast(const(ubyte)[]) s, cast(const(ubyte)[]) upInp, cast(const(ubyte)[]) trueUp));
    }
    static foreach (S; AliasSeq!(dstring, wstring, string))
    {{

        S easy = "123";
        S good = "abCФеж";
        S awful = "\u0131\u023f\u2126";
        S wicked = "\u0130\u1FE2";
        auto options = [easy, good, awful, wicked];
        S[] lower = ["123", "abcфеж", "\u0131\u023f\u03c9", "i\u0307\u1Fe2"];
        S[] upper = ["123", "ABCФЕЖ", "I\u2c7e\u2126", "\u0130\u03A5\u0308\u0300"];

        foreach (val; [easy, good])
        {
            auto e = val.dup;
            auto g = e;
            e.toUpperInPlace();
            assert(e is g);
            e.toLowerInPlace();
            assert(e is g);
        }
        foreach (i, v; options)
        {
            doTest(v, upper[i], lower[i]);
        }

        // a few combinatorial runs
        foreach (i; 0 .. options.length)
        foreach (j; i .. options.length)
        foreach (k; j .. options.length)
        {
            auto sample = options[i] ~ options[j] ~ options[k];
            auto sample2 = options[k] ~ options[j] ~ options[i];
            doTest(sample, upper[i] ~ upper[j] ~ upper[k],
                lower[i] ~ lower[j] ~ lower[k]);
            doTest(sample2, upper[k] ~ upper[j] ~ upper[i],
                lower[k] ~ lower[j] ~ lower[i]);
        }
    }}
}

// test random access ranges
@safe pure unittest
{
    import std.algorithm.comparison : cmp;
    import std.utf : byCodeUnit;
    auto s1 = "FoL".byCodeUnit;
    assert(s1.toUpper.cmp("FOL") == 0);
    auto s2 = "a\u0460B\u0461d".byCodeUnit;
    assert(s2.toUpper.cmp("A\u0460B\u0460D") == 0);
}

/++
    Returns whether `c` is a Unicode alphabetic $(CHARACTER)
    (general Unicode category: Alphabetic).
+/
@safe pure nothrow @nogc
bool isAlpha(dchar c)
{
    // optimization
    if (c < 0xAA)
    {
        return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
    }

    return alphaTrie[c];
}

@safe unittest
{
    auto alpha = unicode("Alphabetic");
    foreach (ch; alpha.byCodepoint)
        assert(isAlpha(ch));
    foreach (ch; 0 .. 0x4000)
        assert((ch in alpha) == isAlpha(ch));
}


/++
    Returns whether `c` is a Unicode mark
    (general Unicode category: Mn, Me, Mc).
+/
@safe pure nothrow @nogc
bool isMark(dchar c)
{
    return markTrie[c];
}

@safe unittest
{
    auto mark = unicode("Mark");
    foreach (ch; mark.byCodepoint)
        assert(isMark(ch));
    foreach (ch; 0 .. 0x4000)
        assert((ch in mark) == isMark(ch));
}

/++
    Returns whether `c` is a Unicode numerical $(CHARACTER)
    (general Unicode category: Nd, Nl, No).
+/
@safe pure nothrow @nogc
bool isNumber(dchar c)
{
    // optimization for ascii case
    if (c <= 0x7F)
    {
        return c >= '0' && c <= '9';
    }
    else
    {
        return numberTrie[c];
    }
}

@safe unittest
{
    auto n = unicode("N");
    foreach (ch; n.byCodepoint)
        assert(isNumber(ch));
    foreach (ch; 0 .. 0x4000)
        assert((ch in n) == isNumber(ch));
}

/++
    Returns whether `c` is a Unicode alphabetic $(CHARACTER) or number.
    (general Unicode category: Alphabetic, Nd, Nl, No).

    Params:
        c = any Unicode character
    Returns:
        `true` if the character is in the Alphabetic, Nd, Nl, or No Unicode
        categories
+/
@safe pure nothrow @nogc
bool isAlphaNum(dchar c)
{
    static import std.ascii;

    // optimization for ascii case
    if (std.ascii.isASCII(c))
    {
        return std.ascii.isAlphaNum(c);
    }
    else
    {
        return isAlpha(c) || isNumber(c);
    }
}

@safe unittest
{
    auto n = unicode("N");
    auto alpha = unicode("Alphabetic");

    foreach (ch; n.byCodepoint)
        assert(isAlphaNum(ch));

    foreach (ch; alpha.byCodepoint)
        assert(isAlphaNum(ch));

    foreach (ch; 0 .. 0x4000)
    {
        assert(((ch in n) || (ch in alpha)) == isAlphaNum(ch));
    }
}

/++
    Returns whether `c` is a Unicode punctuation $(CHARACTER)
    (general Unicode category: Pd, Ps, Pe, Pc, Po, Pi, Pf).
+/
@safe pure nothrow @nogc
bool isPunctuation(dchar c)
{
    static import std.ascii;

    // optimization for ascii case
    if (c <= 0x7F)
    {
        return std.ascii.isPunctuation(c);
    }
    else
    {
        return punctuationTrie[c];
    }
}

@safe unittest
{
    assert(isPunctuation('\u0021'));
    assert(isPunctuation('\u0028'));
    assert(isPunctuation('\u0029'));
    assert(isPunctuation('\u002D'));
    assert(isPunctuation('\u005F'));
    assert(isPunctuation('\u00AB'));
    assert(isPunctuation('\u00BB'));
    foreach (ch; unicode("P").byCodepoint)
        assert(isPunctuation(ch));
}

/++
    Returns whether `c` is a Unicode symbol $(CHARACTER)
    (general Unicode category: Sm, Sc, Sk, So).
+/
@safe pure nothrow @nogc
bool isSymbol(dchar c)
{
   return symbolTrie[c];
}

@safe unittest
{
    import std.format : format;
    assert(isSymbol('\u0024'));
    assert(isSymbol('\u002B'));
    assert(isSymbol('\u005E'));
    assert(isSymbol('\u00A6'));
    foreach (ch; unicode("S").byCodepoint)
        assert(isSymbol(ch), format("%04x", ch));
}

/++
    Returns whether `c` is a Unicode space $(CHARACTER)
    (general Unicode category: Zs)
    Note: This doesn't include '\n', '\r', \t' and other non-space $(CHARACTER).
    For commonly used less strict semantics see $(LREF isWhite).
+/
@safe pure nothrow @nogc
bool isSpace(dchar c)
{
    import std.internal.unicode_tables : isSpaceGen; // generated file
    return isSpaceGen(c);
}

@safe unittest
{
    assert(isSpace('\u0020'));
    auto space = unicode.Zs;
    foreach (ch; space.byCodepoint)
        assert(isSpace(ch));
    foreach (ch; 0 .. 0x1000)
        assert(isSpace(ch) == space[ch]);
}


/++
    Returns whether `c` is a Unicode graphical $(CHARACTER)
    (general Unicode category: L, M, N, P, S, Zs).

+/
@safe pure nothrow @nogc
bool isGraphical(dchar c)
{
    return graphicalTrie[c];
}


@safe unittest
{
    auto set = unicode("Graphical");
    import std.format : format;
    foreach (ch; set.byCodepoint)
        assert(isGraphical(ch), format("%4x", ch));
    foreach (ch; 0 .. 0x4000)
        assert((ch in set) == isGraphical(ch));
}


/++
    Returns whether `c` is a Unicode control $(CHARACTER)
    (general Unicode category: Cc).
+/
@safe pure nothrow @nogc
bool isControl(dchar c)
{
    import std.internal.unicode_tables : isControlGen; // generated file
    return isControlGen(c);
}

@safe unittest
{
    assert(isControl('\u0000'));
    assert(isControl('\u0081'));
    assert(!isControl('\u0100'));
    auto cc = unicode.Cc;
    foreach (ch; cc.byCodepoint)
        assert(isControl(ch));
    foreach (ch; 0 .. 0x1000)
        assert(isControl(ch) == cc[ch]);
}


/++
    Returns whether `c` is a Unicode formatting $(CHARACTER)
    (general Unicode category: Cf).
+/
@safe pure nothrow @nogc
bool isFormat(dchar c)
{
    import std.internal.unicode_tables : isFormatGen; // generated file
    return isFormatGen(c);
}


@safe unittest
{
    assert(isFormat('\u00AD'));
    foreach (ch; unicode("Format").byCodepoint)
        assert(isFormat(ch));
}

// code points for private use, surrogates are not likely to change in near feature
// if need be they can be generated from unicode data as well

/++
    Returns whether `c` is a Unicode Private Use $(CODEPOINT)
    (general Unicode category: Co).
+/
@safe pure nothrow @nogc
bool isPrivateUse(dchar c)
{
    return (0x00_E000 <= c && c <= 0x00_F8FF)
        || (0x0F_0000 <= c && c <= 0x0F_FFFD)
        || (0x10_0000 <= c && c <= 0x10_FFFD);
}

/++
    Returns whether `c` is a Unicode surrogate $(CODEPOINT)
    (general Unicode category: Cs).
+/
@safe pure nothrow @nogc
bool isSurrogate(dchar c)
{
    return (0xD800 <= c && c <= 0xDFFF);
}

/++
    Returns whether `c` is a Unicode high surrogate (lead surrogate).
+/
@safe pure nothrow @nogc
bool isSurrogateHi(dchar c)
{
    return (0xD800 <= c && c <= 0xDBFF);
}

/++
    Returns whether `c` is a Unicode low surrogate (trail surrogate).
+/
@safe pure nothrow @nogc
bool isSurrogateLo(dchar c)
{
    return (0xDC00 <= c && c <= 0xDFFF);
}

/++
    Returns whether `c` is a Unicode non-character i.e.
    a $(CODEPOINT) with no assigned abstract character.
    (general Unicode category: Cn)
+/
@safe pure nothrow @nogc
bool isNonCharacter(dchar c)
{
    return nonCharacterTrie[c];
}

@safe unittest
{
    auto set = unicode("Cn");
    foreach (ch; set.byCodepoint)
        assert(isNonCharacter(ch));
}

private:
// load static data from pre-generated tables into usable datastructures


@safe auto asSet(const (ubyte)[] compressed) pure
{
    return CodepointSet.fromIntervals(decompressIntervals(compressed));
}

@safe pure nothrow auto asTrie(T...)(const scope TrieEntry!T e)
{
    return const(CodepointTrie!T)(e.offsets, e.sizes, e.data);
}

@safe pure nothrow @nogc @property
{
    // It's important to use auto return here, so that the compiler
    // only runs semantic on the return type if the function gets
    // used. Also these are functions rather than templates to not
    // increase the object size of the caller.
    auto lowerCaseTrie() { static immutable res = asTrie(lowerCaseTrieEntries); return res; }
    auto upperCaseTrie() { static immutable res = asTrie(upperCaseTrieEntries); return res; }
    auto simpleCaseTrie() { static immutable res = asTrie(simpleCaseTrieEntries); return res; }
    auto fullCaseTrie() { static immutable res = asTrie(fullCaseTrieEntries); return res; }
    auto alphaTrie() { static immutable res = asTrie(alphaTrieEntries); return res; }
    auto markTrie() { static immutable res = asTrie(markTrieEntries); return res; }
    auto numberTrie() { static immutable res = asTrie(numberTrieEntries); return res; }
    auto punctuationTrie() { static immutable res = asTrie(punctuationTrieEntries); return res; }
    auto symbolTrie() { static immutable res = asTrie(symbolTrieEntries); return res; }
    auto graphicalTrie() { static immutable res = asTrie(graphicalTrieEntries); return res; }
    auto nonCharacterTrie() { static immutable res = asTrie(nonCharacterTrieEntries); return res; }

    //normalization quick-check tables
    auto nfcQCTrie()
    {
        import std.internal.unicode_norm : nfcQCTrieEntries;
        static immutable res = asTrie(nfcQCTrieEntries);
        return res;
    }

    auto nfdQCTrie()
    {
        import std.internal.unicode_norm : nfdQCTrieEntries;
        static immutable res = asTrie(nfdQCTrieEntries);
        return res;
    }

    auto nfkcQCTrie()
    {
        import std.internal.unicode_norm : nfkcQCTrieEntries;
        static immutable res = asTrie(nfkcQCTrieEntries);
        return res;
    }

    auto nfkdQCTrie()
    {
        import std.internal.unicode_norm : nfkdQCTrieEntries;
        static immutable res = asTrie(nfkdQCTrieEntries);
        return res;
    }

    //grapheme breaking algorithm tables
    auto spacingMarkTrie()
    {
        import std.internal.unicode_grapheme : spacingMarkTrieEntries;
        static immutable res = asTrie(spacingMarkTrieEntries);
        return res;
    }

    auto graphemeExtendTrie()
    {
        import std.internal.unicode_grapheme : graphemeExtendTrieEntries;
        static immutable res = asTrie(graphemeExtendTrieEntries);
        return res;
    }

    auto hangLV()
    {
        import std.internal.unicode_grapheme : hangulLVTrieEntries;
        static immutable res = asTrie(hangulLVTrieEntries);
        return res;
    }

    auto hangLVT()
    {
        import std.internal.unicode_grapheme : hangulLVTTrieEntries;
        static immutable res = asTrie(hangulLVTTrieEntries);
        return res;
    }

    auto prependTrie()
    {
        import std.internal.unicode_grapheme : prependTrieEntries;
        static immutable res = asTrie(prependTrieEntries);
        return res;
    }

    auto graphemeControlTrie()
    {
        import std.internal.unicode_grapheme : controlTrieEntries;
        static immutable res = asTrie(controlTrieEntries);
        return res;
    }

    auto xpictoTrie()
    {
        import std.internal.unicode_grapheme : Extended_PictographicTrieEntries;
        static immutable res = asTrie(Extended_PictographicTrieEntries);
        return res;
    }

    // tables below are used for composition/decomposition
    auto combiningClassTrie()
    {
        import std.internal.unicode_comp : combiningClassTrieEntries;
        static immutable res = asTrie(combiningClassTrieEntries);
        return res;
    }

    auto compatMappingTrie()
    {
        import std.internal.unicode_decomp : compatMappingTrieEntries;
        static immutable res = asTrie(compatMappingTrieEntries);
        return res;
    }

    auto canonMappingTrie()
    {
        import std.internal.unicode_decomp : canonMappingTrieEntries;
        static immutable res = asTrie(canonMappingTrieEntries);
        return res;
    }

    auto compositionJumpTrie()
    {
        import std.internal.unicode_comp : compositionJumpTrieEntries;
        static immutable res = asTrie(compositionJumpTrieEntries);
        return res;
    }

    //case conversion tables
    auto toUpperIndexTrie() { static immutable res = asTrie(toUpperIndexTrieEntries); return res; }
    auto toLowerIndexTrie() { static immutable res = asTrie(toLowerIndexTrieEntries); return res; }
    auto toTitleIndexTrie() { static immutable res = asTrie(toTitleIndexTrieEntries); return res; }
    //simple case conversion tables
    auto toUpperSimpleIndexTrie() { static immutable res = asTrie(toUpperSimpleIndexTrieEntries); return res; }
    auto toLowerSimpleIndexTrie() { static immutable res = asTrie(toLowerSimpleIndexTrieEntries); return res; }
    auto toTitleSimpleIndexTrie() { static immutable res = asTrie(toTitleSimpleIndexTrieEntries); return res; }

}

}// version (!std_uni_bootstrap)
