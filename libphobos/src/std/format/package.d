// Written in the D programming language.

/**
This package provides string formatting functionality using
`printf` style format strings.

$(BOOKTABLE ,
$(TR $(TH Submodule) $(TH Function Name) $(TH Description))
$(TR
    $(TD $(I package))
    $(TD $(LREF format))
    $(TD Converts its arguments according to a format string into a string.)
)
$(TR
    $(TD $(I package))
    $(TD $(LREF sformat))
    $(TD Converts its arguments according to a format string into a buffer.)
)
$(TR
    $(TD $(I package))
    $(TD $(LREF FormatException))
    $(TD Signals a problem while formatting.)
)
$(TR
    $(TD $(MREF_ALTTEXT $(D write), std, format, write))
    $(TD $(REF_ALTTEXT $(D formattedWrite), formattedWrite, std, format, write))
    $(TD Converts its arguments according to a format string and writes
         the result to an output range.)
)
$(TR
    $(TD $(MREF_ALTTEXT $(D write), std, format, write))
    $(TD $(REF_ALTTEXT $(D formatValue), formatValue, std, format, write))
    $(TD Formats a value of any type according to a format specifier and
         writes the result to an output range.)
)
$(TR
    $(TD $(MREF_ALTTEXT $(D read), std, format, read))
    $(TD $(REF_ALTTEXT $(D formattedRead), formattedRead, std, format, read))
    $(TD Reads an input range according to a format string and stores the read
         values into its arguments.)
)
$(TR
    $(TD $(MREF_ALTTEXT $(D read), std, format, read))
    $(TD $(REF_ALTTEXT $(D unformatValue), unformatValue, std, format, read))
    $(TD Reads a value from the given input range and converts it according to
         a format specifier.)
)
$(TR
    $(TD $(MREF_ALTTEXT $(D spec), std, format, spec))
    $(TD $(REF_ALTTEXT $(D FormatSpec), FormatSpec, std, format, spec))
    $(TD A general handler for format strings.)
)
$(TR
    $(TD $(MREF_ALTTEXT $(D spec), std, format, spec))
    $(TD $(REF_ALTTEXT $(D singleSpec), singleSpec, std, format, spec))
    $(TD Helper function that returns a `FormatSpec` for a single format specifier.)
))

Limitation: This package does not support localization, but
    adheres to the rounding mode of the floating point unit, if
    available.

$(H3 $(LNAME2 format-strings, Format Strings))

The functions contained in this package use $(I format strings). A
format string describes the layout of another string for reading or
writing purposes. A format string is composed of normal text
interspersed with $(I format specifiers). A format specifier starts
with a percentage sign $(B '%'), optionally followed by one or more
$(I parameters) and ends with a $(I format indicator). A format
indicator may be a simple $(I format character) or a $(I compound
indicator).

$(I Format strings) are composed according to the following grammar:

$(PRE
$(I FormatString):
    $(I FormatStringItem) $(I FormatString)
$(I FormatStringItem):
    $(I Character)
    $(I FormatSpecifier)
$(I FormatSpecifier):
    $(B '%') $(I Parameters) $(I FormatIndicator)

$(I FormatIndicator):
    $(I FormatCharacter)
    $(I CompoundIndicator)
$(I FormatCharacter):
    $(I see remark below)
$(I CompoundIndicator):
    $(B '$(LPAREN)') $(I FormatString) $(B '%$(RPAREN)')
    $(B '$(LPAREN)') $(I FormatString) $(B '%|') $(I Delimiter) $(B '%$(RPAREN)')
$(I Delimiter)
    $(I empty)
    $(I Character) $(I Delimiter)

$(I Parameters):
    $(I Position) $(I Flags) $(I Width) $(I Precision) $(I Separator)
$(I Position):
    $(I empty)
    $(I Integer) $(B '$')
    $(I Integer) $(B ':') $(I Integer) $(B '$')
    $(I Integer) $(B ':') $(B '$')
$(I Flags):
    $(I empty)
    $(I Flag) $(I Flags)
$(I Flag):
    $(B '-')|$(B '+')|$(B '&nbsp;')|$(B '0')|$(B '#')|$(B '=')
$(I Width):
    $(I OptionalPositionalInteger)
$(I Precision):
    $(I empty)
    $(B '.') $(I OptionalPositionalInteger)
$(I Separator):
    $(I empty)
    $(B ',') $(I OptionalInteger)
    $(B ',') $(I OptionalInteger) $(B '?')
$(I OptionalInteger):
    $(I empty)
    $(I Integer)
    $(B '*')
$(I OptionalPositionalInteger):
    $(I OptionalInteger)
    $(B '*') $(I Integer) $(B '$')

$(I Character)
    $(B '%%')
    $(I AnyCharacterExceptPercent)
$(I Integer):
    $(I NonZeroDigit) $(I Digits)
$(I Digits):
    $(I empty)
    $(I Digit) $(I Digits)
$(I NonZeroDigit):
    $(B '1')|$(B '2')|$(B '3')|$(B '4')|$(B '5')|$(B '6')|$(B '7')|$(B '8')|$(B '9')
$(I Digit):
    $(B '0')|$(B '1')|$(B '2')|$(B '3')|$(B '4')|$(B '5')|$(B '6')|$(B '7')|$(B '8')|$(B '9')
)

Note: $(I FormatCharacter) is unspecified. It can be any character
that has no other purpose in this grammar, but it is
recommended to assign (lower- and uppercase) letters.

Note: The $(I Parameters) of a $(I CompoundIndicator) are currently
limited to a $(B '-') flag.

$(H4 $(LNAME2 format-indicator, Format Indicator))

The $(I format indicator) can either be a single character or an
expression surrounded by $(B '%$(LPAREN)') and $(B '%$(RPAREN)'). It specifies the
basic manner in which a value will be formatted and is the minimum
requirement to format a value.

The following characters can be used as $(I format characters):

$(BOOKTABLE ,
   $(TR $(TH FormatCharacter) $(TH Semantics))
   $(TR $(TD $(B 's'))
        $(TD To be formatted in a human readable format.
             Can be used with all types.))
   $(TR $(TD $(B 'c'))
        $(TD To be formatted as a character.))
   $(TR $(TD $(B 'd'))
        $(TD To be formatted as a signed decimal integer.))
   $(TR $(TD $(B 'u'))
        $(TD To be formatted as a decimal image of the underlying bit representation.))
   $(TR $(TD $(B 'b'))
        $(TD To be formatted as a binary image of the underlying bit representation.))
   $(TR $(TD $(B 'o'))
        $(TD To be formatted as an octal image of the underlying bit representation.))
   $(TR $(TD $(B 'x') / $(B 'X'))
        $(TD To be formatted as a hexadecimal image of the underlying bit representation.))
   $(TR $(TD $(B 'e') / $(B 'E'))
        $(TD To be formatted as a real number in decimal scientific notation.))
   $(TR $(TD $(B 'f') / $(B 'F'))
        $(TD To be formatted as a real number in decimal natural notation.))
   $(TR $(TD $(B 'g') / $(B 'G'))
        $(TD To be formatted as a real number in decimal short notation.
             Depending on the number, a scientific notation or
             a natural notation is used.))
   $(TR $(TD $(B 'a') / $(B 'A'))
        $(TD To be formatted as a real number in hexadecimal scientific notation.))
   $(TR $(TD $(B 'r'))
        $(TD To be formatted as raw bytes.
             The output may not be printable and depends on endianness.))
)

The $(I compound indicator) can be used to describe compound types
like arrays or structs in more detail. A compound type is enclosed
within $(B '%$(LPAREN)') and $(B '%$(RPAREN)'). The enclosed sub-format string is
applied to individual elements. The trailing portion of the
sub-format string following the specifier for the element is
interpreted as the delimiter, and is therefore omitted following the
last element. The $(B '%|') specifier may be used to explicitly
indicate the start of the delimiter, so that the preceding portion of
the string will be included following the last element.

The $(I format string) inside of the $(I compound indicator) should
contain exactly one $(I format specifier) (two in case of associative
arrays), which specifies the formatting mode of the elements of the
compound type. This $(I format specifier) can be a $(I compound
indicator) itself.

Note: Inside a $(I compound indicator), strings and characters are
escaped automatically. To avoid this behavior, use `"%-$(LPAREN)"`
instead of `"%$(LPAREN)"`.

$(H4 $(LNAME2 flags, Flags))

There are several flags that affect the outcome of the formatting.

$(BOOKTABLE ,
   $(TR $(TH Flag) $(TH Semantics))
   $(TR $(TD $(B '-'))
        $(TD When the formatted result is shorter than the value
             given by the width parameter, the output is left
             justified. Without the $(B '-') flag, the output remains
             right justified.

             There are two exceptions where the $(B '-') flag has a
             different meaning: (1) with $(B 'r') it denotes to use little
             endian and (2) in case of a compound indicator it means that
             no special handling of the members is applied.))
   $(TR $(TD $(B '='))
        $(TD When the formatted result is shorter than the value
             given by the width parameter, the output is centered.
             If the central position is not possible it is moved slightly
             to the right. In this case, if $(B '-') flag is present in
             addition to the $(B '=') flag, it is moved slightly to the left.))
   $(TR $(TD $(B '+')&nbsp;/&nbsp;$(B '&nbsp;'))
        $(TD Applies to numerical values. By default, positive numbers are not
             formatted to include the `+` sign. With one of these two flags present,
             positive numbers are preceded by a plus sign or a space.
             When both flags are present, a plus sign is used.

             In case of $(B 'r'), a big endian format is used.))
   $(TR $(TD $(B '0'))
        $(TD Is applied to numerical values that are printed right justified.
             If the zero flag is present, the space left to the number is
             filled with zeros instead of spaces.))
   $(TR $(TD $(B '#'))
        $(TD Denotes that an alternative output must be used. This depends on the type
             to be formatted and the $(I format character) used. See the
             sections below for more information.))
)

$(H4 $(LNAME2 width-precision-separator, Width, Precision and Separator))

The $(I width) parameter specifies the minimum width of the result.

The meaning of $(I precision) depends on the format indicator. For
integers it denotes the minimum number of digits printed, for
real numbers it denotes the number of fractional digits and for
strings and compound types it denotes the maximum number of elements
that are included in the output.

A $(I separator) is used for formatting numbers. If it is specified,
the output is divided into chunks of three digits, separated by a $(B
','). The number of digits in a chunk can be given explicitly by
providing a number or a $(B '*') after the $(B ',').

In all three cases the number of digits can be replaced by a $(B
'*'). In this scenario, the next argument is used as the number of
digits. If the argument is a negative number, the $(I precision) and
$(I separator) parameters are considered unspecified. For $(I width),
the absolute value is used and the $(B '-') flag is set.

The $(I separator) can also be followed by a $(B '?'). In that case,
an additional argument is used to specify the symbol that should be
used to separate the chunks.

$(H4 $(LNAME2 position, Position))

By default, the arguments are processed in the provided order. With
the $(I position) parameter it is possible to address arguments
directly. It is also possible to denote a series of arguments with
two numbers separated by $(B ':'), that are all processed in the same
way. The second number can be omitted. In that case the series ends
with the last argument.

It's also possible to use positional arguments for $(I width), $(I
precision) and $(I separator) by adding a number and a $(B
'$(DOLLAR)') after the $(B '*').

$(H4 $(LNAME2 types, Types))

This section describes the result of combining types with format
characters. It is organized in 2 subsections: a list of general
information regarding the formatting of types in the presence of
format characters and a table that contains details for every
available combination of type and format character.

When formatting types, the following rules apply:

$(UL
  $(LI If the format character is upper case, the resulting string will
       be formatted using upper case letters.)
  $(LI The default precision for floating point numbers is 6 digits.)
  $(LI Rounding of floating point numbers adheres to the rounding mode
       of the floating point unit, if available.)
  $(LI The floating point values `NaN` and `Infinity` are formatted as
       `nan` and `inf`, possibly preceded by $(B '+') or $(B '-') sign.)
  $(LI Formatting reals is only supported for 64 bit reals and 80 bit reals.
       All other reals are cast to double before they are formatted. This will
       cause the result to be `inf` for very large numbers.)
  $(LI Characters and strings formatted with the $(B 's') format character
       inside of compound types are surrounded by single and double quotes
       and unprintable characters are escaped. To avoid this, a $(B '-')
       flag can be specified for the compound specifier
       $(LPAREN)e.g. `"%-$(LPAREN)%s%$(RPAREN)"` instead of `"%$(LPAREN)%s%$(RPAREN)"` $(RPAREN).)
  $(LI Structs, unions, classes and interfaces are formatted by calling a
       `toString` method if available.
       See $(MREF_ALTTEXT $(D module std.format.write), std, format, write) for more
       details.)
  $(LI Only part of these combinations can be used for reading. See
       $(MREF_ALTTEXT $(D module std.format.read), std, format, read) for more
       detailed information.)
)

This table contains descriptions for every possible combination of
type and format character:

$(BOOKTABLE ,
   $(TR $(THMINWIDTH Type) $(THMINWIDTH Format Character) $(TH Formatted as...))
   $(TR $(MULTIROW_CELL 1, `null`)
        $(TD $(B 's'))
            $(TD `null`)
   )
   $(TR $(MULTIROW_CELL 3, `bool`)
        $(TD $(B 's'))
            $(TD `false` or `true`)
   )
   $(TR $(TD $(B 'b'), $(B 'd'), $(B 'o'), $(B 'u'), $(B 'x'), $(B 'X'))
            $(TD As the integrals 0 or 1 with the same format character.

            $(I Please note, that $(B 'o') and $(B 'x') with $(B '#') flag
            might produce unexpected results due to special handling of
            the value 0.))
   )
   $(TR $(TD $(B 'r'))
            $(TD `\0` or `\1`)
   )
   $(TR $(MULTIROW_CELL 4, $(I Integral))
        $(TD $(B 's'), $(B 'd'))
            $(TD A signed decimal number. The $(B '#') flag is ignored.)
   )
   $(TR $(TD $(B 'b'), $(B 'o'), $(B 'u'), $(B 'x'), $(B 'X'))
            $(TD An unsigned binary, decimal, octal or hexadecimal number.

                 In case of $(B 'o') and $(B 'x'), the $(B '#') flag
                 denotes that the number must be preceded by `0` and `0x`, with
                 the exception of the value 0, where this does not apply. For
                 $(B 'b') and $(B 'u') the $(B '#') flag has no effect.)
   )
   $(TR $(TD $(B 'e'), $(B 'E'), $(B 'f'), $(B 'F'), $(B 'g'), $(B 'G'), $(B 'a'), $(B 'A'))
            $(TD As a floating point value with the same specifier.

                 Default precision is large enough to add all digits
                 of the integral value.

                 In case of $(B 'a') and $(B 'A'), the integral digit can be
                 any hexadecimal digit.
               )
   )
   $(TR $(TD $(B 'r'))
            $(TD Characters taken directly from the binary representation.)
   )
   $(TR $(MULTIROW_CELL 5, $(I Floating Point))
        $(TD $(B 'e'), $(B 'E'))
            $(TD Scientific notation: Exactly one integral digit followed by a dot
                 and fractional digits, followed by the exponent.
                 The exponent is formatted as $(B 'e') followed by
                 a $(B '+') or $(B '-') sign, followed by at least
                 two digits.

                 When there are no fractional digits and the $(B '#') flag
                 is $(I not) present, the dot is omitted.)
   )
   $(TR $(TD $(B 'f'), $(B 'F'))
            $(TD Natural notation: Integral digits followed by a dot and
                 fractional digits.

                 When there are no fractional digits and the $(B '#') flag
                 is $(I not) present, the dot is omitted.

                 $(I Please note: the difference between $(B 'f') and $(B 'F')
                 is only visible for `NaN` and `Infinity`.))
   )
   $(TR $(TD $(B 's'), $(B 'g'), $(B 'G'))
            $(TD Short notation: If the absolute value is larger than `10 ^^ precision`
                 or smaller than `0.0001`, the scientific notation is used.
                 If not, the natural notation is applied.

                 In both cases $(I precision) denotes the count of all digits, including
                 the integral digits. Trailing zeros (including a trailing dot) are removed.

                 If $(B '#') flag is present, trailing zeros are not removed.)
   )
   $(TR $(TD $(B 'a'), $(B 'A'))
            $(TD Hexadecimal scientific notation: `0x` followed by `1`
                 (or `0` in case of value zero or denormalized number)
                 followed by a dot, fractional digits in hexadecimal
                 notation and an exponent. The exponent is build by `p`,
                 followed by a sign and the exponent in $(I decimal) notation.

                 When there are no fractional digits and the $(B '#') flag
                 is $(I not) present, the dot is omitted.)
   )
   $(TR $(TD $(B 'r'))
            $(TD Characters taken directly from the binary representation.)
   )
   $(TR $(MULTIROW_CELL 3, $(I Character))
        $(TD $(B 's'), $(B 'c'))
            $(TD As the character.

                 Inside of a compound indicator $(B 's') is treated differently: The
                 character is surrounded by single quotes and non printable
                 characters are escaped. This can be avoided by preceding
                 the compound indicator with a $(B '-') flag
                 $(LPAREN)e.g. `"%-$(LPAREN)%s%$(RPAREN)"`$(RPAREN).)
   )
   $(TR $(TD $(B 'b'), $(B 'd'), $(B 'o'), $(B 'u'), $(B 'x'), $(B 'X'))
            $(TD As the integral that represents the character.)
   )
   $(TR $(TD $(B 'r'))
            $(TD Characters taken directly from the binary representation.)
   )
   $(TR $(MULTIROW_CELL 3, $(I String))
        $(TD $(B 's'))
            $(TD The sequence of characters that form the string.

                 Inside of a compound indicator the string is surrounded by double quotes
                 and non printable characters are escaped. This can be avoided
                 by preceding the compound indicator with a $(B '-') flag
                 $(LPAREN)e.g. `"%-$(LPAREN)%s%$(RPAREN)"`$(RPAREN).)
   )
   $(TR $(TD $(B 'r'))
            $(TD The sequence of characters, each formatted with $(B 'r').)
   )
   $(TR $(TD compound)
            $(TD As an array of characters.)
   )
   $(TR $(MULTIROW_CELL 3, $(I Array))
        $(TD $(B 's'))
            $(TD When the elements are characters, the array is formatted as
                 a string. In all other cases the array is surrounded by square brackets
                 and the elements are separated by a comma and a space. If the elements
                 are strings, they are surrounded by double quotes and non
                 printable characters are escaped.)
   )
   $(TR $(TD $(B 'r'))
            $(TD The sequence of the elements, each formatted with $(B 'r').)
   )
   $(TR $(TD compound)
            $(TD The sequence of the elements, each formatted according to the specifications
                 given inside of the compound specifier.)
   )
   $(TR $(MULTIROW_CELL 2, $(I Associative Array))
        $(TD $(B 's'))
            $(TD As a sequence of the elements in unpredictable order. The output is
                 surrounded by square brackets. The elements are separated by a
                 comma and a space. The elements are formatted as `key:value`.)
   )
   $(TR $(TD compound)
            $(TD As a sequence of the elements in unpredictable order. Each element
                 is formatted according to the specifications given inside of the
                 compound specifier. The first specifier is used for formatting
                 the key and the second specifier is used for formatting the value.
                 The order can be changed with positional arguments. For example
                 `"%(%2$s (%1$s), %)"` will write the value, followed by the key in
                 parenthesis.)
   )
   $(TR $(MULTIROW_CELL 2, $(I Enum))
        $(TD $(B 's'))
            $(TD The name of the value. If the name is not available, the base value
                 is used, preceeded by a cast.)
   )
   $(TR $(TD All, but $(B 's'))
            $(TD Enums can be formatted with all format characters that can be used
                 with the base value. In that case they are formatted like the base value.)
   )
   $(TR $(MULTIROW_CELL 3, $(I Input Range))
        $(TD $(B 's'))
            $(TD When the elements of the range are characters, they are written like a string.
                 In all other cases, the elements are enclosed by square brackets and separated
                 by a comma and a space.)
   )
   $(TR $(TD $(B 'r'))
            $(TD The sequence of the elements, each formatted with $(B 'r').)
   )
   $(TR $(TD compound)
            $(TD The sequence of the elements, each formatted according to the specifications
                 given inside of the compound specifier.)
   )
   $(TR $(MULTIROW_CELL 1, $(I Struct))
        $(TD $(B 's'))
            $(TD When the struct has neither an applicable `toString`
                 nor is an input range, it is formatted as follows:
                 `StructType(field1, field2, ...)`.)
   )
   $(TR $(MULTIROW_CELL 1, $(I Class))
        $(TD $(B 's'))
            $(TD When the class has neither an applicable `toString`
                 nor is an input range, it is formatted as the
                 fully qualified name of the class.)
   )
   $(TR $(MULTIROW_CELL 1, $(I Union))
        $(TD $(B 's'))
            $(TD When the union has neither an applicable `toString`
                 nor is an input range, it is formatted as its base name.)
   )
   $(TR $(MULTIROW_CELL 2, $(I Pointer))
        $(TD $(B 's'))
            $(TD A null pointer is formatted as 'null'. All other pointers are
                 formatted as hexadecimal numbers with the format character $(B 'X').)
   )
   $(TR $(TD $(B 'x'), $(B 'X'))
            $(TD Formatted as a hexadecimal number.)
   )
   $(TR $(MULTIROW_CELL 3, $(I SIMD vector))
        $(TD $(B 's'))
            $(TD The array is surrounded by square brackets
                 and the elements are separated by a comma and a space.)
   )
   $(TR $(TD $(B 'r'))
            $(TD The sequence of the elements, each formatted with $(B 'r').)
   )
   $(TR $(TD compound)
            $(TD The sequence of the elements, each formatted according to the specifications
                 given inside of the compound specifier.)
   )
   $(TR $(MULTIROW_CELL 1, $(I Delegate))
        $(TD $(B 's'), $(B 'r'), compound)
            $(TD As the `.stringof` of this delegate treated as a string.

                 $(I Please note: The implementation is currently buggy
                 and its use is discouraged.))
   )
)

Copyright: Copyright The D Language Foundation 2000-2021.

Macros:
SUBREF = $(REF_ALTTEXT $2, $2, std, format, $1)$(NBSP)
MULTIROW_CELL = <td rowspan="$1">$+</td>
THMINWIDTH = <th scope="col" width="20%">$0</th>

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP walterbright.com, Walter Bright), $(HTTP erdani.com,
Andrei Alexandrescu), and Kenji Hara

Source: $(PHOBOSSRC std/format/package.d)
 */
module std.format;

/// Simple use:
@safe unittest
{
    // Easiest way is to use `%s` everywhere:
    assert(format("I got %s %s for %s euros.", 30, "eggs", 5.27) == "I got 30 eggs for 5.27 euros.");

    // Other format characters provide more control:
    assert(format("I got %b %(%X%) for %f euros.", 30, "eggs", 5.27) == "I got 11110 65676773 for 5.270000 euros.");
}

/// Compound specifiers allow formatting arrays and other compound types:
@safe unittest
{
/*
The trailing end of the sub-format string following the specifier for
each item is interpreted as the array delimiter, and is therefore
omitted following the last array item:
 */
    assert(format("My items are %(%s %).", [1,2,3]) == "My items are 1 2 3.");
    assert(format("My items are %(%s, %).", [1,2,3]) == "My items are 1, 2, 3.");

/*
The "%|" delimiter specifier may be used to indicate where the
delimiter begins, so that the portion of the format string prior to
it will be retained in the last array element:
 */
    assert(format("My items are %(-%s-%|, %).", [1,2,3]) == "My items are -1-, -2-, -3-.");

/*
These compound format specifiers may be nested in the case of a
nested array argument:
 */
    auto mat = [[1, 2, 3],
                [4, 5, 6],
                [7, 8, 9]];

    assert(format("%(%(%d %) - %)", mat), "1 2 3 - 4 5 6 - 7 8 9");
    assert(format("[%(%(%d %) - %)]", mat), "[1 2 3 - 4 5 6 - 7 8 9]");
    assert(format("[%([%(%d %)]%| - %)]", mat), "[1 2 3] - [4 5 6] - [7 8 9]");

/*
Strings and characters are escaped automatically inside compound
format specifiers. To avoid this behavior, use "%-(" instead of "%(":
 */
    assert(format("My friends are %s.", ["John", "Nancy"]) == `My friends are ["John", "Nancy"].`);
    assert(format("My friends are %(%s, %).", ["John", "Nancy"]) == `My friends are "John", "Nancy".`);
    assert(format("My friends are %-(%s, %).", ["John", "Nancy"]) == `My friends are John, Nancy.`);
}

/// Using parameters:
@safe unittest
{
    // Flags can be used to influence to outcome:
    assert(format("%g != %+#g", 3.14, 3.14) == "3.14 != +3.14000");

    // Width and precision help to arrange the formatted result:
    assert(format(">%10.2f<", 1234.56789) == ">   1234.57<");

    // Numbers can be grouped:
    assert(format("%,4d", int.max) == "21,4748,3647");

    // It's possible to specify the position of an argument:
    assert(format("%3$s %1$s", 3, 17, 5) == "5 3");
}

/// Providing parameters as arguments:
@safe unittest
{
    // Width as argument
    assert(format(">%*s<", 10, "abc") == ">       abc<");

    // Precision as argument
    assert(format(">%.*f<", 5, 123.2) == ">123.20000<");

    // Grouping as argument
    assert(format("%,*d", 1, int.max) == "2,1,4,7,4,8,3,6,4,7");

    // Grouping separator as argument
    assert(format("%,3?d", '_', int.max) == "2_147_483_647");

    // All at once
    assert(format("%*.*,*?d", 20, 15, 6, '/', int.max) == "   000/002147/483647");
}

public import std.format.read;
public import std.format.spec;
public import std.format.write;

import std.exception : enforce;
import std.range.primitives : isInputRange;
import std.traits : CharTypeOf, isSomeChar, isSomeString, StringTypeOf;
import std.format.internal.write : hasToString;

/**
Signals an issue encountered while formatting.
 */
class FormatException : Exception
{
    /// Generic constructor.
    @safe @nogc pure nothrow
    this()
    {
        super("format error");
    }

    /**
       Creates a new instance of `FormatException`.

       Params:
           msg = message of the exception
           fn = file name of the file where the exception was created (optional)
           ln = line number of the file where the exception was created (optional)
           next = for internal use, should always be null (optional)
     */
    @safe @nogc pure nothrow
    this(string msg, string fn = __FILE__, size_t ln = __LINE__, Throwable next = null)
    {
        super(msg, fn, ln, next);
    }
}

///
@safe unittest
{
    import std.exception : assertThrown;

    assertThrown!FormatException(format("%d", "foo"));
}

package alias enforceFmt = enforce!FormatException;

// @@@DEPRECATED_[2.107.0]@@@
deprecated("formatElement was accidentally made public and will be removed in 2.107.0")
void formatElement(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(StringTypeOf!T) && !hasToString!(T, Char) && !is(T == enum))
{
    import std.format.internal.write : fe = formatElement;

    fe(w, val, f);
}

// @@@DEPRECATED_[2.107.0]@@@
deprecated("formatElement was accidentally made public and will be removed in 2.107.0")
void formatElement(Writer, T, Char)(auto ref Writer w, T val, scope const ref FormatSpec!Char f)
if (is(CharTypeOf!T) && !is(T == enum))
{
    import std.format.internal.write : fe = formatElement;

    fe(w, val, f);
}

// @@@DEPRECATED_[2.107.0]@@@
deprecated("formatElement was accidentally made public and will be removed in 2.107.0")
void formatElement(Writer, T, Char)(auto ref Writer w, auto ref T val, scope const ref FormatSpec!Char f)
if ((!is(StringTypeOf!T) || hasToString!(T, Char)) && !is(CharTypeOf!T) || is(T == enum))
{
    import std.format.internal.write : fe = formatElement;

    fe(w, val, f);
}

// Like NullSink, but toString() isn't even called at all. Used to test the format string.
package struct NoOpSink
{
    void put(E)(scope const E) pure @safe @nogc nothrow {}
}

// @@@DEPRECATED_[2.107.0]@@@
deprecated("unformatElement was accidentally made public and will be removed in 2.107.0")
T unformatElement(T, Range, Char)(ref Range input, scope const ref FormatSpec!Char spec)
if (isInputRange!Range)
{
    import std.format.internal.read : ue = unformatElement;

    return ue(input, spec);
}

// Used to check format strings are compatible with argument types
package(std) enum checkFormatException(alias fmt, Args...) =
{
    import std.conv : text;

    try
    {
        auto n = .formattedWrite(NoOpSink(), fmt, Args.init);

        enforceFmt(n == Args.length, text("Orphan format arguments: args[", n, "..", Args.length, "]"));
    }
    catch (Exception e)
        return e.msg;
    return null;
}();

/**
Converts its arguments according to a format string into a string.

The second version of `format` takes the format string as template
argument. In this case, it is checked for consistency at
compile-time and produces slightly faster code, because the length of
the output buffer can be estimated in advance.

Params:
    fmt = a $(MREF_ALTTEXT format string, std,format)
    args = a variadic list of arguments to be formatted
    Char = character type of `fmt`
    Args = a variadic list of types of the arguments

Returns:
    The formatted string.

Throws:
    A $(LREF FormatException) if formatting did not succeed.

See_Also:
    $(LREF sformat) for a variant, that tries to avoid garbage collection.
 */
immutable(Char)[] format(Char, Args...)(in Char[] fmt, Args args)
if (isSomeChar!Char)
{
    import std.array : appender;

    auto w = appender!(immutable(Char)[]);
    auto n = formattedWrite(w, fmt, args);
    version (all)
    {
        // In the future, this check will be removed to increase consistency
        // with formattedWrite
        import std.conv : text;
        enforceFmt(n == args.length, text("Orphan format arguments: args[", n, "..", args.length, "]"));
    }
    return w.data;
}

///
@safe pure unittest
{
    assert(format("Here are %d %s.", 3, "apples") == "Here are 3 apples.");

    assert("Increase: %7.2f %%".format(17.4285) == "Increase:   17.43 %");
}

@safe pure unittest
{
    import std.exception : assertCTFEable, assertThrown;

    assertCTFEable!(
    {
        assert(format("foo") == "foo");
        assert(format("foo%%") == "foo%");
        assert(format("foo%s", 'C') == "fooC");
        assert(format("%s foo", "bar") == "bar foo");
        assert(format("%s foo %s", "bar", "abc") == "bar foo abc");
        assert(format("foo %d", -123) == "foo -123");
        assert(format("foo %d", 123) == "foo 123");

        assertThrown!FormatException(format("foo %s"));
        assertThrown!FormatException(format("foo %s", 123, 456));

        assert(format("hel%slo%s%s%s", "world", -138, 'c', true) == "helworldlo-138ctrue");
    });

    assert(is(typeof(format("happy")) == string));
    assert(is(typeof(format("happy"w)) == wstring));
    assert(is(typeof(format("happy"d)) == dstring));
}

// https://issues.dlang.org/show_bug.cgi?id=16661
@safe pure unittest
{
    assert(format("%.2f"d, 0.4) == "0.40");
    assert("%02d"d.format(1) == "01"d);
}

@safe unittest
{
    int i;
    string s;

    s = format("hello world! %s %s %s%s%s", true, 57, 1_000_000_000, 'x', " foo");
    assert(s == "hello world! true 57 1000000000x foo");

    s = format("%s %A %s", 1.67, -1.28, float.nan);
    assert(s == "1.67 -0X1.47AE147AE147BP+0 nan", s);

    s = format("%x %X", 0x1234AF, 0xAFAFAFAF);
    assert(s == "1234af AFAFAFAF");

    s = format("%b %o", 0x1234AF, 0xAFAFAFAF);
    assert(s == "100100011010010101111 25753727657");

    s = format("%d %s", 0x1234AF, 0xAFAFAFAF);
    assert(s == "1193135 2947526575");
}

@safe unittest
{
    import std.conv : octal;

    string s;
    int i;

    s = format("%#06.*f", 2, 12.345);
    assert(s == "012.35");

    s = format("%#0*.*f", 6, 2, 12.345);
    assert(s == "012.35");

    s = format("%7.4g:", 12.678);
    assert(s == "  12.68:");

    s = format("%7.4g:", 12.678L);
    assert(s == "  12.68:");

    s = format("%04f|%05d|%#05x|%#5x", -4.0, -10, 1, 1);
    assert(s == "-4.000000|-0010|0x001|  0x1");

    i = -10;
    s = format("%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(s == "-10|-10|-10|-10|-10.0000");

    i = -5;
    s = format("%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(s == "-5| -5|-05|-5|-5.0000");

    i = 0;
    s = format("%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(s == "0|  0|000|0|0.0000");

    i = 5;
    s = format("%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(s == "5|  5|005|5|5.0000");

    i = 10;
    s = format("%d|%3d|%03d|%1d|%01.4f", i, i, i, i, cast(double) i);
    assert(s == "10| 10|010|10|10.0000");

    s = format("%.0d", 0);
    assert(s == "0");

    s = format("%.g", .34);
    assert(s == "0.3");

    s = format("%.0g", .34);
    assert(s == "0.3");

    s = format("%.2g", .34);
    assert(s == "0.34");

    s = format("%0.0008f", 1e-08);
    assert(s == "0.00000001");

    s = format("%0.0008f", 1e-05);
    assert(s == "0.00001000");

    s = "helloworld";
    string r;
    r = format("%.2s", s[0 .. 5]);
    assert(r == "he");
    r = format("%.20s", s[0 .. 5]);
    assert(r == "hello");
    r = format("%8s", s[0 .. 5]);
    assert(r == "   hello");

    byte[] arrbyte = new byte[4];
    arrbyte[0] = 100;
    arrbyte[1] = -99;
    arrbyte[3] = 0;
    r = format("%s", arrbyte);
    assert(r == "[100, -99, 0, 0]");

    ubyte[] arrubyte = new ubyte[4];
    arrubyte[0] = 100;
    arrubyte[1] = 200;
    arrubyte[3] = 0;
    r = format("%s", arrubyte);
    assert(r == "[100, 200, 0, 0]");

    short[] arrshort = new short[4];
    arrshort[0] = 100;
    arrshort[1] = -999;
    arrshort[3] = 0;
    r = format("%s", arrshort);
    assert(r == "[100, -999, 0, 0]");

    ushort[] arrushort = new ushort[4];
    arrushort[0] = 100;
    arrushort[1] = 20_000;
    arrushort[3] = 0;
    r = format("%s", arrushort);
    assert(r == "[100, 20000, 0, 0]");

    int[] arrint = new int[4];
    arrint[0] = 100;
    arrint[1] = -999;
    arrint[3] = 0;
    r = format("%s", arrint);
    assert(r == "[100, -999, 0, 0]");

    long[] arrlong = new long[4];
    arrlong[0] = 100;
    arrlong[1] = -999;
    arrlong[3] = 0;
    r = format("%s", arrlong);
    assert(r == "[100, -999, 0, 0]");

    ulong[] arrulong = new ulong[4];
    arrulong[0] = 100;
    arrulong[1] = 999;
    arrulong[3] = 0;
    r = format("%s", arrulong);
    assert(r == "[100, 999, 0, 0]");

    string[] arr2 = new string[4];
    arr2[0] = "hello";
    arr2[1] = "world";
    arr2[3] = "foo";
    r = format("%s", arr2);
    assert(r == `["hello", "world", "", "foo"]`);

    r = format("%.8d", 7);
    assert(r == "00000007");
    r = format("%.8x", 10);
    assert(r == "0000000a");

    r = format("%-3d", 7);
    assert(r == "7  ");

    r = format("%-1*d", 4, 3);
    assert(r == "3   ");

    r = format("%*d", -3, 7);
    assert(r == "7  ");

    r = format("%.*d", -3, 7);
    assert(r == "7");

    r = format("%-1.*f", 2, 3.1415);
    assert(r == "3.14");

    r = format("abc"c);
    assert(r == "abc");

    //format() returns the same type as inputted.
    wstring wr;
    wr = format("def"w);
    assert(wr == "def"w);

    dstring dr;
    dr = format("ghi"d);
    assert(dr == "ghi"d);

    // Empty static character arrays work as well
    const char[0] cempty;
    assert(format("test%spath", cempty) == "testpath");
    const wchar[0] wempty;
    assert(format("test%spath", wempty) == "testpath");
    const dchar[0] dempty;
    assert(format("test%spath", dempty) == "testpath");

    void* p = () @trusted { return cast(void*) 0xDEADBEEF; } ();
    r = format("%s", p);
    assert(r == "DEADBEEF");

    r = format("%#x", 0xabcd);
    assert(r == "0xabcd");
    r = format("%#X", 0xABCD);
    assert(r == "0XABCD");

    r = format("%#o", octal!12345);
    assert(r == "012345");
    r = format("%o", 9);
    assert(r == "11");
    r = format("%#o", 0);   // https://issues.dlang.org/show_bug.cgi?id=15663
    assert(r == "0");

    r = format("%+d", 123);
    assert(r == "+123");
    r = format("%+d", -123);
    assert(r == "-123");
    r = format("% d", 123);
    assert(r == " 123");
    r = format("% d", -123);
    assert(r == "-123");

    r = format("%%");
    assert(r == "%");

    r = format("%d", true);
    assert(r == "1");
    r = format("%d", false);
    assert(r == "0");

    r = format("%d", 'a');
    assert(r == "97");
    wchar wc = 'a';
    r = format("%d", wc);
    assert(r == "97");
    dchar dc = 'a';
    r = format("%d", dc);
    assert(r == "97");

    byte b = byte.max;
    r = format("%x", b);
    assert(r == "7f");
    r = format("%x", ++b);
    assert(r == "80");
    r = format("%x", ++b);
    assert(r == "81");

    short sh = short.max;
    r = format("%x", sh);
    assert(r == "7fff");
    r = format("%x", ++sh);
    assert(r == "8000");
    r = format("%x", ++sh);
    assert(r == "8001");

    i = int.max;
    r = format("%x", i);
    assert(r == "7fffffff");
    r = format("%x", ++i);
    assert(r == "80000000");
    r = format("%x", ++i);
    assert(r == "80000001");

    r = format("%x", 10);
    assert(r == "a");
    r = format("%X", 10);
    assert(r == "A");
    r = format("%x", 15);
    assert(r == "f");
    r = format("%X", 15);
    assert(r == "F");

    Object c = null;
    r = () @trusted { return format("%s", c); } ();
    assert(r == "null");

    enum TestEnum
    {
        Value1, Value2
    }
    r = format("%s", TestEnum.Value2);
    assert(r == "Value2");

    immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);
    r = () @trusted { return format("%s", aa.values); } ();
    assert(r == `["hello", "betty"]` || r == `["betty", "hello"]`);
    r = format("%s", aa);
    assert(r == `[3:"hello", 4:"betty"]` || r == `[4:"betty", 3:"hello"]`);

    static const dchar[] ds = ['a','b'];
    for (int j = 0; j < ds.length; ++j)
    {
        r = format(" %d", ds[j]);
        if (j == 0)
            assert(r == " 97");
        else
            assert(r == " 98");
    }

    r = format(">%14d<, %s", 15, [1,2,3]);
    assert(r == ">            15<, [1, 2, 3]");

    assert(format("%8s", "bar") == "     bar");
    assert(format("%8s", "b\u00e9ll\u00f4") == "   b\u00e9ll\u00f4");
}

@safe unittest
{
    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
        auto tmp = format("%,d", 1000);
        assert(tmp == "1,000", "'" ~ tmp ~ "'");

        tmp = format("%,?d", 'z', 1234567);
        assert(tmp == "1z234z567", "'" ~ tmp ~ "'");

        tmp = format("%10,?d", 'z', 1234567);
        assert(tmp == " 1z234z567", "'" ~ tmp ~ "'");

        tmp = format("%11,2?d", 'z', 1234567);
        assert(tmp == " 1z23z45z67", "'" ~ tmp ~ "'");

        tmp = format("%11,*?d", 2, 'z', 1234567);
        assert(tmp == " 1z23z45z67", "'" ~ tmp ~ "'");

        tmp = format("%11,*d", 2, 1234567);
        assert(tmp == " 1,23,45,67", "'" ~ tmp ~ "'");

        tmp = format("%11,2d", 1234567);
        assert(tmp == " 1,23,45,67", "'" ~ tmp ~ "'");
    });
}

@safe unittest
{
    auto tmp = format("%,f", 1000.0);
    assert(tmp == "1,000.000000", "'" ~ tmp ~ "'");

    tmp = format("%,f", 1234567.891011);
    assert(tmp == "1,234,567.891011", "'" ~ tmp ~ "'");

    tmp = format("%,f", -1234567.891011);
    assert(tmp == "-1,234,567.891011", "'" ~ tmp ~ "'");

    tmp = format("%,2f", 1234567.891011);
    assert(tmp == "1,23,45,67.891011", "'" ~ tmp ~ "'");

    tmp = format("%18,f", 1234567.891011);
    assert(tmp == "  1,234,567.891011", "'" ~ tmp ~ "'");

    tmp = format("%18,?f", '.', 1234567.891011);
    assert(tmp == "  1.234.567.891011", "'" ~ tmp ~ "'");

    tmp = format("%,?.3f", 'ä', 1234567.891011);
    assert(tmp == "1ä234ä567.891", "'" ~ tmp ~ "'");

    tmp = format("%,*?.3f", 1, 'ä', 1234567.891011);
    assert(tmp == "1ä2ä3ä4ä5ä6ä7.891", "'" ~ tmp ~ "'");

    tmp = format("%,4?.3f", '_', 1234567.891011);
    assert(tmp == "123_4567.891", "'" ~ tmp ~ "'");

    tmp = format("%12,3.3f", 1234.5678);
    assert(tmp == "   1,234.568", "'" ~ tmp ~ "'");

    tmp = format("%,e", 3.141592653589793238462);
    assert(tmp == "3.141593e+00", "'" ~ tmp ~ "'");

    tmp = format("%15,e", 3.141592653589793238462);
    assert(tmp == "   3.141593e+00", "'" ~ tmp ~ "'");

    tmp = format("%15,e", -3.141592653589793238462);
    assert(tmp == "  -3.141593e+00", "'" ~ tmp ~ "'");

    tmp = format("%.4,*e", 2, 3.141592653589793238462);
    assert(tmp == "3.1416e+00", "'" ~ tmp ~ "'");

    tmp = format("%13.4,*e", 2, 3.141592653589793238462);
    assert(tmp == "   3.1416e+00", "'" ~ tmp ~ "'");

    tmp = format("%,.0f", 3.14);
    assert(tmp == "3", "'" ~ tmp ~ "'");

    tmp = format("%3,g", 1_000_000.123456);
    assert(tmp == "1e+06", "'" ~ tmp ~ "'");

    tmp = format("%19,?f", '.', -1234567.891011);
    assert(tmp == "  -1.234.567.891011", "'" ~ tmp ~ "'");
}

// Test for multiple indexes
@safe unittest
{
    auto tmp = format("%2:5$s", 1, 2, 3, 4, 5);
    assert(tmp == "2345", tmp);
}

// https://issues.dlang.org/show_bug.cgi?id=18047
@safe unittest
{
    auto cmp = "     123,456";
    assert(cmp.length == 12, format("%d", cmp.length));
    auto tmp = format("%12,d", 123456);
    assert(tmp.length == 12, format("%d", tmp.length));

    assert(tmp == cmp, "'" ~ tmp ~ "'");
}

// https://issues.dlang.org/show_bug.cgi?id=17459
@safe unittest
{
    auto cmp = "100";
    auto tmp  = format("%0d", 100);
    assert(tmp == cmp, tmp);

    cmp = "0100";
    tmp  = format("%04d", 100);
    assert(tmp == cmp, tmp);

    cmp = "0,000,000,100";
    tmp  = format("%012,3d", 100);
    assert(tmp == cmp, tmp);

    cmp = "0,000,001,000";
    tmp = format("%012,3d", 1_000);
    assert(tmp == cmp, tmp);

    cmp = "0,000,100,000";
    tmp = format("%012,3d", 100_000);
    assert(tmp == cmp, tmp);

    cmp = "0,001,000,000";
    tmp = format("%012,3d", 1_000_000);
    assert(tmp == cmp, tmp);

    cmp = "0,100,000,000";
    tmp = format("%012,3d", 100_000_000);
    assert(tmp == cmp, tmp);
}

// https://issues.dlang.org/show_bug.cgi?id=17459
@safe unittest
{
    auto cmp = "100,000";
    auto tmp  = format("%06,d", 100_000);
    assert(tmp == cmp, tmp);

    cmp = "100,000";
    tmp  = format("%07,d", 100_000);
    assert(tmp == cmp, tmp);

    cmp = "0,100,000";
    tmp  = format("%08,d", 100_000);
    assert(tmp == cmp, tmp);
}

// https://issues.dlang.org/show_bug.cgi?id=20288
@safe unittest
{
    string s = format("%,.2f", double.nan);
    assert(s == "nan", s);

    s = format("%,.2F", double.nan);
    assert(s == "NAN", s);

    s = format("%,.2f", -double.nan);
    assert(s == "-nan", s);

    s = format("%,.2F", -double.nan);
    assert(s == "-NAN", s);

    string g = format("^%13s$", "nan");
    string h = "^          nan$";
    assert(g == h, "\ngot:" ~ g ~ "\nexp:" ~ h);
    string a = format("^%13,3.2f$", double.nan);
    string b = format("^%13,3.2F$", double.nan);
    string c = format("^%13,3.2f$", -double.nan);
    string d = format("^%13,3.2F$", -double.nan);
    assert(a == "^          nan$", "\ngot:'"~ a ~ "'\nexp:'^          nan$'");
    assert(b == "^          NAN$", "\ngot:'"~ b ~ "'\nexp:'^          NAN$'");
    assert(c == "^         -nan$", "\ngot:'"~ c ~ "'\nexp:'^         -nan$'");
    assert(d == "^         -NAN$", "\ngot:'"~ d ~ "'\nexp:'^         -NAN$'");

    a = format("^%-13,3.2f$", double.nan);
    b = format("^%-13,3.2F$", double.nan);
    c = format("^%-13,3.2f$", -double.nan);
    d = format("^%-13,3.2F$", -double.nan);
    assert(a == "^nan          $", "\ngot:'"~ a ~ "'\nexp:'^nan          $'");
    assert(b == "^NAN          $", "\ngot:'"~ b ~ "'\nexp:'^NAN          $'");
    assert(c == "^-nan         $", "\ngot:'"~ c ~ "'\nexp:'^-nan         $'");
    assert(d == "^-NAN         $", "\ngot:'"~ d ~ "'\nexp:'^-NAN         $'");

    a = format("^%+13,3.2f$", double.nan);
    b = format("^%+13,3.2F$", double.nan);
    c = format("^%+13,3.2f$", -double.nan);
    d = format("^%+13,3.2F$", -double.nan);
    assert(a == "^         +nan$", "\ngot:'"~ a ~ "'\nexp:'^         +nan$'");
    assert(b == "^         +NAN$", "\ngot:'"~ b ~ "'\nexp:'^         +NAN$'");
    assert(c == "^         -nan$", "\ngot:'"~ c ~ "'\nexp:'^         -nan$'");
    assert(d == "^         -NAN$", "\ngot:'"~ d ~ "'\nexp:'^         -NAN$'");

    a = format("^%-+13,3.2f$", double.nan);
    b = format("^%-+13,3.2F$", double.nan);
    c = format("^%-+13,3.2f$", -double.nan);
    d = format("^%-+13,3.2F$", -double.nan);
    assert(a == "^+nan         $", "\ngot:'"~ a ~ "'\nexp:'^+nan         $'");
    assert(b == "^+NAN         $", "\ngot:'"~ b ~ "'\nexp:'^+NAN         $'");
    assert(c == "^-nan         $", "\ngot:'"~ c ~ "'\nexp:'^-nan         $'");
    assert(d == "^-NAN         $", "\ngot:'"~ d ~ "'\nexp:'^-NAN         $'");

    a = format("^%- 13,3.2f$", double.nan);
    b = format("^%- 13,3.2F$", double.nan);
    c = format("^%- 13,3.2f$", -double.nan);
    d = format("^%- 13,3.2F$", -double.nan);
    assert(a == "^ nan         $", "\ngot:'"~ a ~ "'\nexp:'^ nan         $'");
    assert(b == "^ NAN         $", "\ngot:'"~ b ~ "'\nexp:'^ NAN         $'");
    assert(c == "^-nan         $", "\ngot:'"~ c ~ "'\nexp:'^-nan         $'");
    assert(d == "^-NAN         $", "\ngot:'"~ d ~ "'\nexp:'^-NAN         $'");
}

@safe unittest
{
    struct S
    {
        int a;

        void toString(void delegate(const(char)[]) sink, string fmt)
        {
            auto spec = singleSpec(fmt);
            sink.formatValue(a, spec);
        }
    }

    S s = S(1);
    auto result = () @trusted { return format!"%5,3d"(s); } ();
    assert(result == "    1");
}

// https://issues.dlang.org/show_bug.cgi?id=23245
@safe unittest
{
    static struct S
    {
        string toString() { return "S"; }
    }

    S[1] s;
    assert(format("%s", s) == "[S]");
}

// https://issues.dlang.org/show_bug.cgi?id=23246
@safe unittest
{
    static struct S
    {
        string toString() { return "S"; }
    }

    S[int] s = [0 : S()];
    assert(format("%s", s) == "[0:S]");
}

/// ditto
typeof(fmt) format(alias fmt, Args...)(Args args)
if (isSomeString!(typeof(fmt)))
{
    import std.array : appender;
    import std.range.primitives : ElementEncodingType;
    import std.traits : Unqual;

    alias e = checkFormatException!(fmt, Args);
    alias Char = Unqual!(ElementEncodingType!(typeof(fmt)));

    static assert(!e, e);
    auto w = appender!(immutable(Char)[]);

    // no need to traverse the string twice during compile time
    if (!__ctfe)
    {
        enum len = guessLength!Char(fmt);
        w.reserve(len);
    }
    else
    {
        w.reserve(fmt.length);
    }

    formattedWrite(w, fmt, args);
    return w.data;
}

/// The format string can be checked at compile-time:
@safe pure unittest
{
    auto s = format!"%s is %s"("Pi", 3.14);
    assert(s == "Pi is 3.14");

    // This line doesn't compile, because 3.14 cannot be formatted with %d:
    // s = format!"%s is %d"("Pi", 3.14);
}

@safe pure unittest
{
    string s;
    static assert(!__traits(compiles, {s = format!"%l"();}));     // missing arg
    static assert(!__traits(compiles, {s = format!""(404);}));    // surplus arg
    static assert(!__traits(compiles, {s = format!"%d"(4.03);})); // incompatible arg
}

// https://issues.dlang.org/show_bug.cgi?id=17381
@safe pure unittest
{
    static assert(!__traits(compiles, format!"%s"(1.5, 2)));
    static assert(!__traits(compiles, format!"%f"(1.5, 2)));
    static assert(!__traits(compiles, format!"%s"(1.5L, 2)));
    static assert(!__traits(compiles, format!"%f"(1.5L, 2)));
}

// called during compilation to guess the length of the
// result of format
private size_t guessLength(Char, S)(S fmtString)
{
    import std.array : appender;

    size_t len;
    auto output = appender!(immutable(Char)[])();
    auto spec = FormatSpec!Char(fmtString);
    while (spec.writeUpToNextSpec(output))
    {
        // take a guess
        if (spec.width == 0 && (spec.precision == spec.UNSPECIFIED || spec.precision == spec.DYNAMIC))
        {
            switch (spec.spec)
            {
                case 'c':
                    ++len;
                    break;
                case 'd':
                case 'x':
                case 'X':
                    len += 3;
                    break;
                case 'b':
                    len += 8;
                    break;
                case 'f':
                case 'F':
                    len += 10;
                    break;
                case 's':
                case 'e':
                case 'E':
                case 'g':
                case 'G':
                    len += 12;
                    break;
                default: break;
            }

            continue;
        }

        if ((spec.spec == 'e' || spec.spec == 'E' || spec.spec == 'g' ||
             spec.spec == 'G' || spec.spec == 'f' || spec.spec == 'F') &&
            spec.precision != spec.UNSPECIFIED && spec.precision != spec.DYNAMIC &&
            spec.width == 0
        )
        {
            len += spec.precision + 5;
            continue;
        }

        if (spec.width == spec.precision)
            len += spec.width;
        else if (spec.width > 0 && spec.width != spec.DYNAMIC &&
                 (spec.precision == spec.UNSPECIFIED || spec.width > spec.precision))
        {
            len += spec.width;
        }
        else if (spec.precision != spec.UNSPECIFIED && spec.precision > spec.width)
            len += spec.precision;
    }
    len += output.data.length;
    return len;
}

@safe pure
unittest
{
    assert(guessLength!char("%c") == 1);
    assert(guessLength!char("%d") == 3);
    assert(guessLength!char("%x") == 3);
    assert(guessLength!char("%b") == 8);
    assert(guessLength!char("%f") == 10);
    assert(guessLength!char("%s") == 12);
    assert(guessLength!char("%02d") == 2);
    assert(guessLength!char("%02d") == 2);
    assert(guessLength!char("%4.4d") == 4);
    assert(guessLength!char("%2.4f") == 4);
    assert(guessLength!char("%02d:%02d:%02d") == 8);
    assert(guessLength!char("%0.2f") == 7);
    assert(guessLength!char("%0*d") == 0);
}

/**
Converts its arguments according to a format string into a buffer.
The buffer has to be large enough to hold the formatted string.

The second version of `sformat` takes the format string as a template
argument. In this case, it is checked for consistency at
compile-time.

Params:
    buf = the buffer where the formatted string should go
    fmt = a $(MREF_ALTTEXT format string, std,format)
    args = a variadic list of arguments to be formatted
    Char = character type of `fmt`
    Args = a variadic list of types of the arguments

Returns:
    A slice of `buf` containing the formatted string.

Throws:
    A $(REF_ALTTEXT RangeError, RangeError, core, exception) if `buf`
    isn't large enough to hold the formatted string
    and a $(LREF FormatException) if formatting did not succeed.

Note:
    In theory this function should be `@nogc`. But with the current
    implementation there are some cases where allocations occur:

    $(UL
    $(LI An exception is thrown.)
    $(LI A custom `toString` function of a compound type allocates.))
 */
char[] sformat(Char, Args...)(return scope char[] buf, scope const(Char)[] fmt, Args args)
{
    import core.exception : RangeError;
    import std.range.primitives;
    import std.utf : encode;

    static struct Sink
    {
        char[] buf;
        size_t i;
        void put(char c)
        {
            if (buf.length <= i)
                throw new RangeError(__FILE__, __LINE__);

            buf[i] = c;
            i += 1;
        }
        void put(dchar c)
        {
            char[4] enc;
            auto n = encode(enc, c);

            if (buf.length < i + n)
                throw new RangeError(__FILE__, __LINE__);

            buf[i .. i + n] = enc[0 .. n];
            i += n;
        }
        void put(scope const(char)[] s)
        {
            if (buf.length < i + s.length)
                throw new RangeError(__FILE__, __LINE__);

            buf[i .. i + s.length] = s[];
            i += s.length;
        }
        void put(scope const(wchar)[] s)
        {
            for (; !s.empty; s.popFront())
                put(s.front);
        }
        void put(scope const(dchar)[] s)
        {
            for (; !s.empty; s.popFront())
                put(s.front);
        }
    }
    auto sink = Sink(buf);
    auto n = formattedWrite(sink, fmt, args);
    version (all)
    {
        // In the future, this check will be removed to increase consistency
        // with formattedWrite
        import std.conv : text;
        enforceFmt(
            n == args.length,
            text("Orphan format arguments: args[", n, " .. ", args.length, "]")
        );
    }
    return buf[0 .. sink.i];
}

/// ditto
char[] sformat(alias fmt, Args...)(char[] buf, Args args)
if (isSomeString!(typeof(fmt)))
{
    alias e = checkFormatException!(fmt, Args);
    static assert(!e, e);
    return .sformat(buf, fmt, args);
}

///
@safe pure unittest
{
    char[20] buf;
    assert(sformat(buf[], "Here are %d %s.", 3, "apples") == "Here are 3 apples.");

    assert(buf[].sformat("Increase: %7.2f %%", 17.4285) == "Increase:   17.43 %");
}

/// The format string can be checked at compile-time:
@safe pure unittest
{
    char[20] buf;

    assert(sformat!"Here are %d %s."(buf[], 3, "apples") == "Here are 3 apples.");

    // This line doesn't compile, because 3.14 cannot be formatted with %d:
    // writeln(sformat!"Here are %d %s."(buf[], 3.14, "apples"));
}

// checking, what is implicitly and explicitly stated in the public unittest
@safe unittest
{
    import std.exception : assertThrown;

    char[20] buf;
    assertThrown!FormatException(sformat(buf[], "Here are %d %s.", 3.14, "apples"));
    assert(!__traits(compiles, sformat!"Here are %d %s."(buf[], 3.14, "apples")));
}

@safe unittest
{
    import core.exception : RangeError;
    import std.exception : assertCTFEable, assertThrown;

    assertCTFEable!(
    {
        char[10] buf;

        assert(sformat(buf[], "foo") == "foo");
        assert(sformat(buf[], "foo%%") == "foo%");
        assert(sformat(buf[], "foo%s", 'C') == "fooC");
        assert(sformat(buf[], "%s foo", "bar") == "bar foo");
        () @trusted {
            assertThrown!RangeError(sformat(buf[], "%s foo %s", "bar", "abc"));
        } ();
        assert(sformat(buf[], "foo %d", -123) == "foo -123");
        assert(sformat(buf[], "foo %d", 123) == "foo 123");

        assertThrown!FormatException(sformat(buf[], "foo %s"));
        assertThrown!FormatException(sformat(buf[], "foo %s", 123, 456));

        assert(sformat(buf[], "%s %s %s", "c"c, "w"w, "d"d) == "c w d");
    });
}

@safe unittest // ensure that sformat avoids the GC
{
    import core.memory : GC;

    const a = ["foo", "bar"];
    const u = () @trusted { return GC.stats().usedSize; } ();
    char[20] buf;
    sformat(buf, "%d", 123);
    sformat(buf, "%s", a);
    sformat(buf, "%s", 'c');
    const v = () @trusted { return GC.stats().usedSize; } ();
    assert(u == v);
}

@safe unittest // https://issues.dlang.org/show_bug.cgi?id=23488
{
    static struct R
    {
        string s = "Ü";
        bool empty() { return s.length == 0; }
        char front() { return s[0]; }
        void popFront() { s = s[1 .. $]; }
    }
    char[2] buf;
    assert(sformat(buf, "%s", R()) == "Ü");
}

version (StdUnittest)
private void formatReflectTest(T)(ref T val, string fmt, string formatted, string fn = __FILE__, size_t ln = __LINE__)
{
    formatReflectTest(val, fmt, [formatted], fn, ln);
}

version (StdUnittest)
private void formatReflectTest(T)(ref T val, string fmt, string[] formatted, string fn = __FILE__, size_t ln = __LINE__)
{
    import core.exception : AssertError;
    import std.algorithm.searching : canFind;
    import std.array : appender;
    import std.math.operations : isClose;
    import std.traits : FloatingPointTypeOf;

    auto w = appender!string();
    formattedWrite(w, fmt, val);

    auto input = w.data;
    enforce!AssertError(formatted.canFind(input), input, fn, ln);

    T val2;
    formattedRead(input, fmt, val2);

    static if (is(FloatingPointTypeOf!T))
        enforce!AssertError(isClose(val, val2), input, fn, ln);
    else
        enforce!AssertError(val == val2, input, fn, ln);
}

@safe unittest
{
    void booleanTest()
    {
        auto b = true;
        formatReflectTest(b, "%s", `true`);
        formatReflectTest(b, "%b", `1`);
        formatReflectTest(b, "%o", `1`);
        formatReflectTest(b, "%d", `1`);
        formatReflectTest(b, "%u", `1`);
        formatReflectTest(b, "%x", `1`);
    }

    void integerTest()
    {
        auto n = 127;
        formatReflectTest(n, "%s", `127`);
        formatReflectTest(n, "%b", `1111111`);
        formatReflectTest(n, "%o", `177`);
        formatReflectTest(n, "%d", `127`);
        formatReflectTest(n, "%u", `127`);
        formatReflectTest(n, "%x", `7f`);
    }

    void floatingTest()
    {
        auto f = 3.14;
        formatReflectTest(f, "%s", `3.14`);
        formatReflectTest(f, "%e", `3.140000e+00`);
        formatReflectTest(f, "%f", `3.140000`);
        formatReflectTest(f, "%g", `3.14`);
    }

    void charTest()
    {
        auto c = 'a';
        formatReflectTest(c, "%s", `a`);
        formatReflectTest(c, "%c", `a`);
        formatReflectTest(c, "%b", `1100001`);
        formatReflectTest(c, "%o", `141`);
        formatReflectTest(c, "%d", `97`);
        formatReflectTest(c, "%u", `97`);
        formatReflectTest(c, "%x", `61`);
    }

    void strTest()
    {
        auto s = "hello";
        formatReflectTest(s, "%s",              `hello`);
        formatReflectTest(s, "%(%c,%)",         `h,e,l,l,o`);
        formatReflectTest(s, "%(%s,%)",         `'h','e','l','l','o'`);
        formatReflectTest(s, "[%(<%c>%| $ %)]", `[<h> $ <e> $ <l> $ <l> $ <o>]`);
    }

    void daTest()
    {
        auto a = [1,2,3,4];
        formatReflectTest(a, "%s",              `[1, 2, 3, 4]`);
        formatReflectTest(a, "[%(%s; %)]",      `[1; 2; 3; 4]`);
        formatReflectTest(a, "[%(<%s>%| $ %)]", `[<1> $ <2> $ <3> $ <4>]`);
    }

    void saTest()
    {
        int[4] sa = [1,2,3,4];
        formatReflectTest(sa, "%s",              `[1, 2, 3, 4]`);
        formatReflectTest(sa, "[%(%s; %)]",      `[1; 2; 3; 4]`);
        formatReflectTest(sa, "[%(<%s>%| $ %)]", `[<1> $ <2> $ <3> $ <4>]`);
    }

    void aaTest()
    {
        auto aa = [1:"hello", 2:"world"];
        formatReflectTest(aa, "%s",                    [`[1:"hello", 2:"world"]`, `[2:"world", 1:"hello"]`]);
        formatReflectTest(aa, "[%(%s->%s, %)]",        [`[1->"hello", 2->"world"]`, `[2->"world", 1->"hello"]`]);
        formatReflectTest(aa, "{%([%s=%(%c%)]%|; %)}", [`{[1=hello]; [2=world]}`, `{[2=world]; [1=hello]}`]);
    }

    import std.exception : assertCTFEable;

    assertCTFEable!(
    {
        booleanTest();
        integerTest();
        floatingTest();
        charTest();
        strTest();
        daTest();
        saTest();
        aaTest();
    });
}
