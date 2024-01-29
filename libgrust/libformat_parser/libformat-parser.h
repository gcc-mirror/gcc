#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

/// Enum of alignments which are supported.
enum class Alignment
{
  /// The value will be aligned to the left.
  AlignLeft,
  /// The value will be aligned to the right.
  AlignRight,
  /// The value will be aligned in the center.
  AlignCenter,
  /// The value will take on a default alignment.
  AlignUnknown,
};

/// Enum for the debug hex flags.
enum class DebugHex
{
  /// The `x` flag in `{:x?}`.
  Lower,
  /// The `X` flag in `{:X?}`.
  Upper,
};

/// Enum for the sign flags.
enum class Sign
{
  /// The `+` flag.
  Plus,
  /// The `-` flag.
  Minus,
};

template <typename T = void> struct Box;

template <typename T = void> struct Option;

/// Enum describing where an argument for a format can be located.
struct Position
{
  enum class Tag
  {
    /// The argument is implied to be located at an index
    ArgumentImplicitlyIs,
    /// The argument is located at a specific index given in the format,
    ArgumentIs,
    /// The argument has a name.
    ArgumentNamed,
  };

  struct ArgumentImplicitlyIs_Body
  {
    uintptr_t _0;
  };

  struct ArgumentIs_Body
  {
    uintptr_t _0;
  };

  struct ArgumentNamed_Body
  {
    const str *_0;
  };

  Tag tag;
  union
  {
    ArgumentImplicitlyIs_Body argument_implicitly_is;
    ArgumentIs_Body argument_is;
    ArgumentNamed_Body argument_named;
  };
};

/// Range inside of a `Span` used for diagnostics when we only have access to
/// relative positions.
struct InnerSpan
{
  uintptr_t start;
  uintptr_t end;
};

/// A count is used for the precision and width parameters of an integer, and
/// can reference either an argument or a literal integer.
struct Count
{
  enum class Tag
  {
    /// The count is specified explicitly.
    CountIs,
    /// The count is specified by the argument with the given name.
    CountIsName,
    /// The count is specified by the argument at the given index.
    CountIsParam,
    /// The count is specified by a star (like in `{:.*}`) that refers to the
    /// argument at the given index.
    CountIsStar,
    /// The count is implied and cannot be explicitly specified.
    CountImplied,
  };

  struct CountIs_Body
  {
    uintptr_t _0;
  };

  struct CountIsName_Body
  {
    const str *_0;
    InnerSpan _1;
  };

  struct CountIsParam_Body
  {
    uintptr_t _0;
  };

  struct CountIsStar_Body
  {
    uintptr_t _0;
  };

  Tag tag;
  union
  {
    CountIs_Body count_is;
    CountIsName_Body count_is_name;
    CountIsParam_Body count_is_param;
    CountIsStar_Body count_is_star;
  };
};

/// Specification for the formatting of an argument in the format string.
struct FormatSpec
{
  /// Optionally specified character to fill alignment with.
  Option<uint32_t> fill;
  /// Span of the optionally specified fill character.
  Option<InnerSpan> fill_span;
  /// Optionally specified alignment.
  Alignment align;
  /// The `+` or `-` flag.
  Option<Sign> sign;
  /// The `#` flag.
  bool alternate;
  /// The `0` flag.
  bool zero_pad;
  /// The `x` or `X` flag. (Only for `Debug`.)
  Option<DebugHex> debug_hex;
  /// The integer precision to use.
  Count precision;
  /// The span of the precision formatting flag (for diagnostics).
  Option<InnerSpan> precision_span;
  /// The string width requested for the resulting format.
  Count width;
  /// The span of the width formatting flag (for diagnostics).
  Option<InnerSpan> width_span;
  /// The descriptor string representing the name of the format desired for
  /// this argument, this can be empty or any number of characters, although
  /// it is required to be one word.
  const str *ty;
  /// The span of the descriptor string (for diagnostics).
  Option<InnerSpan> ty_span;
};

/// Representation of an argument specification.
struct Argument
{
  /// Where to find this argument
  Position position;
  /// The span of the position indicator. Includes any whitespace in implicit
  /// positions (`{  }`).
  InnerSpan position_span;
  /// How to format the argument
  FormatSpec format;
};

/// A piece is a portion of the format string which represents the next part
/// to emit. These are emitted as a stream by the `Parser` class.
struct Piece
{
  enum class Tag
  {
    /// A literal string which should directly be emitted
    String,
    /// This describes that formatting should process the next argument (as
    /// specified inside) for emission.
    NextArgument,
  };

  struct String_Body
  {
    const str *_0;
  };

  struct NextArgument_Body
  {
    Box<Argument> _0;
  };

  Tag tag;
  union
  {
    String_Body string;
    NextArgument_Body next_argument;
  };
};

struct PieceSlice
{
  const Piece *base_ptr;
  uintptr_t len;
};

extern "C" {

PieceSlice
collect_pieces (const char *input);

} // extern "C"
