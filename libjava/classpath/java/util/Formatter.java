/* Formatter.java -- printf-style formatting
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package java.util;

import gnu.java.lang.CPStringBuilder;

import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.Flushable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.text.DateFormatSymbols;
import java.text.DecimalFormatSymbols;

import gnu.classpath.SystemProperties;

/**
 * <p>
 * A Java formatter for <code>printf</code>-style format strings,
 * as seen in the C programming language.   This differs from the
 * C interpretation of such strings by performing much stricter
 * checking of format specifications and their corresponding
 * arguments.  While unknown conversions will be ignored in C,
 * and invalid conversions will only produce compiler warnings,
 * the Java version utilises a full range of run-time exceptions to
 * handle these cases.  The Java version is also more customisable
 * by virtue of the provision of the {@link Formattable} interface,
 * which allows an arbitrary class to be formatted by the formatter.
 * </p>
 * <p>
 * The formatter is accessible by more convienient static methods.
 * For example, streams now have appropriate format methods
 * (the equivalent of <code>fprintf</code>) as do <code>String</code>
 * objects (the equivalent of <code>sprintf</code>).
 * </p>
 * <p>
 * <strong>Note</strong>: the formatter is not thread-safe.  For
 * multi-threaded access, external synchronization should be provided.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public final class Formatter
  implements Closeable, Flushable
{

  /**
   * The output of the formatter.
   */
  private Appendable out;

  /**
   * The locale used by the formatter.
   */
  private Locale locale;

  /**
   * Whether or not the formatter is closed.
   */
  private boolean closed;

  /**
   * The last I/O exception thrown by the output stream.
   */
  private IOException ioException;

  // Some state used when actually formatting.
  /**
   * The format string.
   */
  private String format;

  /**
   * The current index into the string.
   */
  private int index;

  /**
   * The length of the format string.
   */
  private int length;

  /**
   * The formatting locale.
   */
  private Locale fmtLocale;

  // Note that we include '-' twice.  The flags are ordered to
  // correspond to the values in FormattableFlags, and there is no
  // flag (in the sense of this field used when parsing) for
  // UPPERCASE; the second '-' serves as a placeholder.
  /**
   * A string used to index into the formattable flags.
   */
  private static final String FLAGS = "--#+ 0,(";

  /**
   * The system line separator.
   */
  private static final String lineSeparator
    = SystemProperties.getProperty("line.separator");

  /**
   * The type of numeric output format for a {@link BigDecimal}.
   */
  public enum BigDecimalLayoutForm
  {
    DECIMAL_FLOAT,
    SCIENTIFIC
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale and a {@link StringBuilder} as the output stream.
   */
  public Formatter()
  {
    this(null, Locale.getDefault());
  }

  /**
   * Constructs a new <code>Formatter</code> using the specified
   * locale and a {@link StringBuilder} as the output stream.
   * If the locale is <code>null</code>, then no localization
   * is applied.
   *
   * @param loc the locale to use.
   */
  public Formatter(Locale loc)
  {
    this(null, loc);
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale and the specified output stream.
   *
   * @param app the output stream to use.
   */
  public Formatter(Appendable app)
  {
    this(app, Locale.getDefault());
  }

  /**
   * Constructs a new <code>Formatter</code> using the specified
   * locale and the specified output stream.  If the locale is
   * <code>null</code>, then no localization is applied.
   *
   * @param app the output stream to use.
   * @param loc the locale to use.
   */
  public Formatter(Appendable app, Locale loc)
  {
    this.out = app == null ? new StringBuilder() : app;
    this.locale = loc;
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale and character set, with the specified file as the
   * output stream.
   *
   * @param file the file to use for output.
   * @throws FileNotFoundException if the file does not exist
   *                               and can not be created.
   * @throws SecurityException if a security manager is present
   *                           and doesn't allow writing to the file.
   */
  public Formatter(File file)
    throws FileNotFoundException
  {
    this(new OutputStreamWriter(new FileOutputStream(file)));
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale, with the specified file as the output stream
   * and the supplied character set.
   *
   * @param file the file to use for output.
   * @param charset the character set to use for output.
   * @throws FileNotFoundException if the file does not exist
   *                               and can not be created.
   * @throws SecurityException if a security manager is present
   *                           and doesn't allow writing to the file.
   * @throws UnsupportedEncodingException if the supplied character
   *                                      set is not supported.
   */
  public Formatter(File file, String charset)
    throws FileNotFoundException, UnsupportedEncodingException
  {
    this(file, charset, Locale.getDefault());
  }

  /**
   * Constructs a new <code>Formatter</code> using the specified
   * file as the output stream with the supplied character set
   * and locale.  If the locale is <code>null</code>, then no
   * localization is applied.
   *
   * @param file the file to use for output.
   * @param charset the character set to use for output.
   * @param loc the locale to use.
   * @throws FileNotFoundException if the file does not exist
   *                               and can not be created.
   * @throws SecurityException if a security manager is present
   *                           and doesn't allow writing to the file.
   * @throws UnsupportedEncodingException if the supplied character
   *                                      set is not supported.
   */
  public Formatter(File file, String charset, Locale loc)
    throws FileNotFoundException, UnsupportedEncodingException
  {
    this(new OutputStreamWriter(new FileOutputStream(file), charset),
         loc);
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale and character set, with the specified output stream.
   *
   * @param out the output stream to use.
   */
  public Formatter(OutputStream out)
  {
    this(new OutputStreamWriter(out));
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale, with the specified file output stream and the
   * supplied character set.
   *
   * @param out the output stream.
   * @param charset the character set to use for output.
   * @throws UnsupportedEncodingException if the supplied character
   *                                      set is not supported.
   */
  public Formatter(OutputStream out, String charset)
    throws UnsupportedEncodingException
  {
    this(out, charset, Locale.getDefault());
  }

  /**
   * Constructs a new <code>Formatter</code> using the specified
   * output stream with the supplied character set and locale.
   * If the locale is <code>null</code>, then no localization is
   * applied.
   *
   * @param out the output stream.
   * @param charset the character set to use for output.
   * @param loc the locale to use.
   * @throws UnsupportedEncodingException if the supplied character
   *                                      set is not supported.
   */
  public Formatter(OutputStream out, String charset, Locale loc)
    throws UnsupportedEncodingException
  {
    this(new OutputStreamWriter(out, charset), loc);
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale with the specified output stream.  The character
   * set used is that of the output stream.
   *
   * @param out the output stream to use.
   */
  public Formatter(PrintStream out)
  {
    this((Appendable) out);
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale and character set, with the specified file as the
   * output stream.
   *
   * @param file the file to use for output.
   * @throws FileNotFoundException if the file does not exist
   *                               and can not be created.
   * @throws SecurityException if a security manager is present
   *                           and doesn't allow writing to the file.
   */
  public Formatter(String file) throws FileNotFoundException
  {
    this(new OutputStreamWriter(new FileOutputStream(file)));
  }

  /**
   * Constructs a new <code>Formatter</code> using the default
   * locale, with the specified file as the output stream
   * and the supplied character set.
   *
   * @param file the file to use for output.
   * @param charset the character set to use for output.
   * @throws FileNotFoundException if the file does not exist
   *                               and can not be created.
   * @throws SecurityException if a security manager is present
   *                           and doesn't allow writing to the file.
   * @throws UnsupportedEncodingException if the supplied character
   *                                      set is not supported.
   */
  public Formatter(String file, String charset)
    throws FileNotFoundException, UnsupportedEncodingException
  {
    this(file, charset, Locale.getDefault());
  }

  /**
   * Constructs a new <code>Formatter</code> using the specified
   * file as the output stream with the supplied character set
   * and locale.  If the locale is <code>null</code>, then no
   * localization is applied.
   *
   * @param file the file to use for output.
   * @param charset the character set to use for output.
   * @param loc the locale to use.
   * @throws FileNotFoundException if the file does not exist
   *                               and can not be created.
   * @throws SecurityException if a security manager is present
   *                           and doesn't allow writing to the file.
   * @throws UnsupportedEncodingException if the supplied character
   *                                      set is not supported.
   */
  public Formatter(String file, String charset, Locale loc)
    throws FileNotFoundException, UnsupportedEncodingException
  {
    this(new OutputStreamWriter(new FileOutputStream(file), charset),
         loc);
  }

  /**
   * Closes the formatter, so as to release used resources.
   * If the underlying output stream supports the {@link Closeable}
   * interface, then this is also closed.  Attempts to use
   * a formatter instance, via any method other than
   * {@link #ioException()}, after closure results in a
   * {@link FormatterClosedException}.
   */
  public void close()
  {
    if (closed)
      return;
    try
      {
        if (out instanceof Closeable)
          ((Closeable) out).close();
      }
    catch (IOException _)
      {
        // FIXME: do we ignore these or do we set ioException?
        // The docs seem to indicate that we should ignore.
      }
    closed = true;
  }

  /**
   * Flushes the formatter, writing any cached data to the output
   * stream.  If the underlying output stream supports the
   * {@link Flushable} interface, it is also flushed.
   *
   * @throws FormatterClosedException if the formatter is closed.
   */
  public void flush()
  {
    if (closed)
      throw new FormatterClosedException();
    try
      {
        if (out instanceof Flushable)
          ((Flushable) out).flush();
      }
    catch (IOException _)
      {
        // FIXME: do we ignore these or do we set ioException?
        // The docs seem to indicate that we should ignore.
      }
  }

  /**
   * Return the name corresponding to a flag.
   *
   * @param flags the flag to return the name of.
   * @return the name of the flag.
   */
  private String getName(int flags)
  {
    // FIXME: do we want all the flags in here?
    // Or should we redo how this is reported?
    int bit = Integer.numberOfTrailingZeros(flags);
    return FLAGS.substring(bit, bit + 1);
  }

  /**
   * Verify the flags passed to a conversion.
   *
   * @param flags the flags to verify.
   * @param allowed the allowed flags mask.
   * @param conversion the conversion character.
   */
  private void checkFlags(int flags, int allowed, char conversion)
  {
    flags &= ~allowed;
    if (flags != 0)
      throw new FormatFlagsConversionMismatchException(getName(flags),
                                                       conversion);
  }

  /**
   * Throw an exception if a precision was specified.
   *
   * @param precision the precision value (-1 indicates not specified).
   */
  private void noPrecision(int precision)
  {
    if (precision != -1)
      throw new IllegalFormatPrecisionException(precision);
  }

  /**
   * Apply the numeric localization algorithm to a StringBuilder.
   *
   * @param builder the builder to apply to.
   * @param flags the formatting flags to use.
   * @param width the width of the numeric value.
   * @param isNegative true if the value is negative.
   */
  private void applyLocalization(CPStringBuilder builder, int flags, int width,
                                 boolean isNegative)
  {
    DecimalFormatSymbols dfsyms;
    if (fmtLocale == null)
      dfsyms = new DecimalFormatSymbols();
    else
      dfsyms = new DecimalFormatSymbols(fmtLocale);

    // First replace each digit.
    char zeroDigit = dfsyms.getZeroDigit();
    int decimalOffset = -1;
    for (int i = builder.length() - 1; i >= 0; --i)
      {
        char c = builder.charAt(i);
        if (c >= '0' && c <= '9')
          builder.setCharAt(i, (char) (c - '0' + zeroDigit));
        else if (c == '.')
          {
            assert decimalOffset == -1;
            decimalOffset = i;
          }
      }

    // Localize the decimal separator.
    if (decimalOffset != -1)
      {
        builder.deleteCharAt(decimalOffset);
        builder.insert(decimalOffset, dfsyms.getDecimalSeparator());
      }

    // Insert the grouping separators.
    if ((flags & FormattableFlags.COMMA) != 0)
      {
        char groupSeparator = dfsyms.getGroupingSeparator();
        int groupSize = 3;      // FIXME
        int offset = (decimalOffset == -1) ? builder.length() : decimalOffset;
        // We use '>' because we don't want to insert a separator
        // before the first digit.
        for (int i = offset - groupSize; i > 0; i -= groupSize)
          builder.insert(i, groupSeparator);
      }

    if ((flags & FormattableFlags.ZERO) != 0)
      {
        // Zero fill.  Note that according to the algorithm we do not
        // insert grouping separators here.
        for (int i = width - builder.length(); i > 0; --i)
          builder.insert(0, zeroDigit);
      }

    if (isNegative)
      {
        if ((flags & FormattableFlags.PAREN) != 0)
          {
            builder.insert(0, '(');
            builder.append(')');
          }
        else
          builder.insert(0, '-');
      }
    else if ((flags & FormattableFlags.PLUS) != 0)
      builder.insert(0, '+');
    else if ((flags & FormattableFlags.SPACE) != 0)
      builder.insert(0, ' ');
  }

  /**
   * A helper method that handles emitting a String after applying
   * precision, width, justification, and upper case flags.
   *
   * @param arg the string to emit.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void genericFormat(String arg, int flags, int width, int precision)
    throws IOException
  {
    if ((flags & FormattableFlags.UPPERCASE) != 0)
      {
        if (fmtLocale == null)
          arg = arg.toUpperCase();
        else
          arg = arg.toUpperCase(fmtLocale);
      }

    if (precision >= 0 && arg.length() > precision)
      arg = arg.substring(0, precision);

    boolean leftJustify = (flags & FormattableFlags.LEFT_JUSTIFY) != 0;
    if (leftJustify && width == -1)
      throw new MissingFormatWidthException("fixme");
    if (! leftJustify && arg.length() < width)
      {
        for (int i = width - arg.length(); i > 0; --i)
          out.append(' ');
      }
    out.append(arg);
    if (leftJustify && arg.length() < width)
      {
        for (int i = width - arg.length(); i > 0; --i)
          out.append(' ');
      }
  }

  /**
   * Emit a boolean.
   *
   * @param arg the boolean to emit.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param conversion the conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void booleanFormat(Object arg, int flags, int width, int precision,
                             char conversion)
    throws IOException
  {
    checkFlags(flags,
               FormattableFlags.LEFT_JUSTIFY | FormattableFlags.UPPERCASE,
               conversion);
    String result;
    if (arg instanceof Boolean)
      result = String.valueOf((Boolean) arg);
    else
      result = arg == null ? "false" : "true";
    genericFormat(result, flags, width, precision);
  }

  /**
   * Emit a hash code.
   *
   * @param arg the hash code to emit.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param conversion the conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void hashCodeFormat(Object arg, int flags, int width, int precision,
                              char conversion)
    throws IOException
  {
    checkFlags(flags,
               FormattableFlags.LEFT_JUSTIFY | FormattableFlags.UPPERCASE,
               conversion);
    genericFormat(arg == null ? "null" : Integer.toHexString(arg.hashCode()),
                  flags, width, precision);
  }

  /**
   * Emit a String or Formattable conversion.
   *
   * @param arg the String or Formattable to emit.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param conversion the conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void stringFormat(Object arg, int flags, int width, int precision,
                            char conversion)
    throws IOException
  {
    if (arg instanceof Formattable)
      {
        checkFlags(flags,
                   (FormattableFlags.LEFT_JUSTIFY
                    | FormattableFlags.UPPERCASE
                    | FormattableFlags.ALTERNATE),
                   conversion);
        Formattable fmt = (Formattable) arg;
        fmt.formatTo(this, flags, width, precision);
      }
    else
      {
        checkFlags(flags,
                   FormattableFlags.LEFT_JUSTIFY | FormattableFlags.UPPERCASE,
                   conversion);
        genericFormat(arg == null ? "null" : arg.toString(), flags, width,
                      precision);
      }
  }

  /**
   * Emit a character.
   *
   * @param arg the character to emit.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param conversion the conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void characterFormat(Object arg, int flags, int width, int precision,
                               char conversion)
    throws IOException
  {
    checkFlags(flags,
               FormattableFlags.LEFT_JUSTIFY | FormattableFlags.UPPERCASE,
               conversion);
    noPrecision(precision);

    int theChar;
    if (arg instanceof Character)
      theChar = ((Character) arg).charValue();
    else if (arg instanceof Byte)
      theChar = (char) (((Byte) arg).byteValue ());
    else if (arg instanceof Short)
      theChar = (char) (((Short) arg).shortValue ());
    else if (arg instanceof Integer)
      {
        theChar = ((Integer) arg).intValue();
        if (! Character.isValidCodePoint(theChar))
          throw new IllegalFormatCodePointException(theChar);
      }
    else
      throw new IllegalFormatConversionException(conversion, arg.getClass());
    String result = new String(Character.toChars(theChar));
    genericFormat(result, flags, width, precision);
  }

  /**
   * Emit a '%'.
   *
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void percentFormat(int flags, int width, int precision)
    throws IOException
  {
    checkFlags(flags, FormattableFlags.LEFT_JUSTIFY, '%');
    noPrecision(precision);
    genericFormat("%", flags, width, precision);
  }

  /**
   * Emit a newline.
   *
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void newLineFormat(int flags, int width, int precision)
    throws IOException
  {
    checkFlags(flags, 0, 'n');
    noPrecision(precision);
    if (width != -1)
      throw new IllegalFormatWidthException(width);
    genericFormat(lineSeparator, flags, width, precision);
  }

  /**
   * Helper method to do initial formatting and checking for integral
   * conversions.
   *
   * @param arg the formatted argument.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param radix the radix of the number.
   * @param conversion the conversion character.
   * @return the result.
   */
  private CPStringBuilder basicIntegralConversion(Object arg, int flags,
                                                  int width, int precision,
                                                  int radix, char conversion)
  {
    assert radix == 8 || radix == 10 || radix == 16;
    noPrecision(precision);

    // Some error checking.
    if ((flags & FormattableFlags.PLUS) != 0
        && (flags & FormattableFlags.SPACE) != 0)
      throw new IllegalFormatFlagsException(getName(flags));

    if ((flags & FormattableFlags.LEFT_JUSTIFY) != 0 && width == -1)
      throw new MissingFormatWidthException("fixme");

    // Do the base translation of the value to a string.
    String result;
    int basicFlags = (FormattableFlags.LEFT_JUSTIFY
                      // We already handled any possible error when
                      // parsing.
                      | FormattableFlags.UPPERCASE
                      | FormattableFlags.ZERO);
    if (radix == 10)
      basicFlags |= (FormattableFlags.PLUS
                     | FormattableFlags.SPACE
                     | FormattableFlags.COMMA
                     | FormattableFlags.PAREN);
    else
      basicFlags |= FormattableFlags.ALTERNATE;

    if (arg instanceof BigInteger)
      {
        checkFlags(flags,
                   (basicFlags
                    | FormattableFlags.PLUS
                    | FormattableFlags.SPACE
                    | FormattableFlags.PAREN),
                   conversion);
        BigInteger bi = (BigInteger) arg;
        result = bi.toString(radix);
      }
    else if (arg instanceof Number
             && ! (arg instanceof Float)
             && ! (arg instanceof Double))
      {
        checkFlags(flags, basicFlags, conversion);
        long value = ((Number) arg).longValue ();
        if (radix == 8)
          result = Long.toOctalString(value);
        else if (radix == 16)
          result = Long.toHexString(value);
        else
          result = Long.toString(value);
      }
    else
      throw new IllegalFormatConversionException(conversion, arg.getClass());

    return new CPStringBuilder(result);
  }

  /**
   * Emit a hex or octal value.
   *
   * @param arg the hexadecimal or octal value.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param radix the radix of the number.
   * @param conversion the conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void hexOrOctalConversion(Object arg, int flags, int width,
                                    int precision, int radix,
                                    char conversion)
    throws IOException
  {
    assert radix == 8 || radix == 16;

    CPStringBuilder builder = basicIntegralConversion(arg, flags, width,
                                                      precision, radix,
                                                      conversion);
    int insertPoint = 0;

    // Insert the sign.
    if (builder.charAt(0) == '-')
      {
        // Already inserted.  Note that we don't insert a sign, since
        // the only case where it is needed it BigInteger, and it has
        // already been inserted by toString.
        ++insertPoint;
      }
    else if ((flags & FormattableFlags.PLUS) != 0)
      {
        builder.insert(insertPoint, '+');
        ++insertPoint;
      }
    else if ((flags & FormattableFlags.SPACE) != 0)
      {
        builder.insert(insertPoint, ' ');
        ++insertPoint;
      }

    // Insert the radix prefix.
    if ((flags & FormattableFlags.ALTERNATE) != 0)
      {
        builder.insert(insertPoint, radix == 8 ? "0" : "0x");
        insertPoint += radix == 8 ? 1 : 2;
      }

    // Now justify the result.
    int resultWidth = builder.length();
    if (resultWidth < width)
      {
        char fill = ((flags & FormattableFlags.ZERO) != 0) ? '0' : ' ';
        if ((flags & FormattableFlags.LEFT_JUSTIFY) != 0)
          {
            // Left justify.
            if (fill == ' ')
              insertPoint = builder.length();
          }
        else
          {
            // Right justify.  Insert spaces before the radix prefix
            // and sign.
            insertPoint = 0;
          }
        while (resultWidth++ < width)
          builder.insert(insertPoint, fill);
      }

    String result = builder.toString();
    if ((flags & FormattableFlags.UPPERCASE) != 0)
      {
        if (fmtLocale == null)
          result = result.toUpperCase();
        else
          result = result.toUpperCase(fmtLocale);
      }

    out.append(result);
  }

  /**
   * Emit a decimal value.
   *
   * @param arg the hexadecimal or octal value.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param conversion the conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void decimalConversion(Object arg, int flags, int width,
                                 int precision, char conversion)
    throws IOException
  {
    CPStringBuilder builder = basicIntegralConversion(arg, flags, width,
                                                      precision, 10,
                                                      conversion);
    boolean isNegative = false;
    if (builder.charAt(0) == '-')
      {
        // Sign handling is done during localization.
        builder.deleteCharAt(0);
        isNegative = true;
      }

    applyLocalization(builder, flags, width, isNegative);
    genericFormat(builder.toString(), flags, width, precision);
  }

  /**
   * Emit a single date or time conversion to a StringBuilder.
   *
   * @param builder the builder to write to.
   * @param cal the calendar to use in the conversion.
   * @param conversion the formatting character to specify the type of data.
   * @param syms the date formatting symbols.
   */
  private void singleDateTimeConversion(CPStringBuilder builder, Calendar cal,
                                        char conversion,
                                        DateFormatSymbols syms)
  {
    int oldLen = builder.length();
    int digits = -1;
    switch (conversion)
      {
      case 'H':
        builder.append(cal.get(Calendar.HOUR_OF_DAY));
        digits = 2;
        break;
      case 'I':
        builder.append(cal.get(Calendar.HOUR));
        digits = 2;
        break;
      case 'k':
        builder.append(cal.get(Calendar.HOUR_OF_DAY));
        break;
      case 'l':
        builder.append(cal.get(Calendar.HOUR));
        break;
      case 'M':
        builder.append(cal.get(Calendar.MINUTE));
        digits = 2;
        break;
      case 'S':
        builder.append(cal.get(Calendar.SECOND));
        digits = 2;
        break;
      case 'N':
        // FIXME: nanosecond ...
        digits = 9;
        break;
      case 'p':
        {
          int ampm = cal.get(Calendar.AM_PM);
          builder.append(syms.getAmPmStrings()[ampm]);
        }
        break;
      case 'z':
        {
          int zone = cal.get(Calendar.ZONE_OFFSET) / (1000 * 60);
          builder.append(zone);
          digits = 4;
          // Skip the '-' sign.
          if (zone < 0)
            ++oldLen;
        }
        break;
      case 'Z':
        {
          // FIXME: DST?
          int zone = cal.get(Calendar.ZONE_OFFSET) / (1000 * 60 * 60);
          String[][] zs = syms.getZoneStrings();
          builder.append(zs[zone + 12][1]);
        }
        break;
      case 's':
        {
          long val = cal.getTime().getTime();
          builder.append(val / 1000);
        }
        break;
      case 'Q':
        {
          long val = cal.getTime().getTime();
          builder.append(val);
        }
        break;
      case 'B':
        {
          int month = cal.get(Calendar.MONTH);
          builder.append(syms.getMonths()[month]);
        }
        break;
      case 'b':
      case 'h':
        {
          int month = cal.get(Calendar.MONTH);
          builder.append(syms.getShortMonths()[month]);
        }
        break;
      case 'A':
        {
          int day = cal.get(Calendar.DAY_OF_WEEK);
          builder.append(syms.getWeekdays()[day]);
        }
        break;
      case 'a':
        {
          int day = cal.get(Calendar.DAY_OF_WEEK);
          builder.append(syms.getShortWeekdays()[day]);
        }
        break;
      case 'C':
        builder.append(cal.get(Calendar.YEAR) / 100);
        digits = 2;
        break;
      case 'Y':
        builder.append(cal.get(Calendar.YEAR));
        digits = 4;
        break;
      case 'y':
        builder.append(cal.get(Calendar.YEAR) % 100);
        digits = 2;
        break;
      case 'j':
        builder.append(cal.get(Calendar.DAY_OF_YEAR));
        digits = 3;
        break;
      case 'm':
        builder.append(cal.get(Calendar.MONTH) + 1);
        digits = 2;
        break;
      case 'd':
        builder.append(cal.get(Calendar.DAY_OF_MONTH));
        digits = 2;
        break;
      case 'e':
        builder.append(cal.get(Calendar.DAY_OF_MONTH));
        break;
      case 'R':
        singleDateTimeConversion(builder, cal, 'H', syms);
        builder.append(':');
        singleDateTimeConversion(builder, cal, 'M', syms);
        break;
      case 'T':
        singleDateTimeConversion(builder, cal, 'H', syms);
        builder.append(':');
        singleDateTimeConversion(builder, cal, 'M', syms);
        builder.append(':');
        singleDateTimeConversion(builder, cal, 'S', syms);
        break;
      case 'r':
        singleDateTimeConversion(builder, cal, 'I', syms);
        builder.append(':');
        singleDateTimeConversion(builder, cal, 'M', syms);
        builder.append(':');
        singleDateTimeConversion(builder, cal, 'S', syms);
        builder.append(' ');
        singleDateTimeConversion(builder, cal, 'p', syms);
        break;
      case 'D':
        singleDateTimeConversion(builder, cal, 'm', syms);
        builder.append('/');
        singleDateTimeConversion(builder, cal, 'd', syms);
        builder.append('/');
        singleDateTimeConversion(builder, cal, 'y', syms);
        break;
      case 'F':
        singleDateTimeConversion(builder, cal, 'Y', syms);
        builder.append('-');
        singleDateTimeConversion(builder, cal, 'm', syms);
        builder.append('-');
        singleDateTimeConversion(builder, cal, 'd', syms);
        break;
      case 'c':
        singleDateTimeConversion(builder, cal, 'a', syms);
        builder.append(' ');
        singleDateTimeConversion(builder, cal, 'b', syms);
        builder.append(' ');
        singleDateTimeConversion(builder, cal, 'd', syms);
        builder.append(' ');
        singleDateTimeConversion(builder, cal, 'T', syms);
        builder.append(' ');
        singleDateTimeConversion(builder, cal, 'Z', syms);
        builder.append(' ');
        singleDateTimeConversion(builder, cal, 'Y', syms);
        break;
      default:
        throw new UnknownFormatConversionException(String.valueOf(conversion));
      }

    if (digits > 0)
      {
        int newLen = builder.length();
        int delta = newLen - oldLen;
        while (delta++ < digits)
          builder.insert(oldLen, '0');
      }
  }

  /**
   * Emit a date or time value.
   *
   * @param arg the date or time value.
   * @param flags the formatting flags to use.
   * @param width the width to use.
   * @param precision the precision to use.
   * @param conversion the conversion character.
   * @param subConversion the sub conversion character.
   * @throws IOException if the output stream throws an I/O error.
   */
  private void dateTimeConversion(Object arg, int flags, int width,
                                  int precision, char conversion,
                                  char subConversion)
    throws IOException
  {
    noPrecision(precision);
    checkFlags(flags,
               FormattableFlags.LEFT_JUSTIFY | FormattableFlags.UPPERCASE,
               conversion);

    Calendar cal;
    if (arg instanceof Calendar)
      cal = (Calendar) arg;
    else
      {
        Date date;
        if (arg instanceof Date)
          date = (Date) arg;
        else if (arg instanceof Long)
          date = new Date(((Long) arg).longValue());
        else
          throw new IllegalFormatConversionException(conversion,
                                                     arg.getClass());
        if (fmtLocale == null)
          cal = Calendar.getInstance();
        else
          cal = Calendar.getInstance(fmtLocale);
        cal.setTime(date);
      }

    // We could try to be more efficient by computing this lazily.
    DateFormatSymbols syms;
    if (fmtLocale == null)
      syms = new DateFormatSymbols();
    else
      syms = new DateFormatSymbols(fmtLocale);

    CPStringBuilder result = new CPStringBuilder();
    singleDateTimeConversion(result, cal, subConversion, syms);

    genericFormat(result.toString(), flags, width, precision);
  }

  /**
   * Advance the internal parsing index, and throw an exception
   * on overrun.
   *
   * @throws IllegalArgumentException on overrun.
   */
  private void advance()
  {
    ++index;
    if (index >= length)
      {
        // FIXME: what exception here?
        throw new IllegalArgumentException();
      }
  }

  /**
   * Parse an integer appearing in the format string.  Will return -1
   * if no integer was found.
   *
   * @return the parsed integer.
   */
  private int parseInt()
  {
    int start = index;
    while (Character.isDigit(format.charAt(index)))
      advance();
    if (start == index)
      return -1;
    return Integer.parseInt(format.substring(start, index));
  }

  /**
   * Parse the argument index.  Returns -1 if there was no index, 0 if
   * we should re-use the previous index, and a positive integer to
   * indicate an absolute index.
   *
   * @return the parsed argument index.
   */
  private int parseArgumentIndex()
  {
    int result = -1;
    int start = index;
    if (format.charAt(index) == '<')
      {
        result = 0;
        advance();
      }
    else if (Character.isDigit(format.charAt(index)))
      {
        result = parseInt();
        if (format.charAt(index) == '$')
          advance();
        else
          {
            // Reset.
            index = start;
            result = -1;
          }
      }
    return result;
  }

  /**
   * Parse a set of flags and return a bit mask of values from
   * FormattableFlags.  Will throw an exception if a flag is
   * duplicated.
   *
   * @return the parsed flags.
   */
  private int parseFlags()
  {
    int value = 0;
    int start = index;
    while (true)
      {
        int x = FLAGS.indexOf(format.charAt(index));
        if (x == -1)
          break;
        int newValue = 1 << x;
        if ((value & newValue) != 0)
          throw new DuplicateFormatFlagsException(format.substring(start,
                                                                   index + 1));
        value |= newValue;
        advance();
      }
    return value;
  }

  /**
   * Parse the width part of a format string.  Returns -1 if no width
   * was specified.
   *
   * @return the parsed width.
   */
  private int parseWidth()
  {
    return parseInt();
  }

  /**
   * If the current character is '.', parses the precision part of a
   * format string.  Returns -1 if no precision was specified.
   *
   * @return the parsed precision.
   */
  private int parsePrecision()
  {
    if (format.charAt(index) != '.')
      return -1;
    advance();
    int precision = parseInt();
    if (precision == -1)
      // FIXME
      throw new IllegalArgumentException();
    return precision;
  }

  /**
   * Outputs a formatted string based on the supplied specification,
   * <code>fmt</code>, and its arguments using the specified locale.
   * The locale of the formatter does not change as a result; the
   * specified locale is just used for this particular formatting
   * operation.  If the locale is <code>null</code>, then no
   * localization is applied.
   *
   * @param loc the locale to use for this format.
   * @param fmt the format specification.
   * @param args the arguments to apply to the specification.
   * @throws IllegalFormatException if there is a problem with
   *                                the syntax of the format
   *                                specification or a mismatch
   *                                between it and the arguments.
   * @throws FormatterClosedException if the formatter is closed.
   */
  public Formatter format(Locale loc, String fmt, Object... args)
  {
    if (closed)
      throw new FormatterClosedException();

    // Note the arguments are indexed starting at 1.
    int implicitArgumentIndex = 1;
    int previousArgumentIndex = 0;

    try
      {
        fmtLocale = loc;
        format = fmt;
        length = format.length();
        for (index = 0; index < length; ++index)
          {
            char c = format.charAt(index);
            if (c != '%')
              {
                out.append(c);
                continue;
              }

            int start = index;
            advance();

            // We do the needed post-processing of this later, when we
            // determine whether an argument is actually needed by
            // this conversion.
            int argumentIndex = parseArgumentIndex();

            int flags = parseFlags();
            int width = parseWidth();
            int precision = parsePrecision();
            char origConversion = format.charAt(index);
            char conversion = origConversion;
            if (Character.isUpperCase(conversion))
              {
                flags |= FormattableFlags.UPPERCASE;
                conversion = Character.toLowerCase(conversion);
              }

            Object argument = null;
            if (conversion == '%' || conversion == 'n')
              {
                if (argumentIndex != -1)
                  {
                    // FIXME: not sure about this.
                    throw new UnknownFormatConversionException("FIXME");
                  }
              }
            else
              {
                if (argumentIndex == -1)
                  argumentIndex = implicitArgumentIndex++;
                else if (argumentIndex == 0)
                  argumentIndex = previousArgumentIndex;
                // Argument indices start at 1 but array indices at 0.
                --argumentIndex;
                if (argumentIndex < 0 || argumentIndex >= args.length)
                  throw new MissingFormatArgumentException(format.substring(start, index));
                argument = args[argumentIndex];
              }

            switch (conversion)
              {
              case 'b':
                booleanFormat(argument, flags, width, precision,
                              origConversion);
                break;
              case 'h':
                hashCodeFormat(argument, flags, width, precision,
                               origConversion);
                break;
              case 's':
                stringFormat(argument, flags, width, precision,
                             origConversion);
                break;
              case 'c':
                characterFormat(argument, flags, width, precision,
                                origConversion);
                break;
              case 'd':
                checkFlags(flags & FormattableFlags.UPPERCASE, 0, 'd');
                decimalConversion(argument, flags, width, precision,
                                  origConversion);
                break;
              case 'o':
                checkFlags(flags & FormattableFlags.UPPERCASE, 0, 'o');
                hexOrOctalConversion(argument, flags, width, precision, 8,
                                     origConversion);
                break;
              case 'x':
                hexOrOctalConversion(argument, flags, width, precision, 16,
                                     origConversion);
              case 'e':
                // scientificNotationConversion();
                break;
              case 'f':
                // floatingDecimalConversion();
                break;
              case 'g':
                // smartFloatingConversion();
                break;
              case 'a':
                // hexFloatingConversion();
                break;
              case 't':
                advance();
                char subConversion = format.charAt(index);
                dateTimeConversion(argument, flags, width, precision,
                                   origConversion, subConversion);
                break;
              case '%':
                percentFormat(flags, width, precision);
                break;
              case 'n':
                newLineFormat(flags, width, precision);
                break;
              default:
                throw new UnknownFormatConversionException(String.valueOf(origConversion));
              }
          }
      }
    catch (IOException exc)
      {
        ioException = exc;
      }
    return this;
  }

  /**
   * Outputs a formatted string based on the supplied specification,
   * <code>fmt</code>, and its arguments using the formatter's locale.
   *
   * @param format the format specification.
   * @param args the arguments to apply to the specification.
   * @throws IllegalFormatException if there is a problem with
   *                                the syntax of the format
   *                                specification or a mismatch
   *                                between it and the arguments.
   * @throws FormatterClosedException if the formatter is closed.
   */
  public Formatter format(String format, Object... args)
  {
    return format(locale, format, args);
  }

  /**
   * Returns the last I/O exception thrown by the
   * <code>append()</code> operation of the underlying
   * output stream.
   *
   * @return the last I/O exception.
   */
  public IOException ioException()
  {
    return ioException;
  }

  /**
   * Returns the locale used by this formatter.
   *
   * @return the formatter's locale.
   * @throws FormatterClosedException if the formatter is closed.
   */
  public Locale locale()
  {
    if (closed)
      throw new FormatterClosedException();
    return locale;
  }

  /**
   * Returns the output stream used by this formatter.
   *
   * @return the formatter's output stream.
   * @throws FormatterClosedException if the formatter is closed.
   */
  public Appendable out()
  {
    if (closed)
      throw new FormatterClosedException();
    return out;
  }

  /**
   * Returns the result of applying {@link Object#toString()}
   * to the underlying output stream.  The results returned
   * depend on the particular {@link Appendable} being used.
   * For example, a {@link StringBuilder} will return the
   * formatted output but an I/O stream will not.
   *
   * @throws FormatterClosedException if the formatter is closed.
   */
  public String toString()
  {
    if (closed)
      throw new FormatterClosedException();
    return out.toString();
  }
}
