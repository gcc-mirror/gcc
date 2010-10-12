/* java.util.Scanner -- Parses primitive types and strings using regexps
   Copyright (C) 2007  Free Software Foundation, Inc.

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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import java.math.BigDecimal;
import java.math.BigInteger;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.ReadableByteChannel;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.ParseException;

import java.util.Iterator;
import java.util.Locale;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author E0327023 Hernadi Laszlo
*/
public class Scanner
  implements Iterator <String>
{
  private static final String NOT_LONG = "\" is not a long";    //$NON-NLS-1$

  private static final String ERR_PREFIX = "\"";        //$NON-NLS-1$

  private static final String NOT_INT = "\" is not an integer"; //$NON-NLS-1$

  private static final String NOT_DOUBLE = "\" is not a double";        //$NON-NLS-1$

  private static final String NOT_BYTE = "\" is not a byte";    //$NON-NLS-1$

  private static final String NOT_BOOLEAN = "\" is not a boolean";      //$NON-NLS-1$

  private static final String IS_NOT = "\" is not ";    //$NON-NLS-1$

  private static final String DEFAULT_PATTERN_S = "\\p{javaWhitespace}+";       //$NON-NLS-1$

  private static final Pattern DEFAULT_PATTERN =
    Pattern.compile (DEFAULT_PATTERN_S);

  private static final String BIG_INTEGER = "BigInteger";       //$NON-NLS-1$

  private final static String NEW_LINE =
    System.getProperty ("line.separator");

  private IOException lastIOException = null;

  /**
   * An InputStream source if a Constructor with an InputStream source is called, otherwise it
   * stays <source> null </source>.
   */
  private InputStream bIS = null;

  /**
   * Length of the input Buffer, which is the maximum bytes to be read at once.
   */
  private final int MaxBufferLen = 1000000;

  /**
   * Minimum buffer length. If there are less chars in the Buffer than this value reading from
   * source is tried.
   */
  private final int MIN_BUF_LEN = 100;

  /**
   * Maximum number of processed chars in the Buffer. If exeeded, all processed chars from the
   * beginning of the Buffer will be discarded to save space. The bytes left are copyed into a new
   * Buffer.
   */
  private final int MAX_PREFIX = 10000;

  /**
   * The Buffer which is used by the Matcher to find given patterns. It is filled up when matcher
   * hits end or <code> MIN_BUF_LEN </code> is reached.
   */
  private String actBuffer = new String ();

  /**
   * The current radix to use by the methods getNextXXX and hasNextXXX.
   */
  private int currentRadix = 10;

  /**
   * The current locale.
   *
   * @see #useLocale(Locale)
   * @see #locale()
   */
  private Locale actLocale = Locale.getDefault ();

  /**
   * The current pattern for the matcher.
   */
  private Pattern p = DEFAULT_PATTERN;

  /**
   * The current position in the Buffer, at which the next match should start.
   */
  private int actPos = 0;

  /**
   * A global buffer to save new allocations by reading from source.
   */
  private final byte[] tmpBuffer = new byte[this.MaxBufferLen];

  /**
   * The charsetName to use with the source.
   */
  private String charsetName = null;

  /**
   * The Matcher which is used.
   */
  private Matcher myMatcher = this.p.matcher (this.actBuffer);

  /**
   * The MatchResult is generated at each match, even if match() isn't called.
   */
  private MatchResult actResult = null;

  /**
   * A Readable source if a Constructor with a Readable source is called, otherwise it stays
   * <source> null </source>.
   */
  private Readable readableSource = null;

  /**
   * A ReadableByteChannel source if a Constructor with a ReadableByteChannel source is called,
   * otherwise it stays <source> null </source>.
   */
  private ReadableByteChannel rbcSource = null;

  /**
   * Indicates if the close() method was called.
   */
  private boolean isClosed = false;

  /**
   * For performance reasons the last Found is saved, if a hasNextXXX method was called.
   */
  private String lastFound = null;

  private boolean lastFoundPresent = false;

  private int lastNextPos = 0;

  private int lastPatternHash = 0;

  private int last_RegionStart = 0;

  private int last_RegionEnd = 0;

  private boolean last_anchor = false;

  private boolean last_transparent = false;

  private MatchResult lastResult = null;

  /**
   * To keep track of the current position in the stream for the toString method, each time
   * processed chars are removed the amount is added to processedChars.
   */
  private int procesedChars = 0;

  /**
   * needInput is set <code> true </code> before a read method, and if there is no input it blocks
   * and stays <code>true</code>. Right after a read it is set to <code>false</code>.
   */
  private boolean needInput = false;

  private boolean skipped = false;

  /**
   * <code> {@link #doSkipp} </code> indicates that the found pattern belongs to the result. If
   * <code> {@link #doSkipp} </code> is false the match result ends at the beginning of the match.
   * In both cases the current position is set after the pattern, if the found pattern has to be
   * removed, a nextXXX method is called.
   */
  private boolean doSkipp = false;

  /**
   * Indicates if the last match was valid or not.
   */
  private boolean matchValid = false;

  private NumberFormat actFormat = NumberFormat.getInstance (this.actLocale);

  private DecimalFormat df = (DecimalFormat) this.actFormat;

  /**
   * Indicates if current Locale should be used at the input.
   */
  private boolean useLocale = true;

  private DecimalFormatSymbols dfs =
    new DecimalFormatSymbols (this.actLocale);

  /**
   * Constructs a new Scanner with the given File as source.
   * {@link #Scanner(InputStream, String)} is called with <code> null </code> as charsetName.
   *
   * @param source
   *            The File to use as source.
   * @throws FileNotFoundException
   *             If the file is not found an Exception is thrown.
   */
  public Scanner (final File source) throws FileNotFoundException       // TESTED
  {
    this (source, null);
  }

  /**
   * Constructs a new Scanner with the given File as source. <br>
   * {@link #Scanner(InputStream, String)} is called with the given charsetName.
   *
   * @param source
   *            The File to use as source.
   * @param charsetName
   *            Current charset name of the file. If charsetName is null it behaves if it was not
   *            set.
   * @throws FileNotFoundException
   *             If the file is not found an Exception is thrown.
   */
  public Scanner (final File source,
                  final String charsetName) throws FileNotFoundException
  {
    this (new FileInputStream (source), charsetName);
  }

  /**
   * Constructs a new Scanner with the given inputStream. <br>
   * {@link #Scanner(InputStream, String)} is called with <code> null </code> as charsetName.
   *
   * @param source
   *            The InputStream to use as source.
   */
  public Scanner (final InputStream source)     // TESTED
  {
    this (source, null);
  }

  /**
   * Constructs a new Scanner with the InputSream and a charsetName. Afterwards the Buffer is
   * filled.
   *
   * @param source
   *            The InputStream to use as source.
   * @param charsetName
   *            The charsetName to apply on the source's data.
   */
  public Scanner (final InputStream source, final String charsetName)
  {
    this.bIS = (new BufferedInputStream (source));
    this.charsetName = charsetName;
    myFillBuffer ();
  }

  /**
   * Constructs a new Scanner with a Readable input as source.
   *
   * @param source
   *            The Readable to use as source.
   */
  public Scanner (final Readable source)
  {
    this.readableSource = source;
    myFillBuffer ();
  }

  /**
   * Constructs a new Scanner with a ReadableByteChannel as
   * source. Therfore the {@link #Scanner(ReadableByteChannel,
   * String)} is called with <code> null </code> as charsetName.
   *
   * @param source
   *            The ReadableByteChannel to use as source.
   */
  public Scanner (final ReadableByteChannel source)
  {
    this (source, null);
  }

  /**
   * Constructs a new Scanner with a ReadableByteChannel as source and
   * a given charsetName, which is to be applied on it. <br> It also
   * initiates the main Buffer.
   *
   * @param source
   *            The ReadableByteChannel to use as source.
   * @param charsetName
   *            The charsetName to be applied on the source.
   */
  public Scanner (final ReadableByteChannel source, final String charsetName)
  {
    this.charsetName = charsetName;
    this.rbcSource = source;
    myFillBuffer ();
  }

  /**
   * Constructs a new Scanner using the given String as input only.
   *
   * @param source
   *            The whole String to be used as source.
   */
  public Scanner (final String source)  // TESTED
  {
    this.actBuffer = new String (source);
    this.myMatcher.reset (this.actBuffer);
  }

  /**
   * Closes this Scanner. If an {@link IOException} occurs it is
   * catched and is available under {@link #ioException()}.<br> After
   * the Scanner is closed, all searches will lead to a {@link
   * IllegalStateException}.
   */
  public void close ()
  {
    try
    {
      if (this.bIS != null)
        this.bIS.close ();
      if (this.rbcSource != null)
        this.rbcSource.close ();
      this.isClosed = true;
    }
    catch (IOException ioe)
    {
      this.lastIOException = ioe;
    }
  }

  /**
   * Returns the current delimiter.
   *
   * @return the current delimiter.
   */
  public Pattern delimiter ()   // TESTED
  {
    return this.p;
  }

  /**
   * Tries to find the pattern in the current line.
   *
   * @param pattern The pattern which should be searched in the
   * current line of the input.
   * @throws NoSuchElementException
   *             If the pattern was not found.
   * @return If the search was successful, the result or otherwise a
   *         {@link NoSuchElementException} is thrown.
   */
  public String findInLine (final Pattern pattern) throws NoSuchElementException        // TESTED
  {
    String tmpStr = myNextLine (false);
    return myFindPInStr (pattern, tmpStr, 0);
  }

  /**
   * Compiles the given pattern into a {@link Pattern} and calls
   * {@link #findInLine(Pattern)} with the compiled pattern and
   * returns whatever it returns.
   *
   * @param pattern
   *            The pattern which should be matched in the input.
   * @throws NoSuchElementException
   *             If the pattern was not found.
   * @return The match in the current line.
   */
  public String findInLine (final String pattern)       // TESTED
  {
    return findInLine (Pattern.compile (pattern));
  }

  /**
   * Trys to match the pattern within the given horizon.
   *
   * @param pattern
   *            Pattern to search.
   * @param horizon
   * @return The result of the match.
   * @throws IllegalArgumentException
   *             if the horizon is negative.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public String findWithinHorizon (final Pattern pattern, final int horizon)
    throws IllegalArgumentException, IllegalStateException
  {
    if (horizon < 0)
      {
        throw new IllegalArgumentException (horizon + " is negative");
      }

    if (this.isClosed)
      {
        throw new IllegalStateException ("Scanner is closed");
      }

    // doSkipp is set true to get the matching patern together with the found String
    this.doSkipp = true;
    String rc = myFindPInStr (pattern, this.actBuffer, horizon);

    if (rc != null)
      {
        this.actPos += rc.length ();
      }

    return rc;
  }

  /**
   * Compile the pattern and call {@link #findWithinHorizon(Pattern,
   * int)}.
   *
   * @param pattern
   *            Pattern to search.
   * @param horizon
   * @return The result of the match.
   * @throws IllegalArgumentException
   *             if the horizon is negative.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public String findWithinHorizon (final String pattern, final int horizon)
    throws IllegalArgumentException, IllegalStateException
  {
    return findWithinHorizon (Pattern.compile (pattern), horizon);
  }

  /**
   * Checks if there is any next String using the current
   * delimiter. Therefore the string must not be <code> null </code>
   * and the length must be greater then 0. If a {@link
   * NoSuchElementException} is thrown by the search method, it is
   * catched and false is returned.
   *
   * @return <code> true </code> if there is any result using the current delimiter. This wouldn't
   *         lead to a {@link NoSuchElementException}.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNext () throws IllegalStateException        // TESTED
  {
    String tmpStr = null;

    try
    {
      tmpStr = myCoreNext (false, this.p);
    }
    catch (NoSuchElementException nf)
    {
    }

    if (tmpStr == null || tmpStr.length () <= 0)
      {
        return false;
      }
    return true;
  }

  /**
   * Searches the pattern in the next subString before the next
   * current delimiter.
   *
   * @param pattern
   *            The pattern to search for.
   * @return <code> true </code> if the pattern is found before the current delimiter.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNext (final Pattern pattern) throws IllegalStateException   // TESTED
  {
    String tmpStr;

      tmpStr = myNext (pattern, false);

    if (tmpStr == null || tmpStr.length () <= 0)
      {
        return false;
      }
    return true;
  }

  /**
   * Compiles the pattern to a {@link Pattern} and calls {@link
   * #hasNext(Pattern)}.
   *
   * @see #hasNext(Pattern)
   * @param pattern
   *            The pattern as string to search for.
   * @return <code> true </code> if the pattern is found before the current delimiter.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNext (final String pattern) throws IllegalStateException    // TESTED
  {
    return hasNext (Pattern.compile (pattern));
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a BigDecimal number. <br> BigDecimal numbers are always tryed
   * with radix 10.
   *
   * @see #nextBigDecimal()
   * @return <code> true </code> if the next string is a BigDecimal number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextBigDecimal () throws IllegalStateException      // TESTED
  {
    try
    {
      myBigDecimal (false);
      return true;
    }
    catch (InputMismatchException nfe)
    {
      return false;
    }
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a BigInteger number. <br> Call {@link #hasNextBigInteger(int)}
   * with the current radix.
   *
   * @see #nextBigInteger()
   * @return <code> true </code> if the next string is a BigInteger number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextBigInteger () throws IllegalStateException      // TESTED
  {
    return hasNextBigInteger (this.currentRadix);
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a BigInteger number. <br>
   *
   * @param radix
   *            The radix to use for this check. The global radix of the Scanner will not be
   *            changed.
   * @return <code> true </code> if the next string is a BigInteger number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextBigInteger (final int radix) throws
    IllegalStateException
  {
    try
    {
      myNextBigInteger (radix, false, BIG_INTEGER);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if the next string could be a boolean. The method handles
   * the input not case sensitiv, so "true" and "TRUE" and even "tRuE"
   * are <code> true </code>.
   *
   * @see #nextBoolean()
   * @return Return <code> true </code> if the next string is a boolean.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextBoolean () throws IllegalStateException // TESTED
  {
    try
    {
      myNextBoolean (false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a byte number. <br> Calls {@link #hasNextByte(int)} with the
   * current radix.
   *
   * @see #nextByte()
   * @return <code> true </code> if the next string is a byte number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextByte () throws IllegalStateException    // TESTED
  {
    return hasNextByte (this.currentRadix);
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a byte number with the given radix. <br> To check, the private
   * method {@link #myNextByte(int, boolean)} is called, and if no
   * error occurs the next string could be a byte.
   *
   * @see #nextByte(int)
   * @param radix The radix to use for this check. The global radix of
   * the Scanner will not be changed.
   * @return <code> true </code> if the next string is a byte number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextByte (final int radix) throws IllegalStateException
  {
    try
    {
      myNextByte (radix, false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a double number. <br> To check, the private method {@link
   * #myNextDouble(boolean)} is called, and if no error occurs the
   * next string could be a double.
   *
   * @see #nextDouble()
   * @return <code> true </code> if the next string is a double number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextDouble () throws IllegalStateException  // TESTED
  {
    try
    {
      myNextDouble (false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a double number. Because every float is a double this is
   * checked.<br> To check, the private method {@link
   * #myNextDouble(boolean)} is called, and if no error occurs the
   * next string could be a double.
   *
   * @see #nextFloat()
   * @return <code> true </code> if the next string is a double number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextFloat () throws IllegalStateException   // TESTED
  {
    try
    {
      myNextDouble (false);
      // myNextFloat(false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * an int number. <br> To check, the private method {@link
   * #myNextInt(int, boolean)} is called, and if no error occurs the
   * next string could be an int.
   *
   * @see #nextInt(int)
   * @return <code> true </code> if the next string is an int number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextInt () throws IllegalStateException     // TESTED
  {
    return hasNextInt (this.currentRadix);
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * an int number with the given radix. <br> To check, the private
   * method {@link #myNextInt(int, boolean)} is called, and if no
   * error occurs the next string could be an int.
   *
   * @see #nextInt(int)
   * @param radix
   *            The radix to use for this check. The global radix of the Scanner will not be
   *            changed.
   * @return <code> true </code> if the next string is an int number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextInt (final int radix) throws IllegalStateException
  {
    try
    {
      myNextInt (radix, false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if there is a current line, which ends at the next line
   * break or the end of the input.
   *
   * @return <code> true </code> if there is a current line.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextLine () throws IllegalStateException    // TESTED
  {
    return (myNextLine (false) != null);
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a long number. <br> To check, the private method {@link
   * #myNextLong(int, boolean)} is called, and if no error occurs the
   * next string could be a long.
   *
   * @see #nextLong()
   * @return <code> true </code> if the next string is a long number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextLong () throws IllegalStateException    // TESTED
  {
    return hasNextLong (this.currentRadix);
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a long number with the given radix. <br> To check, the private
   * method {@link #myNextLong(int, boolean)} is called, and if no
   * error occurs the next string could be a long.
   *
   * @see #nextLong(int)
   * @param radix
   *            The radix to use for this check. The global radix of the Scanner will not be
   *            changed.
   * @return <code> true </code> if the next string is a long number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextLong (final int radix) throws IllegalStateException
  {
    try
    {
      myNextLong (radix, false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a short number with the given radix. <br> To check, the private
   * method {@link #myNextShort(int, boolean)} is called, and if no
   * error occurs the next string could be a short.
   *
   * @see #nextShort(int)
   * @return <code> true </code> if the next string is a short number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextShort () throws IllegalStateException   // TESTED
  {
    return hasNextShort (this.currentRadix);
  }

  /**
   * Checks if the string to the next delimiter can be interpreted as
   * a short number. <br> To check, the private method {@link
   * #myNextShort(int, boolean)} is called, and if no error occurs the
   * next string could be a short.
   *
   * @see #nextShort(int)
   * @param radix
   *            The radix to use for this check. The global radix of the Scanner will not be
   *            changed.
   * @return <code> true </code> if the next string is a short number.
   * @throws IllegalStateException
   *             if the Scanner is closed.
   */
  public boolean hasNextShort (final int radix) throws IllegalStateException
  {
    try
    {
      myNextShort (radix, false);
      return true;
    }
    catch (InputMismatchException ime)
    {
      return false;
    }
  }

  /**
   * Returns the last {@link IOException} occured.
   *
   * @return Returns the last {@link IOException}.
   */
  public IOException ioException ()
  {
    return this.lastIOException;
  }

  /**
   * Returns the current value of {@link #useLocale}. This is used to
   * tell the Scanner if it should use the Locale format or just
   * handle numbers of the default format.
   *
   * @see #setUseLocale(boolean)
   * @return the useLoclae.
   */
  public boolean isUseLocale () // TESTED
  {
    return this.useLocale;
  }

  /**
   * Returns the current Locale. It is initialized with {@link
   * Locale#getDefault()}.
   *
   * @see #useLocale(Locale)
   * @return Returns the current Locale.
   */
  public Locale locale ()       // TESTED
  {
    return this.actLocale;
  }

  /**
   * Returns the last MatchResult found. This is updated after every
   * successfully search.
   *
   * @return Returns the last {@link MatchResult} found.
   */
  public MatchResult match ()   // TESTED
  {
    return this.actResult;
  }

  /**
   * Uses the current delimiter to find the next string in the
   * buffer. If a string is found the current position is set after
   * the delimiter, otherwise a {@link NoSuchElementException} is
   * thrown. A successful match sets the matchResult.
   *
   * @see #match()
   * @return Returns the next string of the buffer.
   * @throws NoSuchElementException
   *             If no element was found an exception is thrown.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public String next () throws NoSuchElementException, IllegalStateException    // TESTED
  {
    return myCoreNext (true, this.p);
  }

  /**
   * Tries to match the buffer with the given pattern. The current
   * delimiter will not be changed.
   *
   * @param pattern
   *            The pattern to match.
   * @return Returns the next string matching the pattern.
   * @throws NoSuchElementException
   *             If no element was found an exception is thrown.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public String next (final Pattern pattern) throws NoSuchElementException, IllegalStateException       // TESTED
  {
    return myNext (pattern, true);
  }

  /**
   * Tries to match the buffer with the given pattern. The current
   * delimiter will not be changed.  Calls the {@link #next(Pattern)}
   * with the compiled pattern.
   *
   * @see #next(Pattern)
   * @param pattern
   *            The pattern to match.
   * @return Returns the next string matching the pattern.
   * @throws NoSuchElementException
   *             If no element was found an exception is thrown.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public String next (final String pattern) throws NoSuchElementException, IllegalStateException        // TESTED
  {
    return next (Pattern.compile (pattern));
  }

  /**
   * Tries to interpret the next string as a BigDecimal value.
   *
   * @return Returns the BigDecimal value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a BigDecimal.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public BigDecimal nextBigDecimal () throws NoSuchElementException, IllegalStateException      // TESTED
  {
    return myBigDecimal (true);
  }

  /**
   * Tries to interpret the next string as a BigInteger value. Call
   * {@link #nextBigInteger(int)} with the current radix as parameter,
   * and return the value.
   *
   * @see #nextBigInteger(int)
   * @return Returns the BigInteger value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a BigInteger.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public BigInteger nextBigInteger () throws NoSuchElementException, IllegalStateException      // TESTED
  {
    return nextBigInteger (this.currentRadix);
  }

  /**
   * Tries to interpret the next string as a BigInteger value with the
   * given radix.
   *
   * @param radix
   *            The radix to be used for this BigInteger. The current radix of the Scanner is not
   *            changed.
   * @return Returns the BigInteger value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a BigInteger.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public BigInteger nextBigInteger (final int radix) throws
    NoSuchElementException, IllegalStateException
  {
    return myNextBigInteger (radix, true, BIG_INTEGER);
  }

  /**
   * Tries to interpret the next string to the delimiter as a boolean
   * value, ignoring case.
   *
   * @return Returns the boolean value of the next matching string or throws an exception.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a boolean.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public boolean nextBoolean () throws NoSuchElementException, IllegalStateException    // TESTED
  {
    return myNextBoolean (true);
  }

  /**
   * Tries to interpret the next string as a byte value. Call {@link
   * #nextByte(int)} with the current radix as parameter, and return
   * the value.
   *
   * @see #nextByte(int)
   * @return Returns the byte value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a byte
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public byte nextByte () throws NoSuchElementException, IllegalStateException  // TESTED
  {
    return nextByte (this.currentRadix);
  }

  /**
   * Tries to interpret the next string as a byte value with the given
   * radix.
   *
   * @param radix
   *            The radix to be used for this byte. The current radix of the Scanner is not
   *            changed.
   * @return Returns the byte value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a byte.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public byte nextByte (final int radix) throws NoSuchElementException,
    IllegalStateException
  {
    return myNextByte (radix, true);
  }

  /**
   * Tries to interpret the next string as a double value.
   *
   * @return Returns the int value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a double.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public double nextDouble () throws NoSuchElementException, IllegalStateException      // TESTED
  {
    return myNextDouble (true);
  }

  /**
   * Tries to interpret the next string as a double value, and then
   * casts down to float.
   *
   * @return Returns the int value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a double.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public float nextFloat () throws NoSuchElementException, IllegalStateException        // TESTED
  {
    return (float) myNextDouble (true);
    // return myNextFloat(true);
  }

  /**
   * Tries to interpret the next string as an int value. Calls {@link
   * #nextInt(int)} with the current radix as parameter, and return
   * the value.
   *
   * @see #nextInt(int)
   * @return Returns the int value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not an int.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public int nextInt () throws NoSuchElementException, IllegalStateException    // TESTED
  {
    return nextInt (this.currentRadix);
  }

  /**
   * Tries to interpret the next string as an int value with the given
   * radix.
   *
   * @param radix
   *            The radix to be used for this int. The current radix of the Scanner is not changed
   * @return Returns the int value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not an int.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public int nextInt (final int radix) throws NoSuchElementException,
    IllegalStateException
  {
    return myNextInt (radix, true);
  }

  /**
   * Tries to match the system line seperator, and returns the current
   * line.
   *
   * @return Returns the current line.
   * @throws NoSuchElementException
   *             If the current delimiter is not found.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public String nextLine () throws NoSuchElementException, IllegalStateException        // TESTED
  {
    return myNextLine (true);
  }

  /**
   * Tries to interpret the next string as a long value. Calls {@link
   * #nextLong(int)} with the current radix as parameter, and return
   * the value.
   *
   * @see #nextLong(int)
   * @return Returns the long value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a long.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public long nextLong () throws NoSuchElementException, IllegalStateException  // TESTED
  {
    return nextLong (this.currentRadix);
  }

  /**
   * Tries to interpret the next string as a long value with the given
   * radix.
   *
   * @param radix
   *            The radix to be used for this long. The current radix of the Scanner is not
   *            changed
   * @return Returns the long value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a long.
   * @throws IllegalStateException
   *             If the Scanner is closed.
   */
  public long nextLong (final int radix) throws NoSuchElementException,
    IllegalStateException
  {
    return myNextLong (radix, true);
  }

  /**
   * Tries to interpret the next string as a short value. Calls {@link
   * #nextShort(int)} with the current radix as parameter, and return
   * the value.
   *
   * @see #nextShort(int)
   * @return Returns the short value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a short.
   */
  public short nextShort () throws NoSuchElementException       // TESTED
  {
    return nextShort (this.currentRadix);
  }

  /**
   * Tries to interpret the next string as a short value with the
   * given radix.
   *
   * @param radix
   *            The radix to be used for this short. The current radix of the Scanner is not
   *            changed.
   * @return Returns the short value of the next string.
   * @throws NoSuchElementException
   *             If no string is found or the string is not a short.
   */
  public short nextShort (final int radix) throws NoSuchElementException
  {
    return myNextShort (radix, true);
  }

  /**
   * @return Returns the current radix.
   */
  public int radix ()
  {
    return this.currentRadix;
  }

  /**
   * The remove operation is not supported by this implementation of
   * Iterator.
   */
  public void remove ()
  {
  }

  /**
   * @param useLocale the useLocale to set.
   */
  public void setUseLocale (final boolean useLocale)    // TESTED
  {
    this.useLocale = useLocale;
  }

  /**
   * Skips the given pattern. Sets skipped <code>true</code>.
   *
   * @param pattern
   *            Pattern which should be skipped.
   * @return <code>this</code> with the skipped buffer.
   * @throws NoSuchElementException
   *             If the Pattern is not found.
   */
  public Scanner skip (final Pattern pattern) throws NoSuchElementException
  {
    this.doSkipp = true;
    int end;
    boolean found;
    Matcher matcher = pattern.matcher (this.actBuffer);
      matcher.region (this.actPos - 1, this.actBuffer.length ());

      found = matcher.find ();
      found = myFillBuffer_loop (matcher, this.actPos - 1, found);
      end = matcher.end ();

      this.actPos = end + 1;

      this.doSkipp = false;
      this.skipped = true;

      actResult = null;

    if (!found)
      {
        throw new NoSuchElementException ();
      }
    return this;
  }

  /**
   * Skips a given pattern. Calls {@link #skip(Pattern)} with the
   * compiled pattern.
   *
   * @see #skip(Pattern)
   * @param pattern
   *            Pattern which should be skipped.
   * @return <code>this</code> with the skipped buffer.
   */
  public Scanner skip (final String pattern)
  {
    return skip (Pattern.compile (pattern));
  }

  /**
   * Returns the string representation of this Scanner.
   */
  @Override
    public String toString ()
  {
    String tmpStr2;
    String rc = this.getClass ().getName ();
    tmpStr2 = rc;
    tmpStr2 = "[delimiters=" + this.p.pattern () + "]";
    rc += tmpStr2;
    tmpStr2 = "[position=" + (this.procesedChars + this.actPos) + "]";
    rc += tmpStr2;
    tmpStr2 = "[match valid=" + this.matchValid + "]";
    rc += tmpStr2;
    tmpStr2 = "[need input=" + this.needInput + "]";
    rc += tmpStr2;
    tmpStr2 = "[source closed=" + this.isClosed + "]";
    rc += tmpStr2;
    tmpStr2 = "[skipped=" + this.skipped + "]";
    rc += tmpStr2;
    tmpStr2 = "[group separator=\\" + this.dfs.getGroupingSeparator () + "]";
    rc += tmpStr2;
    tmpStr2 = "[decimal separator=\\" + this.dfs.getDecimalSeparator () + "]";
    rc += tmpStr2;
    tmpStr2 =
      "[positive prefix=" + myConvert (this.df.getPositivePrefix ()) + "]";
    rc += tmpStr2;
    tmpStr2 =
      "[negative prefix=" + myConvert (this.df.getNegativePrefix ()) + "]";
    rc += tmpStr2;
    tmpStr2 =
      "[positive suffix=" + myConvert (this.df.getPositiveSuffix ()) + "]";
    rc += tmpStr2;
    tmpStr2 =
      "[negative suffix=" + myConvert (this.df.getNegativeSuffix ()) + "]";
    rc += tmpStr2;
    tmpStr2 = "[NaN string=" + myConvert (this.dfs.getNaN ()) + "]";
    rc += tmpStr2;
    tmpStr2 = "[infinity string=" + myConvert (this.dfs.getInfinity ()) + "]";
    rc += tmpStr2;
    return rc;
  }

  /**
   * Sets the current pattern to the given parameter, and updates the
   * {@link Matcher} with the new pattern.
   *
   * @param pattern
   *            The new pattern to use.
   * @return Returns the Scanner (<code>this</code>) with the new pattern.
   */
  public Scanner useDelimiter (final Pattern pattern)   // TESTED
  {
    if (pattern != null)
      {
        this.p = pattern;
        this.myMatcher = this.p.matcher (this.actBuffer);
      }
    return this;
  }

  /**
   * Sets the current pattern to the given parameter. Compiles the
   * pattern and calls {@link #useDelimiter(Pattern)}
   *
   * @see #useDelimiter(Pattern)
   * @param pattern
   *            The new pattern to use.
   * @return Returns the Scanner (<code>this</code>) with the new pattern.
   */
  public Scanner useDelimiter (final String pattern)    // TESTED
  {
    return useDelimiter (Pattern.compile (pattern));
  }

  /**
   * Sets the current Locale to the given parameter. Formats and
   * Symbols are also set using the new Locale.
   *
   * @param locale The new Locale to use. If it is <code>null</code>
   * nothing happens.
   * @return Returns the Scanner (<code>this</code>) with the new Locale.
   */
  public Scanner useLocale (final Locale locale)        // TESTED
  {
    if (locale != null)
      {
        this.actLocale = locale;
        this.actFormat = NumberFormat.getInstance (this.actLocale);
        this.dfs = new DecimalFormatSymbols (this.actLocale);
        this.df = (DecimalFormat) this.actFormat;
      }
    return this;
  }

  /**
   * Sets the current radix to the current value if the given radix is
   * >= 2 and <= 36 otherwise an {@link IllegalArgumentException} is
   * thrown.
   *
   * @param radix
   *            the new radix to use as default.
   * @return <code> this </code> with the new radix value.
   * @throws IllegalArgumentException
   *             When the given radix is out of bounds.
   */
  public Scanner useRadix (final int radix) throws IllegalArgumentException
  {
    if (radix < 2 || radix > 36)
      {
        throw new IllegalArgumentException ();
      }
    this.currentRadix = radix;
    return this;
  }

  /**
   * Checks if it is necessary to apply the current Locale on the
   * String. If so the String is converted using the {@link
   * NumberFormat#parse(String)} into a Number and then back to a
   * default stringrepresentation of that Number.
   *
   * @see #setUseLocale(boolean)
   * @param str
   *            String to convert into another string.
   * @param radix Radix of the Number in the original string. It has
   * to be 10 for anything to happen.
   * @return Eighter the Stringrepresention of the number without the
   * Locale or an unchanged string.
   * @throws ParseException
   *             if {@link NumberFormat#parse(String)} fails to parse.
   */
  private String myApplyLocale (final String str,
                                final int radix) throws ParseException
  {
    String rc;

    if (this.useLocale && radix == 10)
      {
        rc = this.actFormat.parse (str).toString ();
        return rc;
      }

    return str;
  }

  /**
   * If {@link #useLocale} is set and radix is 10 the string is tryed
   * to be converted to string without Locale settings, because the
   * "normal" convert from Local has only double precision and it is
   * not enough for the about 50 digits of precision of the
   * BigDecimal. So in the first step the string is seperated into the
   * integer part which is converted to a long, and the fraction part
   * is appended afterwards. Between the integer and the fraction part
   * comes a ".". Finally the resulting string is returned.
   *
   * @see #setUseLocale(boolean)
   * @param str String representation of a BigDecimal number.
   * @return The default String representation (without Locale) of the
   * BigInteger.
   * @throws ParseException
   *             If the String has more than one decimal seperators a parse exception is thrown.
   */
  private String myApplyLocaleBD (final String str) throws ParseException
  {
    if (!this.useLocale || this.currentRadix != 10)
      {
        return str;
      }

    String negPrefix = this.df.getNegativePrefix ();
    String negSuffix = this.df.getNegativeSuffix ();
    String posPrefix = this.df.getPositivePrefix ();
    String posSuffix = this.df.getPositiveSuffix ();

    char d = this.dfs.getDecimalSeparator ();
    int begin1, begin2;
    boolean isNegativ = false;
    String parts = null;

    String tmpStr1 = "";

    begin1 = str.indexOf (d);
    begin2 = str.indexOf (d, begin1 + 1);

    if (begin2 > 0)
      {
        throw new ParseException ("more than one Decimal seperators", begin2);
      }

    parts = str.substring (0, begin1);

    if ((negPrefix.length () > 0
         && str.substring (0, negPrefix.length ()).equals (negPrefix))
        || (negSuffix.length () > 0
            && str.substring (str.length () -
                              negSuffix.length ()).equals (negSuffix)))
      {
        parts += negSuffix;
        isNegativ = true;
      }
    else
      if ((posPrefix.length () > 0
           && str.substring (0, posPrefix.length ()).equals (posPrefix))
          || (posSuffix.length () > 0
              && str.substring (str.length () -
                                posSuffix.length ()).equals (posSuffix)))
      {
        parts += posSuffix;
      }

    tmpStr1 = this.actFormat.parse (parts).toString ();

    if (isNegativ)
      {
        tmpStr1 +=
          "." + str.substring (str.indexOf (d) + 1,
                               str.length () - negSuffix.length ());
      }
    else
      {
        tmpStr1 +=
          "." + str.substring (str.indexOf (d) + 1,
                               str.length () - posSuffix.length ());
      }

    return tmpStr1;
  }

  /**
   * Tries to interpret the next String as a BigDecimal. Therfore the
   * next String is get with {@link #myCoreNext(boolean, Pattern)} and
   * then {@link #myApplyLocaleBD(String)} is called to convert the
   * String into a BigDecimal.
   *
   * @param delete
   *            Should the found string be deleted or not.
   * @return Returns the BigDecimal value of the next string.
   * @throws InputMismatchException
   *             If the string is not a BigDecimal
   */
  private BigDecimal myBigDecimal (final boolean delete) throws
    InputMismatchException
  {
    BigDecimal rc;
    String tmp = myCoreNext (delete, this.p);
      try
    {
      tmp = myApplyLocaleBD (tmp);
    }
    catch (ParseException e)
    {
      throw new InputMismatchException (ERR_PREFIX + tmp + IS_NOT +
                                        "BigDecimal!!");
    }
    rc = new BigDecimal (tmp);

    return rc;
  }

  /**
   * Applies suffix ("\E") and prefix ("\Q") if str.length != 0 Used
   * by the toString method.
   *
   * @param str
   *            the string on which the suffix and prefix should be applied.
   * @return The new new string with the suffix and prefix.
   */
  private String myConvert (final String str)
  {
    if (str != null && str.length () > 0)
      {
        return "\\Q" + str + "\\E";
      }
    return str;
  }

  /**
   * Searches the current Matcher for the current Pattern. If the end
   * is reached during the search it tried to read again from the
   * source. The search results are always saved in {@link #actResult}
   * which is returned when match() is called. If doSkip is true the
   * pattern is also taken.
   *
   * @param delete
   *            if true the aktPos is set.
   * @param pattern
   *            pattern to search for.
   * @return Returns the String which matches the pattern.
   * @throws NoSuchElementException
   *             If the search has no result.
   */
  private String myCoreNext (final boolean delete, final Pattern pattern)
    throws NoSuchElementException
  {
    if (this.isClosed)
      {
        throw new IllegalStateException ("Scanner closed");
      }
    if (shallUseLastFound (pattern != null ? pattern : this.p))
      {
        if (this.last_RegionEnd != this.myMatcher.regionEnd ())
          {
            System.out.println (this.last_RegionEnd + " != " +
                                this.myMatcher.regionEnd () + " (" +
                                (this.last_RegionEnd -
                                 this.myMatcher.regionEnd ()) + ")");
          }
        if (delete)
          {
            this.actPos = this.lastNextPos;
            this.lastFoundPresent = false;
            this.actResult = this.lastResult;
          }
        return this.lastFound;
      }

    boolean found = false;
    int left;
    int endIndex;

    String tmp2 = null;

    if (this.actPos > this.MAX_PREFIX)
      {
        // skipp the processed chars so that the size of the buffer don't grow to much even with
        // huge files
        this.procesedChars += this.actPos;
        this.actBuffer = this.actBuffer.substring (this.actPos);
        this.actPos = 0;
        this.myMatcher = pattern.matcher (this.actBuffer);
      }

    left = this.actBuffer.length () - this.actPos;
    if (left < this.MIN_BUF_LEN)
      {
        myFillBuffer ();
      }
    found = this.myMatcher.find (this.actPos);

    found = myFillBuffer_loop (this.myMatcher, this.actPos, found);

    this.needInput = false;

    if (found)
      {
        if (this.doSkipp)
          {
            endIndex = this.myMatcher.end ();
          }
        else
          {
            endIndex = this.myMatcher.start ();
          }
        tmp2 = this.actBuffer.substring (this.actPos, endIndex);
        this.lastNextPos = this.myMatcher.end ();
        /*
         * if the delete flag is set, just set the current position after the end of the matched
         * pattern.
         */
        if (delete)
          {
            this.actPos = this.lastNextPos;
          }
        else
          {
            this.lastFound = tmp2;
            this.lastFoundPresent = true;
            this.lastPatternHash = pattern.hashCode ();
          }
        this.last_RegionStart = this.myMatcher.regionStart ();
        this.last_RegionEnd = this.myMatcher.regionEnd ();
        this.last_anchor = this.myMatcher.hasAnchoringBounds ();
        this.last_transparent = this.myMatcher.hasTransparentBounds ();
      }
    else if (this.myMatcher.hitEnd ())
      // the end of input is matched
      {
        tmp2 = this.actBuffer.substring (this.actPos);
        if (tmp2.length() == 0)
          tmp2 = null;
        this.lastNextPos = this.actBuffer.length ();
        if (delete)
          {
            this.actPos = this.lastNextPos;
          }
        else
          {
            this.lastFound = tmp2;
            this.lastFoundPresent = true;
            this.lastPatternHash = pattern.hashCode ();
          }
        this.last_RegionStart = this.myMatcher.regionStart ();
        this.last_RegionEnd = this.myMatcher.regionEnd ();
        this.last_anchor = this.myMatcher.hasAnchoringBounds ();
        this.last_transparent = this.myMatcher.hasTransparentBounds ();
      }
    else
      {
        /*
         * if no match found an Exception is throwed
         */
        throw new NoSuchElementException ();
      }
    /*
     * change the Result only when a nextXXX() method was called, not if a hasNextXXX() method
     * is called
     */
    if (delete)
      {
        this.actResult = this.myMatcher.toMatchResult ();

        this.matchValid = this.actResult != null;
      }
    else
      {
        this.lastResult = this.myMatcher.toMatchResult ();
      }

    this.skipped = this.doSkipp;
    this.doSkipp = false;

    return tmp2;
  }

  /**
   * Used to fill the String buffer from a source. Therfore the 3
   * possible sources are checked if they are not <code>null</code>
   * and this not used, otherwise the read method is called on the
   * source. If a charsetName is set and not <code>null</code> it is
   * applied to convert to String.
   */
  private void myFillBuffer ()
  {
    int len;
    String tmpStr;
    CharBuffer cb = null;
    ByteBuffer bb = null;

    if (this.bIS != null)
      {
        try
        {
          len = this.bIS.read (this.tmpBuffer);
          if (len < 0)
            {
              return;
            }
          if (this.charsetName != null)
            {
              tmpStr = new String (this.tmpBuffer, 0, len, this.charsetName);
            }
          else
            {
              tmpStr = new String (this.tmpBuffer, 0, len);
            }
          this.actBuffer += tmpStr;
        }
        catch (IOException e)
        {
          this.lastIOException = e;
        }
      }
    else if (this.readableSource != null)
      {
        try
        {
          cb = CharBuffer.allocate (1000);
          this.needInput = true;
          len = this.readableSource.read (cb);
          if (len < 0)
            {
              return;
            }
          this.needInput = false;
          tmpStr = new String (cb.array ());
          this.actBuffer += tmpStr;
        }
        catch (IOException e)
        {
          this.lastIOException = e;
        }
      }
    else if (this.rbcSource != null)
      {
        try
        {
          bb = ByteBuffer.allocate (1000);
          this.needInput = true;
          len = this.rbcSource.read (bb);
          this.needInput = false;
          if (len < 0)
            {
              return;
            }
          if (this.charsetName != null)
            {
              tmpStr = new String (bb.array (), 0, len, this.charsetName);
            }
          else
            {
              tmpStr = new String (bb.array (), 0, len);
            }
          this.actBuffer += tmpStr;
        }
        catch (IOException e)
        {
          this.lastIOException = e;
        }
      }

    this.myMatcher.reset (this.actBuffer);
  }

  /**
   * A loop in which the {@link #myFillBuffer()} is called and checked
   * if the pattern is found in the matcher and if the buffersize
   * changes after the read.
   *
   * @param aktM
   *            The current Matcher.
   * @param pos
   *            Position from which the matcher should start matching.
   * @param found
   *            if already found.
   * @return <code> true </code> if the matcher has found a match.
   */
  private boolean myFillBuffer_loop (final Matcher aktM, final int pos,
                                     boolean found)
  {
    int tmp;

    tmp = this.actBuffer.length ();
    while (aktM.hitEnd ()
           && ((this.bIS != null) || (this.readableSource != null)
               || (this.rbcSource != null)))
      {
        myFillBuffer ();
        if (tmp == this.actBuffer.length ())
          {
            break;
          }
        found = aktM.find (pos);
        this.needInput = true;
      }
    return found;
  }

  /**
   * Used to find the given pattern in the given string before the
   * given horizon. Therfore the current matcher is copied, and
   * overwritten using the given pattern and the given Sting. <br>
   * After the search the original values are restored, and skipped is
   * set <code> true </code>.
   *
   * @param pattern
   *            Pattern which should be matched.
   * @param str
   *            The String in which the pattern should be matched.
   * @param horizon
   *            the horizon whithin the match should be, if 0 then it is ignored.
   * @return Returns the String in the given String that matches the pattern.
   */
  private String myFindPInStr (final Pattern pattern, final String str,
                               final int horizon)
  {
    String rc = null;
    int curPos = this.actPos;
    Matcher aktMatcher = this.myMatcher;

    this.myMatcher = pattern.matcher (str);
    if (horizon > 0)
      {
        this.myMatcher.useAnchoringBounds (true);
        this.myMatcher.useTransparentBounds (true);
        this.myMatcher.region (this.actPos, this.actPos + horizon);
      }
    rc = myCoreNext (true, pattern);
    this.myMatcher = aktMatcher;

    this.actPos = curPos;
    this.skipped = true;

    return rc;
  }

  /**
   * Used by the {@link #hasNext(Pattern)} and {@link #next(Pattern)}
   * methods. Therfore a substring is taken first to the current
   * delimiter, afterwards the given pattern is searched in this
   * subsring.<br> Finally the current Buffer and matcher (which have
   * been temporarily changed) are set back.<br> <br> The {@link
   * #skipped} is set <code> true </code>.
   *
   * @param pattern
   *            Pattern to find until the current delimiter.
   * @param delete
   *            Is <code> true </code> if a next method is called.<br>
   *            Is <code> false </code> if a hasNext method is called.
   * @return Returns the String which is returned by the public methods.
   */
  private String myNext (final Pattern pattern, final boolean delete)
  {
    String tmpStr;
    Matcher aktMatcher = this.myMatcher;
    String result;
    String currBuffer = this.actBuffer;
    int currAktPos;

    tmpStr = myCoreNext (delete, this.p);
    this.myMatcher = pattern.matcher (tmpStr);
    this.actBuffer = tmpStr;
    currAktPos = this.actPos;
    this.actPos = 0;
    result = myCoreNext (delete, pattern);
    this.actPos = currAktPos;

    this.actBuffer = currBuffer;
    this.myMatcher = aktMatcher;
    this.skipped = true;

    return result;
  }

  /**
   * Calls the next() method internally to get the next String, and
   * trys to apply a locale which is only applied if the radix is 10
   * and useLocale is <code> true </code>. Afterwards it is tried to
   * call the Constructor of a {@link BigInteger} with the given
   * radix.
   *
   * @param radix The radix to use.
   * @param delete If the found String should be removed from input or
   * not.
   * @param name name of "BigInteger" in case of an Error.
   * @return Returns the new BigInteger created if there is no Error.
   * @throws InputMismatchException
   *             If there is a {@link ParseException} or a {@link NumberFormatException}.
   */
  private BigInteger myNextBigInteger (final int radix, final boolean delete,
                                       final String name)
  {
    BigInteger rc;
    String tmp = myPrepareForNext (this.p, delete);

    try
    {
      tmp = myApplyLocale (tmp, radix);
      rc = new BigInteger (tmp, radix);
      return rc;
    }
    catch (NumberFormatException nfe)
    {
    }
    catch (ParseException e)
    {
    }
    throw new InputMismatchException (ERR_PREFIX + tmp + IS_NOT + name);
  }

  /**
   * Checks if the next String is either "true" or "false", otherwise
   * an {@link InputMismatchException} is thrown. It ignores the case
   * of the string so that "true" and "TRUE" and even "TrUe" are
   * accepted.
   *
   * @param delete Should the found value be removed from the input or
   * not.
   * @return Returns the boolean value (if it is a boolean).
   * @throws InputMismatchException
   *             If the next String is not a boolean.
   */
  private boolean myNextBoolean (final boolean delete) throws
    InputMismatchException
  {
    String tmp = myPrepareForNext (this.p, delete);
    if (tmp.equalsIgnoreCase ("true"))
      {
        return true;
      }
    else if (tmp.equalsIgnoreCase ("false"))
      {
        return false;
      }
    else
      {
        throw new InputMismatchException (ERR_PREFIX + tmp + NOT_BOOLEAN);
      }
  }

  /**
   * Calls the {@link #myPrepareForNext(Pattern, boolean)} which calls
   * the {@link #myCoreNext(boolean, Pattern)} to return the next
   * String matching the current delimier. Afterwards it is tryed to
   * convert the String into a byte. Any Error will lead into a {@link
   * InputMismatchException}.
   *
   * @param radix The radix to use.
   * @param delete Should the found String be removed from the input.
   * @return Returns the byte value of the String.
   * @throws InputMismatchException if the next String is not a byte.
   */
  private byte myNextByte (final int radix,
                           final boolean delete) throws InputMismatchException
  {
    byte rc;
    String tmp = myPrepareForNext (this.p, delete);

      try
    {
      tmp = myApplyLocale (tmp, radix);
      rc = Byte.parseByte (tmp, radix);
      return rc;
    }
    catch (NumberFormatException nfe)
    {
    }
    catch (ParseException e)
    {
    }
    throw new InputMismatchException (ERR_PREFIX + tmp + NOT_BYTE);
  }

  /**
   * Tries to interpret the next String as a double value. To verify
   * if the double value is correct, it is converted back to a String
   * using the default Locale and this String is compared with the
   * String from which the double was converted. If the two Strings
   * don't match, an {@link InputMismatchException} is thrown.<br>
   * <br> The radix used is always 10 even if the global radix is
   * changed.
   *
   * @param delete Should the String be removed, if true it will be
   * also removed if the String is not a double value.
   * @return Returns the double value of the next String.
   * @throws InputMismatchException if the next String is not a
   * double.
   */
  private double myNextDouble (final boolean delete) throws
    InputMismatchException
  {
    double rc;
    String tmp = myPrepareForNext (this.p, delete);

      try
    {
      tmp = myApplyLocale (tmp, 10);
      rc = Double.parseDouble (tmp);
      if (("" + rc).equals (tmp))
        {
          return rc;
        }
    }
    catch (ParseException e)
    {
    }
    throw new InputMismatchException (ERR_PREFIX + tmp + NOT_DOUBLE);
  }

  /**
   * Tries to interpret the next String as an int value. Therfore
   * {@link #myApplyLocale(String, int)} decides if the current Locale
   * should be applied or not and then the result is parsed using
   * {@link Integer#parseInt(String, int)}. Any Error will lead to an
   * {@link InputMismatchException}.
   *
   * @param radix The radix to use.
   * @param delete <code> true </code> if the String should be deleted
   * from the input.
   * @return Returns the int value of the String.
   * @throws InputMismatchException if the next String is not an int.
   */
  private int myNextInt (final int radix,
                         final boolean delete) throws InputMismatchException
  {
    int rc;
    String tmp = myPrepareForNext (this.p, delete);
    try
      {
        tmp = myApplyLocale (tmp, radix);
        rc = Integer.parseInt (tmp, radix);
        return rc;
      }
    catch (NumberFormatException nfe)
    {
    }
    catch (ParseException e)
    {
    }
    throw new InputMismatchException (ERR_PREFIX + tmp + NOT_INT);
  }

  /**
   * Finds the next line using the {@link #NEW_LINE} constant which is
   * set to the system specific line seperator.
   *
   * @param delete should the found line be deleted from the input.
   * @return the current line.
   */
  private String myNextLine (final boolean delete)
  {
    return myPrepareForNext (Pattern.compile (NEW_LINE), delete);
  }

  /**
   * Tries to interpret the next String as a long value with the given
   * radix. Therfore the {@link Long#parseLong(String, int)} is called
   * and every Error will lead into a {@link InputMismatchException}.
   *
   * @param radix The radix to be used.
   * @param delete Should the found String be deleted from the input.
   * @return the long value of the next String.
   * @throws InputMismatchException if the next String is not a long.
   */
  private long myNextLong (final int radix,
                           final boolean delete) throws InputMismatchException
  {
    long rc;
    String tmp = myPrepareForNext (this.p, delete);

    try
      {
        tmp = myApplyLocale (tmp, radix);
        rc = Long.parseLong (tmp, radix);
        return rc;
      }
    catch (NumberFormatException nfe)
      {
      }
    catch (ParseException e)
      {
      }
    throw new InputMismatchException (ERR_PREFIX + tmp + NOT_LONG);
  }

  /**
   * Tries to interpret the next String as a short value with the
   * given radix. Therfore the {@link Short#parseShort(String, int)}
   * is called and every Error will lead into a {@link
   * InputMismatchException} .
   *
   * @param radix
   *            The radix to be used.
   * @param delete
   *            Should the found String be deleted from the input.
   * @return the long value of the next String.
   * @throws InputMismatchException
   *             if the next String is not a short.
   */
  private short myNextShort (final int radix,
                             final boolean delete) throws
    InputMismatchException
  {
    short rc;
    String tmp = myPrepareForNext (this.p, delete);

    try
      {
        tmp = myApplyLocale (tmp, radix);
        rc = Short.parseShort (tmp, radix);
        return rc;
      }
    catch (NumberFormatException nfe)
      {
      }
    catch (ParseException e)
      {
      }
    throw new InputMismatchException (ERR_PREFIX + tmp +
                                      "\" is not a short");
  }

  /**
   * Sets the current pattern to the given pattern and calls the
   * {@link #myCoreNext(boolean, Pattern)}. Finally sets the pattern
   * back to its old value.
   *
   * @param aktPattern Pattern to be used for the next match.
   * @param delete Should the found String be deleted or not.
   * @return Return the String returned from {@link
   * #myCoreNext(boolean, Pattern)}.
   */
  private String myPrepareForNext (final Pattern aktPattern,
                                   final boolean delete)
  {

    String rc;
    Pattern oldPattern = this.p;
    useDelimiter (aktPattern);

    rc = myCoreNext (delete, aktPattern);

    useDelimiter (oldPattern);

    return rc;
  }

  /**
   * Determinates if the last found can be used, so that after a
   * hasNextXXX the nextXXX has not to search if nothing has
   * changed.<br /> Used in {@link #myCoreNext(boolean, Pattern)}.
   *
   * @param aktP The pattern which should be checked.
   * @return <code> true </code> if the searchresult is already ready.
   */
  private boolean shallUseLastFound (final Pattern aktP)
  {
    if (this.lastFoundPresent &&
        this.lastPatternHash == aktP.hashCode () &&
        this.last_RegionStart == this.myMatcher.regionStart () &&
        this.last_anchor == this.myMatcher.hasAnchoringBounds () &&
        this.last_transparent == this.myMatcher.hasTransparentBounds ())
      {
        if (this.last_RegionEnd != this.myMatcher.regionEnd ())
          {
            int tmpVal =
              this.myMatcher.regionEnd () -
              this.last_RegionEnd - this.MAX_PREFIX;
            if (tmpVal > 0 && tmpVal < 20)
              {
                this.last_RegionEnd =
                  this.myMatcher.regionEnd ();
                return true;
              }
          }
        else
          return true;
      }
    return false;
  }

}
