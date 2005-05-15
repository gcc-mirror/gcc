/* String.java -- immutable character sequences; the object of string literals
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.lang;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.lang.Comparable;
import java.util.Comparator;
import java.util.Locale;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Strings represent an immutable set of characters.  All String literals
 * are instances of this class, and two string literals with the same contents
 * refer to the same String object.
 *
 * <p>This class also includes a number of methods for manipulating the
 * contents of strings (of course, creating a new object if there are any
 * changes, as String is immutable). Case mapping relies on Unicode 3.0.0
 * standards, where some character sequences have a different number of
 * characters in the uppercase version than the lower case.
 *
 * <p>Strings are special, in that they are the only object with an overloaded
 * operator. When you use '+' with at least one String argument, both
 * arguments have String conversion performed on them, and another String (not
 * guaranteed to be unique) results.
 *
 * <p>String is special-cased when doing data serialization - rather than
 * listing the fields of this class, a String object is converted to a string
 * literal in the object stream.
 *
 * @author Paul N. Fisher
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Per Bothner (bothner@cygnus.com)
 * @since 1.0
 * @status updated to 1.4
 */
public final class String implements Serializable, Comparable, CharSequence
{
  // WARNING: String is a CORE class in the bootstrap cycle. See the comments
  // in vm/reference/java/lang/Runtime for implications of this fact.

  /**
   * This is probably not necessary because this class is special cased already
   * but it will avoid showing up as a discrepancy when comparing SUIDs.
   */
  private static final long serialVersionUID = -6849794470754667710L;

  /**
   * This is the object that holds the characters that make up the
   * String.  It might be a char[], or it could be String.  It could
   * even be `this'.  The actual characters can't be located using
   * pure Java code.
   * @see #boffset
   */
  private Object data;

  /**
   * This is a <emph>byte</emph> offset of the actual characters from
   * the start of the character-holding object.  Don't use this field
   * in Java code.
   */
  private int boffset;

  /**
   * Holds the number of characters in value.  Package visible for use
   * by trusted code.
   */
  int count;

  /**
   * Caches the result of hashCode().  If this value is zero, the hashcode
   * is considered uncached (even if 0 is the correct hash value).
   */
  private int cachedHashCode;

  /**
   * An implementation for {@link CASE_INSENSITIVE_ORDER}.
   * This must be {@link Serializable}. The class name is dictated by
   * compatibility with Sun's JDK.
   */
  private static final class CaseInsensitiveComparator
    implements Comparator, Serializable
  {
    /**
     * Compatible with JDK 1.2.
     */
    private static final long serialVersionUID = 8575799808933029326L;

    /**
     * The default private constructor generates unnecessary overhead.
     */
    CaseInsensitiveComparator() {}

    /**
     * Compares to Strings, using
     * <code>String.compareToIgnoreCase(String)</code>.
     *
     * @param o1 the first string
     * @param o2 the second string
     * @return &lt; 0, 0, or &gt; 0 depending on the case-insensitive
     *         comparison of the two strings.
     * @throws NullPointerException if either argument is null
     * @throws ClassCastException if either argument is not a String
     * @see #compareToIgnoreCase(String)
     */
    public int compare(Object o1, Object o2)
    {
      return ((String) o1).compareToIgnoreCase((String) o2);
    }
  } // class CaseInsensitiveComparator

  /**
   * A Comparator that uses <code>String.compareToIgnoreCase(String)</code>.
   * This comparator is {@link Serializable}. Note that it ignores Locale,
   * for that, you want a Collator.
   *
   * @see Collator#compare(String, String)
   * @since 1.2
   */
  public static final Comparator CASE_INSENSITIVE_ORDER
    = new CaseInsensitiveComparator();

  /**
   * Creates an empty String (length 0). Unless you really need a new object,
   * consider using <code>""</code> instead.
   */
  public String()
  {
    data = "".data;
    boffset = 0;
    count = 0;
  }

  /**
   * Copies the contents of a String to a new String. Since Strings are
   * immutable, only a shallow copy is performed.
   *
   * @param str String to copy
   * @throws NullPointerException if value is null
   */
  public String(String str)
  {
    data = str.data;
    boffset = str.boffset;
    count = str.count;
    cachedHashCode = str.cachedHashCode;
  }

  /**
   * Creates a new String using the character sequence of the char array.
   * Subsequent changes to data do not affect the String.
   *
   * @param data char array to copy
   * @throws NullPointerException if data is null
   */
  public String(char[] data)
  {
    init(data, 0, data.length, false);
  }

  /**
   * Creates a new String using the character sequence of a subarray of
   * characters. The string starts at offset, and copies count chars.
   * Subsequent changes to data do not affect the String.
   *
   * @param data char array to copy
   * @param offset position (base 0) to start copying out of data
   * @param count the number of characters from data to copy
   * @throws NullPointerException if data is null
   * @throws IndexOutOfBoundsException if (offset &lt; 0 || count &lt; 0
   *         || offset + count &gt; data.length)
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public String(char[] data, int offset, int count)
  {
    init(data, offset, count, false);
  }

  /**
   * Creates a new String using an 8-bit array of integer values, starting at
   * an offset, and copying up to the count. Each character c, using
   * corresponding byte b, is created in the new String as if by performing:
   *
   * <pre>
   * c = (char) (((hibyte &amp; 0xff) &lt;&lt; 8) | (b &amp; 0xff))
   * </pre>
   *
   * @param ascii array of integer values
   * @param hibyte top byte of each Unicode character
   * @param offset position (base 0) to start copying out of ascii
   * @param count the number of characters from ascii to copy
   * @throws NullPointerException if ascii is null
   * @throws IndexOutOfBoundsException if (offset &lt; 0 || count &lt; 0
   *         || offset + count &gt; ascii.length)
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   * @see #String(byte[])
   * @see #String(byte[], String)
   * @see #String(byte[], int, int)
   * @see #String(byte[], int, int, String)
   * @deprecated use {@link #String(byte[], int, int, String)} to perform
   *             correct encoding
   */
  public String(byte[] ascii, int hibyte, int offset, int count)
  {
    init(ascii, hibyte, offset, count);
  }

  /**
   * Creates a new String using an 8-bit array of integer values. Each
   * character c, using corresponding byte b, is created in the new String
   * as if by performing:
   *
   * <pre>
   * c = (char) (((hibyte &amp; 0xff) &lt;&lt; 8) | (b &amp; 0xff))
   * </pre>
   *
   * @param ascii array of integer values
   * @param hibyte top byte of each Unicode character
   * @throws NullPointerException if ascii is null
   * @see #String(byte[])
   * @see #String(byte[], String)
   * @see #String(byte[], int, int)
   * @see #String(byte[], int, int, String)
   * @see #String(byte[], int, int, int)
   * @deprecated use {@link #String(byte[], String)} to perform
   *             correct encoding
   */
  public String(byte[] ascii, int hibyte)
  {
    init(ascii, hibyte, 0, ascii.length);
  }

  /**
   * Creates a new String using the portion of the byte array starting at the
   * offset and ending at offset + count. Uses the specified encoding type
   * to decode the byte array, so the resulting string may be longer or
   * shorter than the byte array. For more decoding control, use
   * {@link java.nio.charset.CharsetDecoder}, and for valid character sets,
   * see {@link java.nio.charset.Charset}. The behavior is not specified if
   * the decoder encounters invalid characters; this implementation throws
   * an Error.
   *
   * @param data byte array to copy
   * @param offset the offset to start at
   * @param count the number of characters in the array to use
   * @param encoding the name of the encoding to use
   * @throws NullPointerException if data or encoding is null
   * @throws IndexOutOfBoundsException if offset or count is incorrect
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   * @throws UnsupportedEncodingException if encoding is not found
   * @throws Error if the decoding fails
   * @since 1.1
   */
  public String(byte[] data, int offset, int count, String encoding)
    throws UnsupportedEncodingException
  {
    init (data, offset, count, encoding);
  }

  /**
   * Creates a new String using the byte array. Uses the specified encoding
   * type to decode the byte array, so the resulting string may be longer or
   * shorter than the byte array. For more decoding control, use
   * {@link java.nio.charset.CharsetDecoder}, and for valid character sets,
   * see {@link java.nio.charset.Charset}. The behavior is not specified if
   * the decoder encounters invalid characters; this implementation throws
   * an Error.
   *
   * @param data byte array to copy
   * @param encoding the name of the encoding to use
   * @throws NullPointerException if data or encoding is null
   * @throws UnsupportedEncodingException if encoding is not found
   * @throws Error if the decoding fails
   * @see #String(byte[], int, int, String)
   * @since 1.1
   */
  public String(byte[] data, String encoding)
    throws UnsupportedEncodingException
  {
    this(data, 0, data.length, encoding);
  }

  /**
   * Creates a new String using the portion of the byte array starting at the
   * offset and ending at offset + count. Uses the encoding of the platform's
   * default charset, so the resulting string may be longer or shorter than
   * the byte array. For more decoding control, use
   * {@link java.nio.charset.CharsetDecoder}.  The behavior is not specified
   * if the decoder encounters invalid characters; this implementation throws
   * an Error.
   *
   * @param data byte array to copy
   * @param offset the offset to start at
   * @param count the number of characters in the array to use
   * @throws NullPointerException if data is null
   * @throws IndexOutOfBoundsException if offset or count is incorrect
   * @throws Error if the decoding fails
   * @see #String(byte[], int, int, String)
   * @since 1.1
   */
  public String(byte[] data, int offset, int count)
  {
    try
      {
	init (data, offset, count,
	      System.getProperty("file.encoding", "8859_1"));
      }
    catch (UnsupportedEncodingException x1)
      {
	// Maybe the default encoding is bad.
	try
	  {
	    init (data, offset, count, "8859_1");
	  }
	catch (UnsupportedEncodingException x2)
	  {
	    // We know this can't happen.
	  }
      }
  }

  /**
   * Creates a new String using the byte array. Uses the encoding of the
   * platform's default charset, so the resulting string may be longer or
   * shorter than the byte array. For more decoding control, use
   * {@link java.nio.charset.CharsetDecoder}.  The behavior is not specified
   * if the decoder encounters invalid characters; this implementation throws
   * an Error.
   *
   * @param data byte array to copy
   * @throws NullPointerException if data is null
   * @throws Error if the decoding fails
   * @see #String(byte[], int, int)
   * @see #String(byte[], int, int, String)
   * @since 1.1
   */
  public String(byte[] data)
  {
    this(data, 0, data.length);
  }

  /**
   * Creates a new String using the character sequence represented by
   * the StringBuffer. Subsequent changes to buf do not affect the String.
   *
   * @param buffer StringBuffer to copy
   * @throws NullPointerException if buffer is null
   */
  public String(StringBuffer buffer)
  {
    synchronized (buffer)
      {
	// Share unless buffer is 3/4 empty.
	boolean should_copy = ((buffer.count << 2) < buffer.value.length);
	if (! should_copy)
	  buffer.shared = true;
	init (buffer.value, 0, buffer.count, ! should_copy);
      }
  }

  /**
   * Creates a new String using the character sequence represented by
   * the StringBuilder. Subsequent changes to buf do not affect the String.
   *
   * @param buffer StringBuilder to copy
   * @throws NullPointerException if buffer is null
   */
  public String(StringBuilder buffer)
  {
    this(buffer.value, 0, buffer.count);
  }

  /**
   * Special constructor which can share an array when safe to do so.
   *
   * @param data the characters to copy
   * @param offset the location to start from
   * @param count the number of characters to use
   * @param dont_copy true if the array is trusted, and need not be copied
   * @throws NullPointerException if chars is null
   * @throws StringIndexOutOfBoundsException if bounds check fails
   */
  String(char[] data, int offset, int count, boolean dont_copy)
  {
    init(data, offset, count, dont_copy);
  }

  // This is used by gnu.gcj.runtime.StringBuffer, so it must have
  // package-private protection.  It is accessed via CNI and so avoids
  // ordinary protection mechanisms.
  String(gnu.gcj.runtime.StringBuffer buffer)
  {
    // No need to synchronize or mark the buffer, since we know it is
    // only used once.
    init (buffer);
  }

  /**
   * Returns the number of characters contained in this String.
   *
   * @return the length of this String
   */
  public int length()
  {
    return count;
  }

  /**
   * Returns the character located at the specified index within this String.
   *
   * @param index position of character to return (base 0)
   * @return character located at position index
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= length()
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public native char charAt(int index);

  /**
   * Copies characters from this String starting at a specified start index,
   * ending at a specified stop index, to a character array starting at
   * a specified destination begin index.
   *
   * @param srcBegin index to begin copying characters from this String
   * @param srcEnd index after the last character to be copied from this String
   * @param dst character array which this String is copied into
   * @param dstBegin index to start writing characters into dst
   * @throws NullPointerException if dst is null
   * @throws IndexOutOfBoundsException if any indices are out of bounds
   *         (while unspecified, source problems cause a
   *         StringIndexOutOfBoundsException, and dst problems cause an
   *         ArrayIndexOutOfBoundsException)
   */
  public native void getChars(int srcBegin, int srcEnd,
			      char[] dst, int dstBegin);

  /**
   * Copies the low byte of each character from this String starting at a
   * specified start index, ending at a specified stop index, to a byte array
   * starting at a specified destination begin index.
   *
   * @param srcBegin index to being copying characters from this String
   * @param srcEnd index after the last character to be copied from this String
   * @param dst byte array which each low byte of this String is copied into
   * @param dstBegin index to start writing characters into dst
   * @throws NullPointerException if dst is null and copy length is non-zero
   * @throws IndexOutOfBoundsException if any indices are out of bounds
   *         (while unspecified, source problems cause a
   *         StringIndexOutOfBoundsException, and dst problems cause an
   *         ArrayIndexOutOfBoundsException)
   * @see #getBytes()
   * @see #getBytes(String)
   * @deprecated use {@link #getBytes()}, which uses a char to byte encoder
   */
  public native void getBytes(int srcBegin, int srcEnd,
			      byte[] dst, int dstBegin);

  /**
   * Converts the Unicode characters in this String to a byte array. Uses the
   * specified encoding method, so the result may be longer or shorter than
   * the String. For more encoding control, use
   * {@link java.nio.charset.CharsetEncoder}, and for valid character sets,
   * see {@link java.nio.charset.Charset}. The behavior is not specified if
   * the encoder encounters a problem; this implementation returns null.
   *
   * @param enc encoding name
   * @return the resulting byte array, or null on a problem
   * @throws NullPointerException if enc is null
   * @throws UnsupportedEncodingException if encoding is not supported
   * @since 1.1
   */
  public native byte[] getBytes(String enc)
    throws UnsupportedEncodingException;

  /**
   * Converts the Unicode characters in this String to a byte array. Uses the
   * encoding of the platform's default charset, so the result may be longer
   * or shorter than the String. For more encoding control, use
   * {@link java.nio.charset.CharsetEncoder}.  The behavior is not specified if
   * the encoder encounters a problem; this implementation returns null.
   *
   * @return the resulting byte array, or null on a problem
   * @since 1.1
   */
  public byte[] getBytes()
  {
    try
      {
	return getBytes (System.getProperty("file.encoding", "8859_1"));
      }
    catch (UnsupportedEncodingException x)
      {
	// This probably shouldn't happen, but could if file.encoding
	// is somehow changed to a value we don't understand.
	try
	  {
	    return getBytes ("8859_1");
	  }
	catch (UnsupportedEncodingException x2)
	  {
	    // This really shouldn't happen, because the 8859_1
	    // encoding should always be available.
	    throw new InternalError ("couldn't find 8859_1 encoder");
	  }
      }
  }

  /**
   * Predicate which compares anObject to this. This is true only for Strings
   * with the same character sequence.
   *
   * @param anObject the object to compare
   * @return true if anObject is semantically equal to this
   * @see #compareTo(String)
   * @see #equalsIgnoreCase(String)
   */
  public native boolean equals(Object anObject);

  /**
   * Compares the given StringBuffer to this String. This is true if the
   * StringBuffer has the same content as this String at this moment.
   *
   * @param buffer the StringBuffer to compare to
   * @return true if StringBuffer has the same character sequence
   * @throws NullPointerException if the given StringBuffer is null
   * @since 1.4
   */
  public native boolean contentEquals(StringBuffer buffer);

  /**
   * Compares a String to this String, ignoring case. This does not handle
   * multi-character capitalization exceptions; instead the comparison is
   * made on a character-by-character basis, and is true if:<br><ul>
   * <li><code>c1 == c2</code></li>
   * <li><code>Character.toUpperCase(c1)
   *     == Character.toUpperCase(c2)</code></li>
   * <li><code>Character.toLowerCase(c1)
   *     == Character.toLowerCase(c2)</code></li>
   * </ul>
   *
   * @param anotherString String to compare to this String
   * @return true if anotherString is equal, ignoring case
   * @see #equals(Object)
   * @see Character#toUpperCase(char)
   * @see Character#toLowerCase(char)
   */
  public native boolean equalsIgnoreCase(String anotherString);

  /**
   * Compares this String and another String (case sensitive,
   * lexicographically). The result is less than 0 if this string sorts
   * before the other, 0 if they are equal, and greater than 0 otherwise.
   * After any common starting sequence is skipped, the result is
   * <code>this.charAt(k) - anotherString.charAt(k)</code> if both strings
   * have characters remaining, or
   * <code>this.length() - anotherString.length()</code> if one string is
   * a subsequence of the other.
   *
   * @param anotherString the String to compare against
   * @return the comparison
   * @throws NullPointerException if anotherString is null
   */
  public native int compareTo(String anotherString);

  /**
   * Behaves like <code>compareTo(java.lang.String)</code> unless the Object
   * is not a <code>String</code>.  Then it throws a
   * <code>ClassCastException</code>.
   *
   * @param o the object to compare against
   * @return the comparison
   * @throws NullPointerException if o is null
   * @throws ClassCastException if o is not a <code>String</code>
   * @since 1.2
   */
  public int compareTo(Object o)
  {
    return compareTo((String) o);
  }

  /**
   * Compares this String and another String (case insensitive). This
   * comparison is <em>similar</em> to equalsIgnoreCase, in that it ignores
   * locale and multi-characater capitalization, and compares characters
   * after performing
   * <code>Character.toLowerCase(Character.toUpperCase(c))</code> on each
   * character of the string. This is unsatisfactory for locale-based
   * comparison, in which case you should use {@link java.text.Collator}.
   *
   * @param str the string to compare against
   * @return the comparison
   * @see Collator#compare(String, String)
   * @since 1.2
   */
  public int compareToIgnoreCase(String str)
  {
    return this.toUpperCase().toLowerCase().compareTo(
     str.toUpperCase().toLowerCase());
  }  

  /**
   * Predicate which determines if this String matches another String
   * starting at a specified offset for each String and continuing
   * for a specified length. Indices out of bounds are harmless, and give
   * a false result.
   *
   * @param toffset index to start comparison at for this String
   * @param other String to compare region to this String
   * @param ooffset index to start comparison at for other
   * @param len number of characters to compare
   * @return true if regions match (case sensitive)
   * @throws NullPointerException if other is null
   */
  public native boolean regionMatches(int toffset,
				      String other, int ooffset, int len);

  /**
   * Predicate which determines if this String matches another String
   * starting at a specified offset for each String and continuing
   * for a specified length, optionally ignoring case. Indices out of bounds
   * are harmless, and give a false result. Case comparisons are based on
   * <code>Character.toLowerCase()</code> and
   * <code>Character.toUpperCase()</code>, not on multi-character
   * capitalization expansions.
   *
   * @param ignoreCase true if case should be ignored in comparision
   * @param toffset index to start comparison at for this String
   * @param other String to compare region to this String
   * @param oofset index to start comparison at for other
   * @param len number of characters to compare
   * @return true if regions match, false otherwise
   * @throws NullPointerException if other is null
   */
  public native boolean regionMatches(boolean ignoreCase, int toffset,
				      String other, int ooffset, int len);

  /**
   * Predicate which determines if this String contains the given prefix,
   * beginning comparison at toffset. The result is false if toffset is
   * negative or greater than this.length(), otherwise it is the same as
   * <code>this.substring(toffset).startsWith(prefix)</code>.
   *
   * @param prefix String to compare
   * @param toffset offset for this String where comparison starts
   * @return true if this String starts with prefix
   * @throws NullPointerException if prefix is null
   * @see #regionMatches(boolean, int, String, int, int)
   */
  public native boolean startsWith(String prefix, int toffset);

  /**
   * Predicate which determines if this String starts with a given prefix.
   * If the prefix is an empty String, true is returned.
   *
   * @param prefix String to compare
   * @return true if this String starts with the prefix
   * @throws NullPointerException if prefix is null
   * @see #startsWith(String, int)
   */
  public boolean startsWith(String prefix)
  {
    return startsWith (prefix, 0);
  }

  /**
   * Predicate which determines if this String ends with a given suffix.
   * If the suffix is an empty String, true is returned.
   *
   * @param suffix String to compare
   * @return true if this String ends with the suffix
   * @throws NullPointerException if suffix is null
   * @see #regionMatches(boolean, int, String, int, int)
   */
  public boolean endsWith(String suffix)
  {
    return regionMatches (this.count - suffix.count, suffix, 0, suffix.count);
  }

  /**
   * Computes the hashcode for this String. This is done with int arithmetic,
   * where ** represents exponentiation, by this formula:<br>
   * <code>s[0]*31**(n-1) + s[1]*31**(n-2) + ... + s[n-1]</code>.
   *
   * @return hashcode value of this String
   */
  public native int hashCode();

  /**
   * Finds the first instance of a character in this String.
   *
   * @param ch character to find
   * @return location (base 0) of the character, or -1 if not found
   */
  public int indexOf(int ch)
  {
    return indexOf(ch, 0);
  }

  /**
   * Finds the first instance of a character in this String, starting at
   * a given index.  If starting index is less than 0, the search
   * starts at the beginning of this String.  If the starting index
   * is greater than the length of this String, -1 is returned.
   *
   * @param ch character to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the character, or -1 if not found
   */
  public native int indexOf(int ch, int fromIndex);

  /**
   * Finds the last instance of a character in this String.
   *
   * @param ch character to find
   * @return location (base 0) of the character, or -1 if not found
   */
  public int lastIndexOf(int ch)
  {
    return lastIndexOf(ch, count - 1);
  }

  /**
   * Finds the last instance of a character in this String, starting at
   * a given index.  If starting index is greater than the maximum valid
   * index, then the search begins at the end of this String.  If the
   * starting index is less than zero, -1 is returned.
   *
   * @param ch character to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the character, or -1 if not found
   */
  public native int lastIndexOf(int ch, int fromIndex);

  /**
   * Finds the first instance of a String in this String.
   *
   * @param str String to find
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   */
  public int indexOf(String str)
  {
    return indexOf(str, 0);
  }

  /**
   * Finds the first instance of a String in this String, starting at
   * a given index.  If starting index is less than 0, the search
   * starts at the beginning of this String.  If the starting index
   * is greater than the length of this String, -1 is returned.
   *
   * @param str String to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   */
  public native int indexOf(String str, int fromIndex);

  /**
   * Finds the last instance of a String in this String.
   *
   * @param str String to find
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   */
  public int lastIndexOf(String str)
  {
    return lastIndexOf(str, count - str.count);
  }

  /**
   * Finds the last instance of a String in this String, starting at
   * a given index.  If starting index is greater than the maximum valid
   * index, then the search begins at the end of this String.  If the
   * starting index is less than zero, -1 is returned.
   *
   * @param str String to find
   * @param fromIndex index to start the search
   * @return location (base 0) of the String, or -1 if not found
   * @throws NullPointerException if str is null
   */
  public int lastIndexOf(String str, int fromIndex)
  {
    if (fromIndex >= count)
      fromIndex = count - str.count;
    for (;; --fromIndex)
      {
	if (fromIndex < 0)
	  return -1;
	if (startsWith(str, fromIndex))
	  return fromIndex;
      }
  }

  /**
   * Creates a substring of this String, starting at a specified index
   * and ending at the end of this String.
   *
   * @param begin index to start substring (base 0)
   * @return new String which is a substring of this String
   * @throws IndexOutOfBoundsException if begin &lt; 0 || begin &gt; length()
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   */
  public String substring(int begin)
  {
    return substring(begin, count);
  }

  /**
   * Creates a substring of this String, starting at a specified index
   * and ending at one character before a specified index.
   *
   * @param begin index to start substring (inclusive, base 0)
   * @param end index to end at (exclusive)
   * @return new String which is a substring of this String
   * @throws IndexOutOfBoundsException if begin &lt; 0 || end &gt; length()
   *         || begin &gt; end (while unspecified, this is a
   *         StringIndexOutOfBoundsException)
   */
  public native String substring(int begin, int end);

  /**
   * Creates a substring of this String, starting at a specified index
   * and ending at one character before a specified index. This behaves like
   * <code>substring(begin, end)</code>.
   *
   * @param begin index to start substring (inclusive, base 0)
   * @param end index to end at (exclusive)
   * @return new String which is a substring of this String
   * @throws IndexOutOfBoundsException if begin &lt; 0 || end &gt; length()
   *         || begin &gt; end
   * @since 1.4
   */
  public CharSequence subSequence(int begin, int end)
  {
    return substring(begin, end);
  }

  /**
   * Concatenates a String to this String. This results in a new string unless
   * one of the two originals is "".
   *
   * @param str String to append to this String
   * @return newly concatenated String
   * @throws NullPointerException if str is null
   */
  public native String concat(String str);

  /**
   * Replaces every instance of a character in this String with a new
   * character. If no replacements occur, this is returned.
   *
   * @param oldChar the old character to replace
   * @param newChar the new character
   * @return new String with all instances of oldChar replaced with newChar
   */
  public native String replace(char oldChar, char newChar);

  /**
   * Test if this String matches a regular expression. This is shorthand for
   * <code>{@link Pattern}.matches(regex, this)</code>.
   *
   * @param regex the pattern to match
   * @return true if the pattern matches
   * @throws NullPointerException if regex is null
   * @throws PatternSyntaxException if regex is invalid
   * @see Pattern#matches(String, CharSequence)
   * @since 1.4
   */
  public boolean matches(String regex)
  {
    return Pattern.matches(regex, this);
  }

  /**
   * Replaces the first substring match of the regular expression with a
   * given replacement. This is shorthand for <code>{@link Pattern}
   *   .compile(regex).matcher(this).replaceFirst(replacement)</code>.
   *
   * @param regex the pattern to match
   * @param replacement the replacement string
   * @return the modified string
   * @throws NullPointerException if regex or replacement is null
   * @throws PatternSyntaxException if regex is invalid
   * @see #replaceAll(String, String)
   * @see Pattern#compile(String)
   * @see Pattern#matcher(CharSequence)
   * @see Matcher#replaceFirst(String)
   * @since 1.4
   */
  public String replaceFirst(String regex, String replacement)
  {
    return Pattern.compile(regex).matcher(this).replaceFirst(replacement);
  }

  /**
   * Replaces all matching substrings of the regular expression with a
   * given replacement. This is shorthand for <code>{@link Pattern}
   *   .compile(regex).matcher(this).replaceAll(replacement)</code>.
   *
   * @param regex the pattern to match
   * @param replacement the replacement string
   * @return the modified string
   * @throws NullPointerException if regex or replacement is null
   * @throws PatternSyntaxException if regex is invalid
   * @see #replaceFirst(String, String)
   * @see Pattern#compile(String)
   * @see Pattern#matcher(CharSequence)
   * @see Matcher#replaceAll(String)
   * @since 1.4
   */
  public String replaceAll(String regex, String replacement)
  {
    return Pattern.compile(regex).matcher(this).replaceAll(replacement);
  }

  /**
   * Split this string around the matches of a regular expression. Each
   * element of the returned array is the largest block of characters not
   * terminated by the regular expression, in the order the matches are found.
   *
   * <p>The limit affects the length of the array. If it is positive, the
   * array will contain at most n elements (n - 1 pattern matches). If
   * negative, the array length is unlimited, but there can be trailing empty
   * entries. if 0, the array length is unlimited, and trailing empty entries
   * are discarded.
   *
   * <p>For example, splitting "boo:and:foo" yields:<br>
   * <table border=0>
   * <th><td>Regex</td> <td>Limit</td> <td>Result</td></th>
   * <tr><td>":"</td>   <td>2</td>  <td>{ "boo", "and:foo" }</td></tr>
   * <tr><td>":"</td>   <td>t</td>  <td>{ "boo", "and", "foo" }</td></tr>
   * <tr><td>":"</td>   <td>-2</td> <td>{ "boo", "and", "foo" }</td></tr>
   * <tr><td>"o"</td>   <td>5</td>  <td>{ "b", "", ":and:f", "", "" }</td></tr>
   * <tr><td>"o"</td>   <td>-2</td> <td>{ "b", "", ":and:f", "", "" }</td></tr>
   * <tr><td>"o"</td>   <td>0</td>  <td>{ "b", "", ":and:f" }</td></tr>
   * </table>
   *
   * <p>This is shorthand for
   * <code>{@link Pattern}.compile(regex).split(this, limit)</code>.
   *
   * @param regex the pattern to match
   * @param limit the limit threshold
   * @return the array of split strings
   * @throws NullPointerException if regex or replacement is null
   * @throws PatternSyntaxException if regex is invalid
   * @see Pattern#compile(String)
   * @see Pattern#split(CharSequence, int)
   * @since 1.4
   */
  public String[] split(String regex, int limit)
  {
    return Pattern.compile(regex).split(this, limit);
  }

  /**
   * Split this string around the matches of a regular expression. Each
   * element of the returned array is the largest block of characters not
   * terminated by the regular expression, in the order the matches are found.
   * The array length is unlimited, and trailing empty entries are discarded,
   * as though calling <code>split(regex, 0)</code>.
   *
   * @param regex the pattern to match
   * @return the array of split strings
   * @throws NullPointerException if regex or replacement is null
   * @throws PatternSyntaxException if regex is invalid
   * @see #split(String, int)
   * @see Pattern#compile(String)
   * @see Pattern#split(CharSequence, int)
   * @since 1.4
   */
  public String[] split(String regex)
  {
    return Pattern.compile(regex).split(this, 0);
  }

  /**
   * Lowercases this String according to a particular locale. This uses
   * Unicode's special case mappings, as applied to the given Locale, so the
   * resulting string may be a different length.
   *
   * @param loc locale to use
   * @return new lowercased String, or this if no characters were lowercased
   * @throws NullPointerException if loc is null
   * @see #toUpperCase(Locale)
   * @since 1.1
   */
  public native String toLowerCase(Locale locale);

  /**
   * Lowercases this String. This uses Unicode's special case mappings, as
   * applied to the platform's default Locale, so the resulting string may
   * be a different length.
   *
   * @return new lowercased String, or this if no characters were lowercased
   * @see #toLowerCase(Locale)
   * @see #toUpperCase()
   */
  public String toLowerCase()
  {
    // The JDK is a bit confused about what to do here.  If we pass in
    // the default Locale then special Locale handling might be
    // invoked.  However, the docs also say that Character.toLowerCase
    // rules here.  We go with the latter.
    return toLowerCase (null);
  }

  /**
   * Uppercases this String according to a particular locale. This uses
   * Unicode's special case mappings, as applied to the given Locale, so the
   * resulting string may be a different length.
   *
   * @param loc locale to use
   * @return new uppercased String, or this if no characters were uppercased
   * @throws NullPointerException if loc is null
   * @see #toLowerCase(Locale)
   * @since 1.1
   */
  public native String toUpperCase(Locale locale);

  /**
   * Uppercases this String. This uses Unicode's special case mappings, as
   * applied to the platform's default Locale, so the resulting string may
   * be a different length.
   *
   * @return new uppercased String, or this if no characters were uppercased
   * @see #toUpperCase(Locale)
   * @see #toLowerCase()
   */
  public String toUpperCase()
  {
    // The JDK is a bit confused about what to do here.  If we pass in
    // the default Locale then special Locale handling might be
    // invoked.  However, the docs also say that Character.toLowerCase
    // rules here.  We go with the latter.
    return toUpperCase (null);
  }

  /**
   * Trims all characters less than or equal to <code>'\u0020'</code>
   * (<code>' '</code>) from the beginning and end of this String. This
   * includes many, but not all, ASCII control characters, and all
   * {@link Character#whitespace(char)}.
   *
   * @return new trimmed String, or this if nothing trimmed
   */
  public native String trim();

  /**
   * Returns this, as it is already a String!
   *
   * @return this
   */
  public String toString()
  {
    return this;
  }

  /**
   * Copies the contents of this String into a character array. Subsequent
   * changes to the array do not affect the String.
   *
   * @return character array copying the String
   */
  public native char[] toCharArray();

  /**
   * Returns a String representation of an Object. This is "null" if the
   * object is null, otherwise it is <code>obj.toString()</code> (which
   * can be null).
   *
   * @param obj the Object
   * @return the string conversion of obj
   */
  public static String valueOf(Object obj)
  {
    return obj == null ? "null" : obj.toString();
  }

  /**
   * Returns a String representation of a character array. Subsequent
   * changes to the array do not affect the String.
   *
   * @param data the character array
   * @return a String containing the same character sequence as data
   * @throws NullPointerException if data is null
   * @see #valueOf(char[], int, int)
   * @see #String(char[])
   */
  public static String valueOf(char[] data)
  {
    return valueOf (data, 0, data.length);
  }

  /**
   * Returns a String representing the character sequence of the char array,
   * starting at the specified offset, and copying chars up to the specified
   * count. Subsequent changes to the array do not affect the String.
   *
   * @param data character array
   * @param offset position (base 0) to start copying out of data
   * @param count the number of characters from data to copy
   * @return String containing the chars from data[offset..offset+count]
   * @throws NullPointerException if data is null
   * @throws IndexOutOfBoundsException if (offset &lt; 0 || count &lt; 0
   *         || offset + count &gt; data.length)
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   * @see #String(char[], int, int)
   */
  public static native String valueOf(char[] data, int offset, int count);

  /**
   * Returns a String representing the character sequence of the char array,
   * starting at the specified offset, and copying chars up to the specified
   * count. Subsequent changes to the array do not affect the String.
   *
   * @param data character array
   * @param offset position (base 0) to start copying out of data
   * @param count the number of characters from data to copy
   * @return String containing the chars from data[offset..offset+count]
   * @throws NullPointerException if data is null
   * @throws IndexOutOfBoundsException if (offset &lt; 0 || count &lt; 0
   *         || offset + count &gt; data.length)
   *         (while unspecified, this is a StringIndexOutOfBoundsException)
   * @see #String(char[], int, int)
   */
  public static String copyValueOf(char[] data, int offset, int count)
  {
    String r = new String ();
    r.init(data, offset, count, false);
    return r;
  }

  /**
   * Returns a String representation of a character array. Subsequent
   * changes to the array do not affect the String.
   *
   * @param data the character array
   * @return a String containing the same character sequence as data
   * @throws NullPointerException if data is null
   * @see #copyValueOf(char[], int, int)
   * @see #String(char[])
   */
  public static String copyValueOf(char[] data)
  {
    return copyValueOf (data, 0, data.length);
  }

  /**
   * Returns a String representing a boolean.
   *
   * @param b the boolean
   * @return "true" if b is true, else "false"
   */
  public static String valueOf(boolean b)
  {
    return b ? "true" : "false";
  }

  /**
   * Returns a String representing a character.
   *
   * @param c the character
   * @return String containing the single character c
   */
  public static native String valueOf(char c);

  /**
   * Returns a String representing an integer.
   *
   * @param i the integer
   * @return String containing the integer in base 10
   * @see Integer#toString(int)
   */
  public static native String valueOf(int i);

  /**
   * Returns a String representing a long.
   *
   * @param l the long
   * @return String containing the long in base 10
   * @see Long#toString(long)
   */
  public static String valueOf(long l)
  {
    return Long.toString(l);
  }

  /**
   * Returns a String representing a float.
   *
   * @param f the float
   * @return String containing the float
   * @see Float#toString(float)
   */
  public static String valueOf(float f)
  {
    return Float.toString(f);
  }

  /**
   * Returns a String representing a double.
   *
   * @param d the double
   * @return String containing the double
   * @see Double#toString(double)
   */
  public static String valueOf(double d)
  {
    return Double.toString(d);
  }

  /**
   * Fetches this String from the intern hashtable. If two Strings are
   * considered equal, by the equals() method, then intern() will return the
   * same String instance. ie. if (s1.equals(s2)) then
   * (s1.intern() == s2.intern()). All string literals and string-valued
   * constant expressions are already interned.
   *
   * @return the interned String
   */
  public native String intern();


  private native void init(char[] chars, int offset, int count,
			   boolean dont_copy);
  private native void init(byte[] chars, int hibyte, int offset, int count);
  private native void init(byte[] chars, int offset, int count, String enc)
    throws UnsupportedEncodingException;
  private native void init(gnu.gcj.runtime.StringBuffer buffer);
}
