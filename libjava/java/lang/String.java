/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.UnsupportedEncodingException;
import java.io.Serializable;
import java.lang.Comparable;
import java.util.Comparator;
import java.util.Locale;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date September 4, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Complete to 1.3.
 */

public final class String implements Serializable, Comparable, CharSequence
{
  private Object data;
  private int boffset; // Note this is a byte offset - don't use in Java code!
  private int count;

  // This is probably not necessary because this class is special cased already
  // but it will avoid showing up as a discrepancy when comparing SUIDs.
  private static final long serialVersionUID = -6849794470754667710L;

  /**
   * An implementation for {@link CASE_INSENSITIVE_ORDER}.
   * This must be {@link Serializable}.
   */
  private static final class CaseInsensitiveComparator
    implements Comparator, Serializable
  {
    /**
     * The default private constructor generates unnecessary overhead
     */
    CaseInsensitiveComparator() {}

    /**
     * Compares two Strings, using
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
  }

  /**
   * A Comparator that uses <code>String.compareToIgnoreCase(String)</code>.
   * This comparator is {@link Serializable}.
   *
   * @since 1.2
   */
  public static final Comparator CASE_INSENSITIVE_ORDER
    = new CaseInsensitiveComparator();

  public String ()
  {
    init();
  }

  public String (String value)
  {
    data = value.data;
    boffset = value.boffset;
    count = value.count;
  }

  public String (StringBuffer buffer)
  {
    synchronized (buffer)
      {
	buffer.shared = true;
	init (buffer.value, 0, buffer.count, true);
      }
  }

  // This is used by gnu.gcj.runtime.StringBuffer, so it must have
  // package-private protection.  It is accessed via CNI and so avoids
  // ordinary protection mechanisms.
  String (gnu.gcj.runtime.StringBuffer buffer)
  {
    // No need to synchronize or mark the buffer, since we know it is
    // only used once.
    init (buffer.value, 0, buffer.count, true);
  }

  public String (char[] data)
  {
    init(data, 0, data.length, false);
  }

  public String (char[] data, int offset, int count)
  {
    init(data, offset, count, false);
  }

  public String (byte[] byteArray)
  {
    this (byteArray, 0, byteArray.length);
  }

  public String (byte[] byteArray, int offset, int count)
  {
    try
      {
	init (byteArray, offset, count,
	      System.getProperty("file.encoding", "8859_1"));
      }
    catch (UnsupportedEncodingException x1)
      {
	// Maybe the default encoding is bad.
	try
	  {
	    init (byteArray, offset, count, "8859_1");
	  }
	catch (UnsupportedEncodingException x2)
	  {
	    // We know this can't happen.
	  }
      }
  }

  public String (byte[] byteArray, String enc)
    throws UnsupportedEncodingException
  {
    this (byteArray, 0, byteArray.length, enc);
  }

  public String (byte[] byteArray, int offset, int count, String enc)
    throws UnsupportedEncodingException
  {
    init (byteArray, offset, count, enc);
  }

  public static String copyValueOf(char[] data)
  {
    return copyValueOf (data, 0, data.length);
  }

  public static String copyValueOf(char[] data, int offset, int count)
  {
    String r = new String ();
    r.init(data, offset, count, false);
    return r;
  }

  /** @deprecated */
  public String (byte[] ascii, int hibyte)
  {
    init(ascii, hibyte, 0, ascii.length);
  }

  /** @deprecated */
  public String (byte[] ascii, int hibyte, int offset, int count)
  {
    init(ascii, hibyte, offset, count);
  }

  public String toString ()
  {
    return this;
  }

  public native boolean equals (Object anObject);

  public native int hashCode ();

  public int length ()
  {
    return count;
  }

  public native char charAt (int index);

  public native void getChars (int srcBegin, int srcEnd,
			       char[] dst, int dstBegin);

  public byte[] getBytes ()
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

  public native byte[] getBytes (String enc)
    throws UnsupportedEncodingException;

  /** @deprecated */
  public native void getBytes (int srcBegin, int srcEnd,
				byte[] dst, int dstBegin);

  public native char[] toCharArray ();

  public native boolean equalsIgnoreCase (String anotherString);

  public native int compareTo (String anotherString);

  public int compareTo (Object obj)
  {
    return compareTo ((String)obj);
  }
  
  public int compareToIgnoreCase (String str)
  {
    return this.toUpperCase().toLowerCase().compareTo(
     str.toUpperCase().toLowerCase());
  }  

  public native boolean regionMatches (int toffset,
				       String other, int ooffset, int len);

  public native boolean regionMatches (boolean ignoreCase, int toffset,
				       String other, int ooffset, int len);

  public boolean startsWith (String prefix)
  {
    return startsWith (prefix, 0);
  }

  public native boolean startsWith (String prefix, int toffset);

  public boolean endsWith (String suffix)
  {
    return regionMatches (this.count - suffix.count, suffix, 0, suffix.count);
  }

  // No such method specified in the doc, including JDK 1.2.
  // public boolean endsWith (String suffix, int toffset)
  // {
  //   return regionMatches (toffset, suffix, 0, suffix.count);
  // }

  // The Language Specification, and the JDK 1.2 API docs say that
  // index and lastIndex take an int, while the Class Libraries
  // say they take a char.  The former wins ...

  public int indexOf (int ch)
  {
    return indexOf (ch, 0);
  }

  public native int indexOf (int ch, int fromIndex);

  public int indexOf (String str)
  {
    return indexOf (str, 0);
  }

  public native int indexOf (String str, int fromIndex);

  public int lastIndexOf (int ch)
  {
    return lastIndexOf (ch, count - 1);
  }

  public native int lastIndexOf (int ch, int fromIndex);

  public int lastIndexOf (String str)
  {
    return lastIndexOf (str, count - str.count);
  }

  public int lastIndexOf (String str, int fromIndex)
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
   * and ending at one character before a specified index.
   * <p>
   * To implement <code>CharSequence</code>.
   * Calls <code>substring(beginIndex, endIndex)</code>.
   *
   * @param beginIndex index to start substring (base 0)
   * @param endIndex index after the last character to be 
   *   copied into the substring
   * 
   * @return new String which is a substring of this String
   *
   * @exception StringIndexOutOfBoundsException 
   *   if (beginIndex < 0 || endIndex > this.length() || beginIndex > endIndex)
   */
  public CharSequence subSequence(int beginIndex, int endIndex)
    throws IndexOutOfBoundsException
  {
    return substring(beginIndex, endIndex);
  }

  public String substring (int beginIndex)
  {
    return substring (beginIndex, count);
  }

  public native String substring (int beginIndex, int endIndex);

  public native String concat (String str);

  public native String replace (char oldChar, char newChar);

  public native String toLowerCase (Locale locale);
  public native String toUpperCase (Locale locale);

  public String toLowerCase ()
  {
    // The JDK is a bit confused about what to do here.  If we pass in
    // the default Locale then special Locale handling might be
    // invoked.  However, the docs also say that Character.toLowerCase
    // rules here.  We go with the latter.
    return toLowerCase (null);
  }

  public String toUpperCase ()
  {
    // The JDK is a bit confused about what to do here.  If we pass in
    // the default Locale then special Locale handling might be
    // invoked.  However, the docs also say that Character.toLowerCase
    // rules here.  We go with the latter.
    return toUpperCase (null);
  }

  public native String trim ();

  public static String valueOf (Object obj)
  {
    return obj == null ? "null" : obj.toString();
  }

  public static String valueOf (char[] data)
  {
    return valueOf (data, 0, data.length);
  }

  public static native String valueOf (char[] data, int offset, int count);

  public static String valueOf (boolean b)
  {
    return b ? "true" : "false";
  }

  public static native String valueOf (char c);

  public static native String valueOf (int i);

  public static String valueOf (long l)
  {
    return Long.toString(l);
  }

  public static String valueOf (float f)
  {
    return Float.toString(f);
  }

  public static String valueOf (double d)
  {
    return Double.toString(d);
  }

  public native String intern ();

  private native void init ();
  private native void init (char[] chars, int offset, int count,
			    boolean dont_copy);
  private native void init (byte[] chars, int hibyte, int offset, int count);
  private native void init (byte[] chars, int offset, int count, String enc)
    throws UnsupportedEncodingException;
  private static native void rehash ();
}
