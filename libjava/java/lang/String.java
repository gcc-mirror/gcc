/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.UnsupportedEncodingException;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date September 4, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Complete to 1.1, but see FIXMEs. Also see testsuite results.
 */

public final class String
{
  private Object data;
  private int boffset; // Note this is a byte offset - don't use in Java code!
  private int count;

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
    // because String is final, we actually get this far on a null reference
    if (this == null)
      throw new NullPointerException();
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

  public String substring (int beginIndex)
  {
    return substring (beginIndex, count);
  }

  public native String substring (int beginIndex, int endIndex);

  public native String concat (String str);

  public native String replace (char oldChar, char newChar);

  public native String toLowerCase ();

  public native String toUpperCase ();

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

  public static String valueOf (int i)
  {
    return Integer.toString(i);
  }

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
  private native void unintern ();
  private static native void rehash ();
}
