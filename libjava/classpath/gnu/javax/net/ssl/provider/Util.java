/* Util.java -- Miscellaneous utility methods.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.PrintWriter;
import java.io.StringWriter;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;

import java.nio.ByteBuffer;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.Security;

/**
 * A collection of useful class methods.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public final class Util
{

  // Constants.
  // -------------------------------------------------------------------------

  static final String HEX = "0123456789abcdef";

  // Static methods only.
  private Util() { }

  // Class methods.
  // -------------------------------------------------------------------------

  public static Object wrapBuffer(ByteBuffer buffer)
  {
    return wrapBuffer(buffer, "");
  }
  
  public static Object wrapBuffer(ByteBuffer buffer, String prefix)
  {
    return new WrappedBuffer(buffer, prefix);
  }
  
  private static class WrappedBuffer
  {
    private final ByteBuffer buffer;
    private final String prefix;
    
    WrappedBuffer(ByteBuffer buffer, String prefix)
    {
      this.buffer = buffer;
      this.prefix = prefix;
    }
    
    public String toString()
    {
      return hexDump(buffer, prefix);
    }
  }
  
  /**
   * Convert a hexadecimal string into its byte representation.
   *
   * @param hex The hexadecimal string.
   * @return The converted bytes.
   */
  public static byte[] toByteArray(String hex)
  {
    hex = hex.toLowerCase();
    byte[] buf = new byte[hex.length() / 2];
    int j = 0;
    for (int i = 0; i < buf.length; i++)
      {
        buf[i] = (byte) ((Character.digit(hex.charAt(j++), 16) << 4) |
                          Character.digit(hex.charAt(j++), 16));
      }
    return buf;
  }

  /**
   * Convert a byte array to a hexadecimal string, as though it were a
   * big-endian arbitrarily-sized integer.
   *
   * @param buf The bytes to format.
   * @param off The offset to start at.
   * @param len The number of bytes to format.
   * @return A hexadecimal representation of the specified bytes.
   */
  public static String toHexString(byte[] buf, int off, int len)
  {
    StringBuffer str = new StringBuffer();
    for (int i = 0; i < len; i++)
      {
        str.append(HEX.charAt(buf[i+off] >>> 4 & 0x0F));
        str.append(HEX.charAt(buf[i+off] & 0x0F));
      }
    return str.toString();
  }

  /**
   * See {@link #toHexString(byte[],int,int)}.
   */
  public static String toHexString(byte[] buf)
  {
    return Util.toHexString(buf, 0, buf.length);
  }

  /**
   * Convert a byte array to a hexadecimal string, separating octets
   * with the given character.
   *
   * @param buf The bytes to format.
   * @param off The offset to start at.
   * @param len The number of bytes to format.
   * @param sep The character to insert between octets.
   * @return A hexadecimal representation of the specified bytes.
   */
  public static String toHexString(byte[] buf, int off, int len, char sep)
  {
    StringBuffer str = new StringBuffer();
    for (int i = 0; i < len; i++)
      {
        str.append(HEX.charAt(buf[i+off] >>> 4 & 0x0F));
        str.append(HEX.charAt(buf[i+off] & 0x0F));
        if (i < len - 1)
          str.append(sep);
      }
    return str.toString();
  }

  /**
   * See {@link #toHexString(byte[],int,int,char)}.
   */
  public static String toHexString(byte[] buf, char sep)
  {
    return Util.toHexString(buf, 0, buf.length, sep);
  }

  /**
   * Create a representation of the given byte array similar to the
   * output of <code>`hexdump -C'</code>, which is
   *
   * <p><pre>OFFSET  SIXTEEN-BYTES-IN-HEX  PRINTABLE-BYTES</pre>
   *
   * <p>The printable bytes show up as-is if they are printable and
   * not a newline character, otherwise showing as '.'.
   *
   * @param buf The bytes to format.
   * @param off The offset to start at.
   * @param len The number of bytes to encode.
   * @param prefix A string to prepend to every line.
   * @return The formatted string.
   */
  public static String hexDump(byte[] buf, int off, int len, String prefix)
  {
    String nl = getProperty("line.separator");
    StringBuffer str = new StringBuffer();
    int i = 0;
    while (i < len)
      {
        if (prefix != null)
          str.append(prefix);
        str.append(Util.formatInt(i+off, 16, 8));
        str.append("  ");
        String s = Util.toHexString(buf, i+off, Math.min(16, len-i), ' ');
        str.append(s);
        for (int j = s.length(); j < 49; j++)
          str.append(" ");
        for (int j = 0; j < Math.min(16, len - i); j++)
          {
            if ((buf[i+off+j] & 0xFF) < 0x20 || (buf[i+off+j] & 0xFF) > 0x7E)
              str.append('.');
            else
              str.append((char) (buf[i+off+j] & 0xFF));
          }
        str.append(nl);
        i += 16;
      }
    return str.toString();
  }

  public static String hexDump (ByteBuffer buf)
  {
    return hexDump (buf, null);
  }

  public static String hexDump (ByteBuffer buf, String prefix)
  {
    buf = buf.duplicate();
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    int i = 0;
    int len = buf.remaining();
    byte[] line = new byte[16];
    while (i < len)
      {
        if (prefix != null)
          out.print(prefix);
        out.print(Util.formatInt (i, 16, 8));
        out.print("  ");
        int l = Math.min(16, len - i);
        buf.get(line, 0, l);
        String s = Util.toHexString(line, 0, l, ' ');
        out.print(s);
        for (int j = s.length(); j < 49; j++)
          out.print(' ');
        for (int j = 0; j < l; j++)
          {
            int c = line[j] & 0xFF;
            if (c < 0x20 || c > 0x7E)
              out.print('.');
            else
              out.print((char) c);
          }
        out.println();
        i += 16;
      }
    return str.toString();
  }

  /**
   * See {@link #hexDump(byte[],int,int,String)}.
   */
  public static String hexDump(byte[] buf, int off, int len)
  {
    return hexDump(buf, off, len, "");
  }

  /**
   * See {@link #hexDump(byte[],int,int,String)}.
   */
  public static String hexDump(byte[] buf, String prefix)
  {
    return hexDump(buf, 0, buf.length, prefix);
  }

  /**
   * See {@link #hexDump(byte[],int,int,String)}.
   */
  public static String hexDump(byte[] buf)
  {
    return hexDump(buf, 0, buf.length);
  }

  /**
   * Format an integer into the specified radix, zero-filled.
   *
   * @param i The integer to format.
   * @param radix The radix to encode to.
   * @param len The target length of the string. The string is
   *   zero-padded to this length, but may be longer.
   * @return The formatted integer.
   */
  public static String formatInt(int i, int radix, int len)
  {
    String s = Integer.toString(i, radix);
    StringBuffer buf = new StringBuffer();
    for (int j = 0; j < len - s.length(); j++)
      buf.append("0");
    buf.append(s);
    return buf.toString();
  }

  /**
   * Concatenate two byte arrays into one.
   *
   * @param b1 The first byte array.
   * @param b2 The second byte array.
   * @return The concatenation of b1 and b2.
   */
  public static byte[] concat(byte[] b1, byte[] b2)
  {
    byte[] b3 = new byte[b1.length+b2.length];
    System.arraycopy(b1, 0, b3, 0, b1.length);
    System.arraycopy(b2, 0, b3, b1.length, b2.length);
    return b3;
  }

  /**
   * See {@link #trim(byte[],int,int)}.
   */
  public static byte[] trim(byte[] buffer, int len)
  {
    return trim(buffer, 0, len);
  }

  /**
   * Returns a portion of a byte array, possibly zero-filled.
   *
   * @param buffer The byte array to trim.
   * @param off The offset to begin reading at.
   * @param len The number of bytes to return. This value can be larger
   *        than <i>buffer.length - off</i>, in which case the rest of the
   *        returned byte array will be filled with zeros.
   * @throws IndexOutOfBoundsException If <i>off</i> or <i>len</i> is
   *         negative, or if <i>off</i> is larger than the byte array's
   *         length.
   * @return The trimmed byte array.
   */
  public static byte[] trim(byte[] buffer, int off, int len)
  {
    if (off < 0 || len < 0 || off > buffer.length)
      throw new IndexOutOfBoundsException("max=" + buffer.length +
                                          " off=" + off + " len=" + len);
    if (off == 0 && len == buffer.length)
      return buffer;
    byte[] b = new byte[len];
    System.arraycopy(buffer, off, b, 0, Math.min(len, buffer.length - off));
    return b;
  }

  /**
   * Returns the byte array representation of the given big integer with
   * the leading zero byte (if any) trimmed off.
   *
   * @param bi The integer to trim.
   * @return The byte representation of the big integer, with any leading
   *   zero removed.
   */
  public static byte[] trim(BigInteger bi)
  {
    byte[] buf = bi.toByteArray();
    if (buf[0] == 0x00 && !bi.equals(BigInteger.ZERO))
      {
        return trim(buf, 1, buf.length - 1);
      }
    else
      {
        return buf;
      }
  }

  /**
   * Returns the integer value of <code>{@link
   * java.lang.System#currentTimeMillis()} / 1000</code>.
   *
   * @return The current time, in seconds.
   */
  public static int unixTime()
  {
    return (int) (System.currentTimeMillis() / 1000L);
  }

  /**
   * Transform an Object array into another by calling the given method
   * on each object. The returned object array will have the runtime
   * type of <i>returnType</i>. For example, the following will transform
   * array of objects into their String representations, returning a String
   * array. For example:
   *
   * <blockquote><p><code>
   * String[] strings = (String[]) Util.transform(array, String.class,
   * "toString", null);
   * </code></p></blockquote>
   *
   * <p>If any element of the given array is <tt>null</tt>, then that
   * entry in the returned array will also be <tt>null</tt>.
   *
   * @param array The array to transform. It does not need to be of
   *        uniform type.
   * @param returnType The desired return type of the returned array.
   *        This must by the <i>component</i> type, not the array type.
   * @param method The name of the method to invoke from each object.
   * @param args The arguments to pass to the method, or <tt>null</tt>
   *        if the method takes no arguments.
   * @throws InvocationTargetException If an exception occurs while
   *         calling <i>method</i> of any object.
   * @throws NoSuchMethodException If <i>method</i> is not the name of
   *         a valid method of any component of the array.
   * @throws ClassCastException If the returned object from the method
   *         is not assignable to the return type.
   * @throws IllegalArgumentException If <i>args</i> is not appropriate
   *         for <i>method</i>
   * @throws IllegalAccessException If <i>method</i> is not accessible.
   * @throws SecurityException If <i>method</i> is not accessible.
   * @return An array containing the output of <i>method</i> called on
   *         each element of <i>array</i> with <i>args</i>. The return type
   *         of the array will be an array of <i>returnType</i>.
   */
  static Object[] transform(Object[] array, Class returnType,
                            String method, Object[] args)
    throws InvocationTargetException, NoSuchMethodException,
           IllegalAccessException
  {
    if (args == null)
      args = new Object[0];
    Object[] result = (Object[]) Array.newInstance(returnType, array.length);
    Class[] argsClasses = new Class[args.length];
    for (int i = 0; i < args.length; i++)
      {
        argsClasses[i] = args[i].getClass();
      }
    for (int i = 0; i < array.length; i++)
      {
        if (array[i] == null)
          {
            result[i] = null;
            continue;
          }
        Class objClass = array[i].getClass();
        Method objMethod = objClass.getMethod(method, argsClasses);
        Object o = objMethod.invoke(array[i], args);
        if (!returnType.isAssignableFrom(o.getClass()))
          throw new ClassCastException();
        result[i] = o;
      }
    return result;
  }

  /**
   * Get a system property as a privileged action.
   *
   * @param name The name of the property to get.
   * @return The property named <i>name</i>, or null if the property is
   *   not set.
   * @throws SecurityException If the Jessie code still does not have
   *   permission to read the property.
   */
  @Deprecated static String getProperty(final String name)
  {
    return (String) AccessController.doPrivileged(
      new PrivilegedAction()
      {
        public Object run()
        {
          return System.getProperty(name);
        }
      }
    );
  }

  /**
   * Get a security property as a privileged action.
   *
   * @param name The name of the property to get.
   * @return The property named <i>name</i>, or null if the property is
   *   not set.
   * @throws SecurityException If the Jessie code still does not have
   *   permission to read the property.
   */
  @Deprecated static String getSecurityProperty(final String name)
  {
    return (String) AccessController.doPrivileged(
      new PrivilegedAction()
      {
        public Object run()
        {
          return Security.getProperty(name);
        }
      }
    );
  }
}
