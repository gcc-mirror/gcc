/* BinaryRefAddr.java -- RefAddr that uses a byte array as content.
   Copyright (C) 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package javax.naming;

import java.util.Arrays;

/**
 * RefAddr that uses a byte array as content.
 * This can be used to reference objects that can only be represented as
 * byte arrays.
 *
 * @see Reference
 * @since 1.3
 * @author Mark Wielaard (mark@klomp.org)
 */
public class BinaryRefAddr extends RefAddr
{

  /**
   * The possibly null content of this RefAddr.
   * Set by the constructor and returned by getContent.
   */
  private final byte[] buf;

  /**
   * Contructs a new BinaryRefAddr with the given type and content.
   * The complete content of the byte array is copied to a new array.
   */
  public BinaryRefAddr (String addrType, byte[] buf)
  {
    this(addrType, buf, 0, buf.length);
  }

  /**
   * Contructs a new BinaryRefAddr with the given type and the content
   * taken from the given byte array.
   * The content of the byte array is copied to a new array.
   */
  public BinaryRefAddr (String addrType, byte[] buf, int off, int length)
  {
    super(addrType);
    this.buf = new byte[length];
    System.arraycopy(buf, off, this.buf, 0, length);
  }

  /**
   * Returns the byte array contents as given to the constructor.
   * The returned byte array is shared with this object and other callers.
   * Changing the content of the buffer is discouraged and should only be
   * done when the byte array is locked.
   */
  public Object getContent ()
  {
    return buf;
  }

  /**
   * Checks if the object is a BinaryRefAddr with the same type and with the
   * same bytes in the content.
   *
   * @return true if the given object is an instance of BinaryRefAddr,
   *         the addrType is the same as this addrType and the bytes of the
   *         content are the same.
   */
  public boolean equals (Object o)
  {
    if (o instanceof BinaryRefAddr)
      {
        BinaryRefAddr refAddr = (BinaryRefAddr) o;
        if (this.getType().equals(refAddr.getType()))
	  {
	    byte[] c1 = (byte[]) this.getContent();
	    byte[] c2 = (byte[]) refAddr.getContent();
	    return Arrays.equals(c1, c2);
	  }
      }
    return false;
  }

  /**
   * Returns the hashCode which is the hasCode of the String returned by
   * <code>getType()</code> plus the hashCode of the byte array returned by
   * <code>getContent</code>. The hashCode of the byte array is calculated
   * by taking the xor of all the bytes in the array, or zero when there are
   * no bytes in the array.
   */
  public int hashCode()
  {
    int result = 0;
    byte[] b = (byte[]) getContent();
    for (int i=0; i < b.length; i++)
      result = result^b[i];

    return getType().hashCode() + result;
  }

  private static char[] hex = {'0', '1', '2', '3', '4', '5', '6', '7',
			       '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
  /**
   * Returns a String representation of the RefAddr. Only the first 32 bytes
   * of the content are added as hex encoded characters.
   * Should only be used for debugging purposes.
   */
  public String toString()
  {
    StringBuffer sb = new StringBuffer("[RefAddr type: ");
    sb.append(getType());
    sb.append(" content: 0x");
    byte[] b = (byte[]) getContent();
    for (int i=0; i < b.length && i < 32; i++)
      {
	sb.append(hex[(b[i]&0xf0)>>4]);
	sb.append(hex[b[i]&0x0f]);
      }
    if (b.length > 32)
      sb.append("...");
    sb.append("]");
    return sb.toString();
  }
}
