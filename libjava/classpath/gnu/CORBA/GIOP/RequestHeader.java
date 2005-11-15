/* RequestHeader.java -- The GIOP 1.0 request message.
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


package gnu.CORBA.GIOP;

import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.CDR.AbstractCdrOutput;

import org.omg.CORBA.portable.IDLEntity;

/**
 * The GIOP request message.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class RequestHeader
  extends ContextHandler
  implements IDLEntity
{
  /**
   * The currently free request id. This field is incremented each time the new
   * request header is constructed. To facilitate error detection, the first
   * free id is equal to 0x01234567 (19088743).
   */
  private static int freeId = 0x01234567;

  /**
   * The operation being invoked (IDL scope name).
   */
  public String operation;

  /**
   * Identifies the object that is the target of the invocation.
   */
  public byte[] object_key;

  /**
   * A value identifying the requesting principal. Initialised into a single
   * zero byte.
   *
   * @deprecated by CORBA 2.2.
   */
  public byte[] requesting_principal;

  /**
   * This is used to associate the reply message with the previous request
   * message. Initialised each time by the different value, increasing form 1 to
   * Integer.MAX_VALUE.
   */
  public int request_id = getNextId();

  /**
   * If true, the response from the server is expected.
   */
  protected boolean response_expected = true;

  /**
   * Get next free request id. The value of the free request id starts from
   * 0x02345678, it is incremented each time this function is called and is
   * reset to 1 after reaching Integer.MAX_VALUE.
   *
   * @return the next free request id.
   */
  public static synchronized int getNextId()
  {
    int f = freeId;
    if (freeId == Integer.MAX_VALUE)
      freeId = 1;
    else
      freeId++;

    return f;
  }

  /**
   * Set if the sender expects any response to this message.
   */
  public abstract void setResponseExpected(boolean expected);

  /**
   * Return true if response is expected.
   */
  public abstract boolean isResponseExpected();

  /**
   * Converts an byte array into hexadecimal string values. Used in various
   * toString() methods.
   */
  public String bytes(byte[] array)
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < array.length; i++)
      {
        b.append(Integer.toHexString(array[i] & 0xFF));
        b.append(" ");
      }
    return b.toString();
  }

  /**
   * Reads the header from the stream.
   *
   * @param in a stream to read from.
   */
  public abstract void read(AbstractCdrInput in);

  /**
   * Return a string representation.
   */
  public abstract String toString();

  /**
   * Writes the header to the stream.
   *
   * @param out a stream to write into.
   */
  public abstract void write(AbstractCdrOutput out);

}
