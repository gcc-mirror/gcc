/* BERReader.java -- basic encoding rules (BER) reader.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package gnu.java.security.ber;

import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

public class BERReader extends DERReader implements BER
{

  /**
   * Create a new DER reader from a byte array.
   *
   * @param in The encoded bytes.
   */
  public BERReader(byte[] in)
  {
    super(in);
  }

  public BERReader (byte[] in, int off, int len)
  {
    super(in, off, len);
  }

  /**
   * Create a new DER readed from an input stream.
   *
   * @param in The encoded bytes.
   */
  public BERReader(InputStream in)
  {
    super(in);
  }

  public DERValue read() throws IOException
  {
    in.mark(2);
    int tag = in.read();
    if (tag == -1)
      throw new EOFException();
    int length = in.read();
    if (length == 0)
      {
        if (tag == 0)
          return END_OF_SEQUENCE;
        return new BERValue(tag, CONSTRUCTED_VALUE, new byte[] { (byte) tag, 0 });
      }
    else
      {
        in.reset();
        return super.read();
      }
  }

  public int peek() throws IOException
  {
    in.mark(1);
    int ret = in.read();
    in.reset();
    return ret;
  }
}
