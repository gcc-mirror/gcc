/* DeflaterOutputStream.java - Output filter for compressing.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

package java.util.zip;

import java.io.FilterOutputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class DeflaterOutputStream extends FilterOutputStream
{
  public void close () throws IOException
  {
    finish ();
    out.close();
  }

  protected void deflate () throws IOException
  {
    while (true)
      {
	int len = def.deflate(buf, 0, buf.length);
	if (len == 0 || len == -1)
	  break;
	out.write(buf, 0, len);
      }
  }

  public DeflaterOutputStream (OutputStream out)
  {
    this (out, new Deflater (), 512);
  }

  public DeflaterOutputStream (OutputStream out, Deflater defl)
  {
    this (out, defl, 512);
  }

  public DeflaterOutputStream(OutputStream out, Deflater defl, int bufsize)
  {
    super (out);
    buf = new byte[bufsize];
    def = defl;
  }

  public void finish () throws IOException
  {
    def.finish();
    deflate ();
  }

  public void write (int bval) throws IOException
  {
    byte[] b = new byte[1];
    b[0] = (byte) bval;
    write (b, 0, 1);
  }

  public void write (byte[] buf, int off, int len) throws IOException
  {
    def.setInput (buf, off, len);
    deflate ();
  }

  // The retrieval buffer.
  protected byte[] buf;

  // Deflater used to compress data.
  protected Deflater def;
}
