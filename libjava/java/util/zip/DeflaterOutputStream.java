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
    do
      {
	int len = def.deflate(buf, 0, buf.length);
	if (len > 0)
	  out.write(buf, 0, len);
       }
    while (! def.needsInput());
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
    if (inbufLength > 0)
      {
	def.setInput (inbuf, 0, inbufLength);
	deflate ();
	inbufLength = 0;
      }
    def.finish();
    while (! def.finished ())
      {
	int len = def.deflate(buf, 0, buf.length);
	if (len > 0)
	  out.write(buf, 0, len);
      }
  }

  public void write (int bval) throws IOException
  {
    if (inbuf == null)
      {
	inbuf = new byte[128];
      }
    else if (inbufLength == inbuf.length)
      {
	def.setInput (inbuf, 0, inbufLength);
	deflate ();
	inbufLength = 0;
      }
    inbuf[inbufLength++] = (byte) bval;
  }

  public void write (byte[] buf, int off, int len) throws IOException
  {
    if (inbufLength > 0)
      {
	def.setInput (inbuf, 0, inbufLength);
	deflate ();
	inbufLength = 0;
      }
    def.setInput (buf, off, len);
    deflate ();
  }

  // Used, if needed, for write(int).
  private byte[] inbuf;
  // Used length of inbuf.
  private int inbufLength;

  // The retrieval buffer.
  protected byte[] buf;

  // Deflater used to compress data.
  protected Deflater def;
}
