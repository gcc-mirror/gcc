/* InflaterInputStream.java - Input stream filter for decompressing
   Copyright (C) 1999, 2000, 2002, 2003 Free Software Foundation, Inc.

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

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class InflaterInputStream extends FilterInputStream
{
  protected void fill () throws IOException
  {
    len = in.read(buf, 0, buf.length);
    if (len != -1)
      inf.setInput(buf, 0, len);
  }

  public InflaterInputStream (InputStream in)
  {
    this (in, new Inflater (), 512);
  }

  public InflaterInputStream (InputStream in, Inflater infl)
  {
    this (in, infl, 512);
  }

  public InflaterInputStream (InputStream in, Inflater inf, int size)
  {
    super (in);

    if (in == null)
      throw new NullPointerException ("in may not be null");

    if (inf == null)
      throw new NullPointerException ("inf may not be null");
    
    if (size < 0)
      throw new IllegalArgumentException ("size may not be negative");
    
    this.inf = inf;
    this.buf = new byte [size];
  }

  public int read () throws IOException
  {
    byte[] buf = new byte[1];
    int r = read (buf, 0, 1);
    if (r != -1)
      r = buf[0] & 0xff;
    return r;
  }

  public int read (byte[] buf, int off, int len) throws IOException
  {
    if (inf == null)
      throw new IOException ("stream closed");
    if (len == 0)
      return 0;
    if (inf.finished())
      return -1;

    int count = 0;
    while (count == 0)
      {
	if (inf.needsInput())
	  fill ();
	try
	  {
	    count = inf.inflate(buf, off, len);	
	    if (count == 0)
	      {
		if (this.len == -1)
		  {
		    // Couldn't get any more data to feed to the Inflater
		    return -1;
		  }
		if (inf.needsDictionary())
		  throw new ZipException ("Inflater needs Dictionary");
	      }
	  }
	catch (DataFormatException dfe)
	  {
	    throw new ZipException (dfe.getMessage());
	  }
      }
    return count;
  }

  public void close () throws IOException
  {
    inf = null;
    super.close ();
  }

  public int available () throws IOException
  {
    // According to the JDK 1.2 docs, this should only ever return 0
    // or 1 and should not be relied upon by Java programs.
    if (inf == null)
      throw new IOException ("stream closed");
    return inf.finished () ? 0 : 1;
  }

  public long skip (long n) throws IOException
  {
    if (inf == null)
      throw new IOException ("stream closed");

    if (n == 0)
      return 0;

    int min = (int) Math.min(n, 1024);
    byte[] buf = new byte[min];

    long s = 0;
    while (n > 0)
      {
	int r = read (buf, 0, min);
	if (r == -1)
	  break;
	n -= r;
	s += r;
	min = (int) Math.min(n, 1024);
      }

    return s;
  }

  // Buffer for delivering uncompressed data to inflater.
  protected byte[] buf;

  // Inflater used to decompress data.
  protected Inflater inf;

  // Number of read bytes in buf.
  protected int len;
}
