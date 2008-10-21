/* ChunkedInputStream.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package gnu.java.net.protocol.http;

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;
import java.io.InputStream;
import java.net.ProtocolException;


//
// Note that we rely on the implemtation of skip() in the super class
// (InputStream) calling our read methods to account for chunk headers
// while skipping.
//


/**
 * Input stream wrapper for the "chunked" transfer-coding.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class ChunkedInputStream
  extends InputStream
{
  Headers headers;

  /** The underlying stream. */
  private InputStream in;

  /** Size of the chunk we're reading.  */
  int size;
  /** Number of bytes we've read in this chunk.  */
  int count;
  /**
   * True when we should read meta-information, false when we should
   * read data.
   */
  boolean meta;
  /** True when we've hit EOF.  */
  boolean eof;

  /**
   * Constructor.
   * @param in the response socket input stream
   * @param headers the headers to receive additional header lines
   */
  public ChunkedInputStream(InputStream in, Headers headers)
  {
    this.in = in;
    this.headers = headers;
    size = -1;
    count = 0;
    meta = true;
  }

  public int read()
    throws IOException
  {
    byte[] buf = new byte[1];
    int len = read(buf, 0, 1);
    if (len == -1)
      {
        return -1;
      }
    return 0xff & buf[0];
  }

  public synchronized int read(byte[] buffer, int offset, int length)
    throws IOException
  {
    if (eof)
      {
        return -1;
      }
    if (meta)
      {
        // Read chunk header
        int c, last = 0;
        boolean seenSemi = false;
        CPStringBuilder buf = new CPStringBuilder();
        do
          {
            c = in.read();
            if (c == 0x3b) // ;
              {
                seenSemi = true;
              }
            else if (c == 0x0a && last == 0x0d) // CRLF
              {
                try
                  {
                    size = Integer.parseInt(buf.toString(), 16);
                  }
                catch (NumberFormatException nfe)
                  {
                    IOException ioe = new IOException("Bad chunk header");
                    ioe.initCause(nfe);
                    // Unrecoverable.  Don't try to read more.
                    in.close();
                    throw ioe;
                  }
                break;
              }
            else if (!seenSemi && c >= 0x30)
              {
                buf.append ((char) c);
              }
            last = c;
          }
        while(c != -1);
        count = 0;
        meta = false;
      }
    if (size == 0)
      {
        // Read trailer
        headers.parse(in);
        eof = true;
        return -1;
      }
    else
      {
	int canRead = Math.min(size - count, length);
	int len = in.read(buffer, offset, canRead);
	if (len == -1)
	  {
	    // This is an error condition but it isn't clear what we
	    // should do with it.
	    eof = true;
	    return -1;
	  }
        count += len;
        if (count == size)
          {
            // Read CRLF
            int c1 = in.read();
            int c2 = in.read();
            if (c1 == -1 || c2 == -1)
              {
                // EOF before CRLF: bad, but ignore
                eof = true;
                return -1;
              }
            if (c1 != 0x0d || c2 != 0x0a)
              {
                throw new ProtocolException("expecting CRLF: " + c1 + "," + c2);
              }
            meta = true;
          }
        return len;
      }
  }

  /**
   * This method returns the number of bytes that can be read from
   * this stream before a read might block.  Even if the underlying
   * InputStream has data available past the end of the current chunk,
   * we have no way of knowing how large the next chunk header will
   * be. So we cannot report available data past the current chunk.
   *
   * @return The number of bytes that can be read before a read might
   * block
   *
   * @exception IOException If an error occurs
   */
  public int available() throws IOException
  {
    if (meta)
      return 0;
    
    return Math.min(in.available(), size - count);
  }

  /**
   * This method closes the ChunkedInputStream by closing the underlying
   * InputStream.
   * 
   * @exception IOException If an error occurs
   */
  public void close() throws IOException
  {
    in.close();
  }
  
}

