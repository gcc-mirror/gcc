/* SequenceInputStream.java -- Reads multiple input streams in sequence
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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


package java.io;

import java.util.Enumeration;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
/**
  * This class merges a sequence of multiple <code>InputStream</code>'s in
  * order to form a single logical stream that can be read by applications
  * that expect only one stream.
  * <p>
  * The streams passed to the constructor method are read in order until
  * they return -1 to indicate they are at end of stream.  When a stream
  * reports end of stream, it is closed, then the next stream is read.
  * When the last stream is closed, the next attempt to read from this
  * stream will return a -1 to indicate it is at end of stream.
  * <p>
  * If this stream is closed prior to all subordinate streams being read
  * to completion, all subordinate streams are closed.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy <warrenl@cygnus.com>
  */
public class SequenceInputStream extends InputStream
{
  /** The handle for the current input stream. */
  private InputStream in;

  /** Secondary input stream; not used if constructed w/ enumeration. */
  private InputStream in2;

  /** The enum handle; not used if constructed w/ 2 explicit input streams. */
  private Enumeration enum;

 /**
  * This method creates a new <code>SequenceInputStream</code> that obtains
  * its list of subordinate <code>InputStream</code>s from the specified
  * <code>Enumeration</code>
  *
  * @param e An <code>Enumeration</code> that will return a list of
  * <code>InputStream</code>s to read in sequence
  */
  public SequenceInputStream(Enumeration e)
  {
    enum = e;
    in = (InputStream) enum.nextElement();
    in2 = null;
  }

 /**
  * This method creates a new <code>SequenceInputStream</code> that will read
  * the two specified subordinate <code>InputStream</code>s in sequence.
  *
  * @param s1 The first <code>InputStream</code> to read
  * @param s2 The second <code>InputStream</code> to read
  */
  public SequenceInputStream(InputStream s1, InputStream s2)
  {
    in = s1;
    in2 = s2;
  }

 /**
  * This method returns the number of bytes than can be read from the
  * currently being read subordinate stream before that stream could
  * block.  Note that it is possible more bytes than this can actually
  * be read without the stream blocking.  If a 0 is returned, then the
  * stream could block on the very next read.
  *
  * @return The number of bytes that can be read before blocking could occur
  *
  * @exception IOException If an error occurs
  */
  public int available() throws IOException
  {
    if (in == null)
      return 0;

    return in.available();
  }

 /**
  * Closes this stream.  This will cause any remaining unclosed subordinate
  * <code>InputStream</code>'s to be closed as well.  Subsequent attempts to 
  * read from this stream may cause an exception.
  *
  * @exception IOException If an error occurs
  */
  public void close() throws IOException
  {
    while (in != null)
      {
	in.close();
	in = getNextStream ();
      }
  }

 /**
  * This method reads an unsigned byte from the input stream and returns it
  * as an int in the range of 0-255.  This method also will return -1 if
  * the end of the stream has been reached.  This will only happen when
  * all of the subordinate streams have been read.
  * <p>
  * This method will block until the byte can be read.
  *
  * @return The byte read, or -1 if end of stream
  *
  * @exception IOException If an error occurs
  */
  public int read() throws IOException
  {
    int ch = -1;

    while (in != null && (ch = in.read()) < 0)
      {
	in.close();
        in = getNextStream();
      }

    return ch;
  }

 /**
  * This method reads bytes from a stream and stores them into a caller
  * supplied buffer.  It starts storing the data at index <code>offset</code>
  * into the buffer and attempts to read <code>len</code> bytes. This method
  * can return before reading the number of bytes requested. The actual number
  * of bytes read is returned as an int. A -1 is returend to indicate the
  * end of the stream. This will only happen when all of the subordinate
  * streams have been read.
  * <p>
  * This method will block until at least one byte can be read.
  *
  * @param b The array into which bytes read should be stored
  * @param off The offset into the array to start storing bytes
  * @param len The requested number of bytes to read
  *
  * @return The actual number of bytes read, or -1 if end of stream
  *
  * @exception IOException If an error occurs
  */
  public int read(byte[] b, int off, int len) throws IOException
  {
    int ch = -1;

    // The validity of the parameters will be checked by in.read so
    // don't bother doing it here.
    while (in != null && (ch = in.read(b, off, len)) < 0)
      {
	in.close();
        in = getNextStream();
      }

    return ch;
  }

 /**
  * This private method is used to get the next <code>InputStream</code> to
  * read from. Returns null when no more streams are available.
  */
  private InputStream getNextStream()
  {
    InputStream nextIn = null;

    if (enum != null)
      {
        if (enum.hasMoreElements())
	  nextIn = (InputStream) enum.nextElement();
      }
    else
      if (in2 != null)
	{
	  nextIn = in2;
	  in2 = null;
	}

    return nextIn;
  }
}
