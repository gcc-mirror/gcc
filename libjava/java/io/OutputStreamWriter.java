/* OutputStreamWriter.java -- Writer that converts chars to bytes
   Copyright (C) 1998, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.

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
import gnu.gcj.convert.UnicodeToBytes;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 17, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, but only supports 8859_1.
 */
public class OutputStreamWriter extends Writer
{
  BufferedOutputStream out;

  UnicodeToBytes converter;

  /* Temporary buffer. */
  private char[] work;
  private int wcount;

  private OutputStreamWriter(OutputStream out, UnicodeToBytes encoder)
  {
    this.out = out instanceof BufferedOutputStream 
	       ? (BufferedOutputStream) out
	       : new BufferedOutputStream(out, 250);
    /* Don't need to call super(out) here as long as the lock gets set. */
    this.lock = out;
    this.converter = encoder;
  }

  public OutputStreamWriter(OutputStream out, String encoding_scheme)
   throws UnsupportedEncodingException
  {
    this(out, UnicodeToBytes.getEncoder(encoding_scheme));
  }

  /**
   * This method initializes a new instance of <code>OutputStreamWriter</code>
   * to write to the specified stream using the default encoding.
   *
   * @param out The <code>OutputStream</code> to write to
   */
  public OutputStreamWriter(OutputStream out)
  {
    this(out, UnicodeToBytes.getDefaultEncoder());
  }

  /**
   * This method closes this stream, and the underlying 
   * <code>OutputStream</code>
   *
   * @exception IOException If an error occurs
   */
  public void close() throws IOException
  {
    synchronized (lock)
      {
	if (out != null)
	  {
	    flush();
	    out.close();
	    out = null;
	  }
	work = null;
      }
  }

  /**
   * This method returns the name of the character encoding scheme currently
   * in use by this stream.  If the stream has been closed, then this method
   * may return <code>null</code>.
   *
   * @return The encoding scheme name
   */
  public String getEncoding()
  {
    return out != null ? converter.getName() : null;
  }

  /**
   * This method flushes any buffered bytes to the underlying output sink.
   *
   * @exception IOException If an error occurs
   */
  public void flush() throws IOException
  {
    synchronized (lock)
      {
	if (out == null)
	  throw new IOException("Stream closed");

	if (wcount > 0)
	  {
	    writeChars(work, 0, wcount);
	    wcount = 0;
	  }
	out.flush();
      }
  }

  public void write(char[] buf, int offset, int count)
     throws IOException
  {
    synchronized (lock)
      {
	if (out == null)
	  throw new IOException("Stream closed");

	if (wcount > 0)
	  {
	    writeChars(work, 0, wcount);
	    wcount = 0;
	  }
	writeChars(buf, offset, count);
      }
  }

  /** Writes characters through to the inferior BufferedOutputStream.
   * Ignores wcount and the work buffer. */
  private void writeChars(char[] buf, int offset, int count)
    throws IOException
  {
    while (count > 0)
      {
	// We must flush if out.count == out.buf.length.
	// It is probably a good idea to flush if out.buf is almost full.
	// This test is an approximation for "almost full".
	if (out.count + count >= out.buf.length)
	  {
	    out.flush();
	    if (out.count != 0)
	      throw new IOException("unable to flush output byte buffer");
	  }
	converter.setOutput(out.buf, out.count);
	int converted = converter.write(buf, offset, count);
	offset += converted;
	count -= converted;
	out.count = converter.count;
      }
  }

  public void write(String str, int offset, int count)
     throws IOException
  {
    synchronized (lock)
      {
	if (out == null)
	  throw new IOException("Stream closed");

	if (work == null)
	  work = new char[100];
	int wlength = work.length;
	while (count > 0)
	  {
	    int size = count;
	    if (wcount + size > wlength)
	      {
		if (2*wcount > wlength)
		  {
		    writeChars(work, 0, wcount);
		    wcount = 0;
		  }
		if (wcount + size > wlength)
		  size = wlength - wcount;
	      }
	    str.getChars(offset, offset+size, work, wcount);
	    offset += size;
	    count -= size;
	    wcount += size;
	  }
      }
  }

  /**
   * This method writes a single character to the output stream.
   *
   * @param c The char to write, passed as an int.
   *
   * @exception IOException If an error occurs
   */
  public void write(int ch) throws IOException
  {
    synchronized (lock)
      {
	if (out == null)
	  throw new IOException("Stream closed");

	if (work == null)
	  work = new char[100];
	if (wcount >= work.length)
	  {
	    writeChars(work, 0, wcount);
	    wcount = 0;
	  }
	work[wcount++] = (char) ch;
      }
  }

} // class OutputStreamWriter

