/* InputStreamReader.java -- Reader than transforms bytes to chars
   Copyright (C) 1998, 1999, 2001, 2003 Free Software Foundation, Inc.

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
import gnu.gcj.convert.*;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 22, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, but only supports 8859_1.
 */

public class InputStreamReader extends Reader
{
  BufferedInputStream in;

  // Buffer of chars read from in and converted but not consumed.
  char[] work;
  // Next available character (in work buffer) to read.
  int wpos;
  // Last available character (in work buffer) to read.
  int wcount;

  BytesToUnicode converter;

  public InputStreamReader(InputStream in)
  {
    this(in, BytesToUnicode.getDefaultDecoder());
  }

  public InputStreamReader(InputStream in, String enc)
    throws UnsupportedEncodingException
  {
    this(in, BytesToUnicode.getDecoder(enc));
  }

  private InputStreamReader(InputStream in, BytesToUnicode decoder)
  {
    // FIXME: someone could pass in a BufferedInputStream whose buffer
    // is smaller than the longest encoded character for this
    // encoding.  We will probably go into an infinite loop in this
    // case.  We probably ought to just have our own byte buffering
    // here.
    this.in = in instanceof BufferedInputStream
              ? (BufferedInputStream) in
              : new BufferedInputStream(in);
    /* Don't need to call super(in) here as long as the lock gets set. */
    this.lock = in;
    converter = decoder;
    converter.setInput(this.in.buf, 0, 0);
  }

  public void close() throws IOException
  {
    synchronized (lock)
      {
	if (in != null)
	  in.close();
	in = null;
	work = null;
	wpos = wcount = 0;
      }
  }

  public String getEncoding()
  {
    return in != null ? converter.getName() : null;
  }

  public boolean ready() throws IOException
  {
    synchronized (lock)
      {
	if (in == null)
	  throw new IOException("Stream closed");

	if (wpos < wcount)
	  return true;

	// According to the spec, an InputStreamReader is ready if its
	// input buffer is not empty (above), or if bytes are
	// available on the underlying byte stream.
	return in.available () > 0;
      }
  }

  public int read(char buf[], int offset, int length) throws IOException
  {
    synchronized (lock)
      {
	if (in == null)
	  throw new IOException("Stream closed");

	if (length == 0)
	  return 0;

	int wavail = wcount - wpos;
	if (wavail <= 0)
	  {
	    // Nothing waiting, so refill our buffer.
	    if (! refill ())
	      return -1;
	    wavail = wcount - wpos;
	  }

	if (length > wavail)
	  length = wavail;
	System.arraycopy(work, wpos, buf, offset, length);
	wpos += length;
	return length;
      }
  }

  public int read() throws IOException
  {
    synchronized (lock)
      {
	if (in == null)
	  throw new IOException("Stream closed");

	int wavail = wcount - wpos;
	if (wavail <= 0)
	  {
	    // Nothing waiting, so refill our buffer.
	    if (! refill ())
	      return -1;
	  }

	return work[wpos++];
      }
  }

  // Read more bytes and convert them into the WORK buffer.
  // Return false on EOF.
  private boolean refill () throws IOException
  {
    wcount = wpos = 0;

    if (work == null)
      work = new char[100];

    for (;;)
      {
	// We have knowledge of the internals of BufferedInputStream
	// here.  Eww.
	in.mark (0);
	// BufferedInputStream.refill() can only be called when
	// `pos>=count'.
	boolean r = in.pos < in.count || in.refill ();
	in.reset ();
	if (! r)
	  return false;
	converter.setInput(in.buf, in.pos, in.count);
	int count = converter.read (work, wpos, work.length - wpos);
	in.skip(converter.inpos - in.pos);
	if (count > 0)
	  {
	    wcount += count;
	    return true;
	  }
      }
  }
}
