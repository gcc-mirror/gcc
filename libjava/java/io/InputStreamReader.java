/* Copyright (C) 1998, 1999, 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
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
