/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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

  public String getEncoding() { return converter.getName(); }

  private OutputStreamWriter(OutputStream out, UnicodeToBytes encoder)
  {
    this.out = out instanceof BufferedOutputStream 
	       ? (BufferedOutputStream) out
	       : new BufferedOutputStream(out, 250);
    /* Don't need to call super(out) here as long as the lock gets set. */
    this.lock = out;
    this.converter = encoder;
  }

  public OutputStreamWriter(OutputStream out, String enc)
   throws UnsupportedEncodingException
  {
    this(out, UnicodeToBytes.getEncoder(enc));
  }

  public OutputStreamWriter(OutputStream out)
  {
    this(out, UnicodeToBytes.getDefaultEncoder());
  }

  public void close() throws IOException
  {
    synchronized (lock)
      {
	flush();
	if (out != null)
	  {
	    out.close();
	    out = null;
	  }
	work = null;
      }
  }

  public void flush() throws IOException
  {
    synchronized (lock)
      {
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

  public void write(int ch) throws IOException
  {
    synchronized (lock)
      {
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
}
