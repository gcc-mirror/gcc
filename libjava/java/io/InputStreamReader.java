/* Copyright (C) 1998, 1999  Red Hat, Inc.

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
    this.in = in instanceof BufferedInputStream
              ? (BufferedInputStream) in
              : new BufferedInputStream(in, 250);
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

  public String getEncoding() { return converter.getName(); }

  public boolean ready() throws IOException
  {
    synchronized (lock)
      {
	if (wpos < wcount)
	  return true;
	if (work == null)
	  {
	    work = new char[100];
	    wpos = 0;
	    wcount = 0;
	  }
	for (;;)
	  {
	    if (in.available() <= 0)
	      return false;
	    in.mark(1);
	    int b = in.read();
	    if (b < 0)
	      return true;
	    in.reset();
	    converter.setInput(in.buf, in.pos, in.count);
	    wpos = 0;
	    wcount = converter.read(work, 0, work.length);
	    in.skip(converter.inpos - in.pos);
	    if (wcount > 0)
	      return true;
	  }
      }
  }

  public int read(char buf[], int offset, int length) throws IOException
  {
    synchronized (lock)
      {
	int wavail = wcount - wpos;
	if (wavail > 0)
	  {
	    if (length > wavail)
	      length = wavail;
	    System.arraycopy(work, wpos, buf, offset, length);
	    wpos += length;
	    return length;
	  }
	else
	  {
	    if (length == 0)
	      return 0;
	    for (;;)
	      {
		in.mark(1);
		int b = in.read();
		if (b < 0)
		  return -1;
		in.reset();
		converter.setInput(in.buf, in.pos, in.count);
		int count = converter.read (buf, offset, length);
		in.skip(converter.inpos - in.pos);
		if (count > 0)
		  return count;
	      }
	  }
      }
  }

  public int read() throws IOException
  {
    synchronized (lock)
      {
	int wavail = wcount - wpos;
	if (wavail > 0)
	  return work[wpos++];
	if (work == null)
	  {
	    work = new char[100];
	    wpos = 0;
	    wcount = 0;
	  }
	else if (wavail == 0)
	  {
	    wpos = 0;
	    wcount = 0;
	  }
	int count = read(work, wpos, work.length-wpos);
	if (count <= 0)
	  return -1;
	wcount = wpos + count;
	return work[wpos++];
      }
  }
}
