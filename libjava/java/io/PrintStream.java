// PrintStream.java - Print string representations

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;
import gnu.gcj.convert.UnicodeToBytes;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Not finished.
 */

public class PrintStream extends FilterOutputStream
{
  /* Notice the implementation is quite similar to OutputStreamWriter.
   * This leads to some minor duplication, because neither inherits
   * from the other, and we want to maximize performance. */

  public boolean checkError ()
  {
    return error;
  }

  public void close ()
  {
    try
      {
	out.close();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  public void flush ()
  {
    try
      {
	out.flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  private synchronized void print (String str, boolean println)
  {
    try
      {
        writeChars(str, 0, str.length());
	if (println)
	  writeChars(line_separator, 0, line_separator.length);
	if (auto_flush)
	  flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  private synchronized void print (char[] chars, int pos, int len,
				   boolean println)
  {
    try
      {
        writeChars(chars, pos, len);
	if (println)
	  writeChars(line_separator, 0, line_separator.length);
	if (auto_flush)
	  flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  /** Writes characters through to the inferior BufferedOutputStream. */
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

  private void writeChars(String str, int offset, int count)
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
	int converted = converter.write(str, offset, count, work);
	offset += converted;
	count -= converted;
	out.count = converter.count;
      }
  }

  public void print (boolean bool)
  {
    print(String.valueOf(bool), false);
  }

  public void print (int inum)
  {
    print(String.valueOf(inum), false);
  }

  public void print (long lnum)
  {
    print(String.valueOf(lnum), false);
  }

  public void print (float fnum)
  {
    print(String.valueOf(fnum), false);
  }

  public void print (double dnum)
  {
    print(String.valueOf(dnum), false);
  }

  public void print (Object obj)
  {
    print(obj == null ? "null" : obj.toString(), false);
  }

  public void print (String str)
  {
    print(str == null ? "null" : str, false);
  }

  public synchronized void print (char ch)
  {
    work[0] = ch;
    print(work, 0, 1, false);
  }

  public void print (char[] charArray)
  {
    print(charArray, 0, charArray.length, false);
  }

  public void println ()
  {
    print(line_separator, 0, line_separator.length, false);
  }

  public void println (boolean bool)
  {
    print(String.valueOf(bool), true);
  }

  public void println (int inum)
  {
    print(String.valueOf(inum), true);
  }

  public void println (long lnum)
  {
    print(String.valueOf(lnum), true);
  }

  public void println (float fnum)
  {
    print(String.valueOf(fnum), true);
  }

  public void println (double dnum)
  {
    print(String.valueOf(dnum), true);
  }

  public void println (Object obj)
  {
    print(obj == null ? "null" : obj.toString(), true);
  }

  public void println (String str)
  {
    print (str == null ? "null" : str, true);
  }

  public synchronized void println (char ch)
  {
    work[0] = ch;
    print(work, 0, 1, true);
  }

  public void println (char[] charArray)
  {
    print(charArray, 0, charArray.length, true);
  }

  public PrintStream (OutputStream out)
  {
    this(out, false);
  }

  public PrintStream (OutputStream out, boolean af)
  {
    super(out);
    if (out instanceof BufferedOutputStream)
      this.out = (BufferedOutputStream) out;
    else
      {
        this.out = new BufferedOutputStream(out, 250);
	/* PrintStream redefines "out". Explicitly reset FilterOutputStream's
	 * "out" so that they're referring to the same thing. */
	super.out = this.out;    
      }
    converter = UnicodeToBytes.getDefaultEncoder();
    error = false;
    auto_flush = af;
  }

  protected void setError ()
  {
    error = true;
  }

  public void write (int oneByte)
  {
    try
      {
	out.write(oneByte);
	if (auto_flush && oneByte == '\n')
	  out.flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  public void write (byte[] buffer, int offset, int count)
  {
    try
      {
	out.write(buffer, offset, count);
	if (auto_flush)
	  out.flush();
      }
    catch (IOException e)
      {
	setError ();
      }
  }

  BufferedOutputStream out;
  UnicodeToBytes converter;

  char[] work = new char[100];

  // True if error occurred.
  private boolean error;
  // True if auto-flush.
  private boolean auto_flush;

  // Line separator string.
  private static final char[] line_separator
    = System.getProperty("line.separator").toCharArray();
}
