/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 17, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 * However, should use native methods for conversion.
 */

public class PrintWriter extends Writer
{
  private boolean autoflush;
  private boolean error;
  Writer out;

  public PrintWriter(Writer wr)
  {
    super(wr);
    this.out = wr;
  }

  public PrintWriter(Writer wr, boolean autoflush)
  {
    super(wr);
    this.out = wr;
    this.autoflush = autoflush;
  }

  public PrintWriter(OutputStream out)
  {
    super();
    this.out = new OutputStreamWriter(out);
    this.lock = this.out;
  }

  public PrintWriter(OutputStream out, boolean autoflush)
  {
    this(out);
    this.autoflush = autoflush;
  }
  protected void setError() { error = true; }

  public boolean checkError()
  {
    flush();
    return error;
  }

  public void flush()
  {
    try
      {
	out.flush();
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void close()
  {
    try
      {
	out.close();
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void print(String str)
  {
    try
      {
	out.write(str == null ? "null" : str);
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void print(char ch)
  {
    write((int) ch);
  }

  public void print(char[] charArray)
  {
    write(charArray, 0, charArray.length);
  }

  public void print(boolean bool)
  {
    print(bool ? "true" : "false");
  }

  public void print(int inum)
  {
    print(Integer.toString(inum));
  }

  public void print(long lnum)
  {
    print(Long.toString(lnum));
  }

  public void print(float fnum)
  {
    print(Float.toString(fnum));
  }

  public void print(double dnum)
  {
    print(Double.toString(dnum));
  }

  public void print(Object obj)
  {
    print(obj == null ? "null" : obj.toString());
  }

  private static final char[] line_separator
  = System.getProperty("line.separator").toCharArray();

  public void println()
  {
    synchronized (lock)
      {
	printlnUnsynchronized();
      }
  }

  private void printlnUnsynchronized()
  {
    try
      {
	write(line_separator, 0, line_separator.length);
	if (autoflush)
	  out.flush();
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void println(boolean bool)
  {
    synchronized (lock)
      {
	print(bool);
	printlnUnsynchronized();
      }
  }
  public void println(int inum)
  {
    synchronized (lock)
      {
	print(inum);
	printlnUnsynchronized();
      }
  }

  public void println(long lnum)
  {
    synchronized (lock)
      {
	print(lnum);
	printlnUnsynchronized();
      }
  }

  public void println(float fnum)
  {
    synchronized (lock)
      {
	print(fnum);
	printlnUnsynchronized();
      }
  }

  public void println(double dnum)
  {
    synchronized (lock)
      {
	print(dnum);
	printlnUnsynchronized();
      }
  }

  public void println(Object obj)
  {
    synchronized (lock)
      {
	print(obj);
	printlnUnsynchronized();
      }
  }

  public void println(String str)
  {
    synchronized (lock)
      {
	print(str);
	printlnUnsynchronized();
      }
  }

  public void println(char ch)
  {
    synchronized (lock)
      {
	print(ch);
	printlnUnsynchronized();
      }
  }

  public void println(char[] charArray)
  {
    synchronized (lock)
      {
	print(charArray);
	printlnUnsynchronized();
      }
  }

  public void write(int ch)
  {
    try
      {
	out.write(ch);
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void write(char[] charArray, int offset, int count)
  {
    try
      {
	out.write(charArray, offset, count);
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void write(String str, int offset, int count)
  {
    try
      {
	out.write(str, offset, count);
      }
    catch (IOException ex)
      {
	error = true;
      }
  }

  public void write(char[] charArray)
  {
    write(charArray, 0, charArray.length);
  }  

  public void write(String str)
  {
    write(str, 0, str.length());
  }  
}
