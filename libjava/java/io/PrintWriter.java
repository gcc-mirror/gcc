/* PrintWriter.java -- prints primitive values and objects to a stream as text
   Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

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

package java.io;

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 * However, should use native methods for conversion.
 */

/**
 * This class prints Java primitive values and objects to a stream as
 * text.  None of the methods in this class throw an exception.  However,
 * errors can be detected by calling the <code>checkError()</code> method.
 * Additionally, this stream can be designated as "autoflush" when 
 * created so that any writes are automatically flushed to the underlying
 * output sink whenever one of the <code>println</code> methods is
 * called.  (Note that this differs from the <code>PrintStream</code>
 * class which also auto-flushes when it encounters a newline character
 * in the chars written).
 *
 * @author Per Bothner (bothner@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date April 17, 1998.  
 */
public class PrintWriter extends Writer
{
  /**
   * <code>true</code> if auto-flush is enabled, <code>false</code> otherwise
   */
  private boolean autoflush;

  /**
   * This boolean indicates whether or not an error has ever occurred
   * on this stream.
   */
  private boolean error;

  /**
   * This is the underlying <code>Writer</code> we are sending output
   * to
   */
  protected Writer out;

  /**
   * This method intializes a new <code>PrintWriter</code> object to write
   * to the specified output sink.  The form of the constructor does not
   * enable auto-flush functionality.
   *
   * @param wr The <code>Writer</code> to write to.
   */
  public PrintWriter(Writer wr)
  {
    super(wr.lock);
    this.out = wr;
  }

  /**
   * This method intializes a new <code>PrintWriter</code> object to write
   * to the specified output sink.  This constructor also allows "auto-flush"
   * functionality to be specified where the stream will be flushed after
   * every line is terminated or newline character is written.
   *
   * @param wr The <code>Writer</code> to write to.
   * @param autoflush <code>true</code> to flush the stream after every 
   * line, <code>false</code> otherwise
   */
  public PrintWriter(Writer wr, boolean autoflush)
  {
    super(wr.lock);
    this.out = wr;
    this.autoflush = autoflush;
  }

  /**
   * This method initializes a new <code>PrintWriter</code> object to write
   * to the specified <code>OutputStream</code>.  Characters will be converted
   * to chars using the system default encoding.  Auto-flush functionality
   * will not be enabled.
   *
   * @param out The <code>OutputStream</code> to write to
   */
  public PrintWriter(OutputStream out)
  {
    super();
    this.out = new OutputStreamWriter(out);
    this.lock = this.out;
  }

  /**
   * This method initializes a new <code>PrintWriter</code> object to write
   * to the specified <code>OutputStream</code>.  Characters will be converted
   * to chars using the system default encoding.  This form of the 
   * constructor allows auto-flush functionality to be enabled if desired
   *
   * @param out The <code>OutputStream</code> to write to
   * @param autoflush <code>true</code> to flush the stream after every 
   * <code>println</code> call, <code>false</code> otherwise.
   */
  public PrintWriter(OutputStream out, boolean autoflush)
  {
    this(out);
    this.autoflush = autoflush;
  }

  /**
   * This method can be called by subclasses to indicate that an error
   * has occurred and should be reported by <code>checkError</code>.
   */
  protected void setError()
  {
    error = true;
  }

  /**
   * This method checks to see if an error has occurred on this stream.  Note
   * that once an error has occurred, this method will continue to report
   * <code>true</code> forever for this stream.  Before checking for an
   * error condition, this method flushes the stream.
   *
   * @return <code>true</code> if an error has occurred, 
   * <code>false</code> otherwise
   */
  public boolean checkError()
  {
    flush();
    return error;
  }

  /**
   * This method flushes any buffered chars to the underlying stream and
   * then flushes that stream as well.
   */
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

  /**
   * This method closes this stream and all underlying streams.
   */
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

  /**
   * This method prints a <code>String</code> to the stream.  The actual
   * value printed depends on the system default encoding.
   *
   * @param str The <code>String</code> to print.
   */
  public void print(String str)
  {
    write(str == null ? "null" : str);
  }

  /**
   * This method prints a char to the stream.  The actual value printed is
   * determined by the character encoding in use.
   *
   * @param ch The <code>char</code> value to be printed
   */
  public void print(char ch)
  {
    write((int) ch);
  }

  /**
   * This method prints an array of characters to the stream.  The actual
   * value printed depends on the system default encoding.
   *
   * @param charArray The array of characters to print.
   */
  public void print(char[] charArray)
  {
    write(charArray, 0, charArray.length);
  }

  /**
   * This methods prints a boolean value to the stream.  <code>true</code>
   * values are printed as "true" and <code>false</code> values are printed
   * as "false".
   *
   * @param bool The <code>boolean</code> value to print
   */
  public void print(boolean bool)
  {
    // We purposely call write() and not print() here.  This preserves
    // compatibility with JDK 1.2.
    write (bool ? "true" : "false");
  }

  /**
   * This method prints an integer to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * @param inum The <code>int</code> value to be printed
   */
  public void print(int inum)
  {
    // We purposely call write() and not print() here.  This preserves
    // compatibility with JDK 1.2.
    write(Integer.toString(inum));
  }

  /**
   * This method prints a long to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * @param lnum The <code>long</code> value to be printed
   */
  public void print(long lnum)
  {
    // We purposely call write() and not print() here.  This preserves
    // compatibility with JDK 1.2.
    write(Long.toString(lnum));
  }

  /**
   * This method prints a float to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * @param fnum The <code>float</code> value to be printed
   */
  public void print(float fnum)
  {
    // We purposely call write() and not print() here.  This preserves
    // compatibility with JDK 1.2.
    write(Float.toString(fnum));
  }

  /**
   * This method prints a double to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * @param dnum The <code>double</code> value to be printed
   */
  public void print(double dnum)
  {
    // We purposely call write() and not print() here.  This preserves
    // compatibility with JDK 1.2.
    write(Double.toString(dnum));
  }

  /**
   * This method prints an <code>Object</code> to the stream.  The actual
   * value printed is determined by calling the <code>String.valueOf()</code>
   * method.
   *
   * @param obj The <code>Object</code> to print.
   */
  public void print(Object obj)
  {
    // We purposely call write() and not print() here.  This preserves
    // compatibility with JDK 1.2.
    write(obj == null ? "null" : obj.toString());
  }

  /**
   * This is the system dependent line separator
   */
  private static final char[] line_separator
    = System.getProperty("line.separator").toCharArray();

  /**
   * This method prints a line separator sequence to the stream.  The value
   * printed is determined by the system property <xmp>line.separator</xmp>
   * and is not necessarily the Unix '\n' newline character.
   */
  public void println()
  {
    synchronized (lock)
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
  }

  /**
   * This methods prints a boolean value to the stream.  <code>true</code>
   * values are printed as "true" and <code>false</code> values are printed
   * as "false".
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param bool The <code>boolean</code> value to print
   */
  public void println(boolean bool)
  {
    synchronized (lock)
      {
	print(bool);
	println();
      }
  }

  /**
   * This method prints an integer to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param inum The <code>int</code> value to be printed
   */
  public void println(int inum)
  {
    synchronized (lock)
      {
	print(inum);
	println();
      }
  }

  /**
   * This method prints a long to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param lnum The <code>long</code> value to be printed
   */
  public void println(long lnum)
  {
    synchronized (lock)
      {
	print(lnum);
	println();
      }
  }

  /**
   * This method prints a float to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param fnum The <code>float</code> value to be printed
   */
  public void println(float fnum)
  {
    synchronized (lock)
      {
	print(fnum);
	println();
      }
  }

  /**
   * This method prints a double to the stream.  The value printed is
   * determined using the <code>String.valueOf()</code> method.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param dnum The <code>double</code> value to be printed
   */
  public void println(double dnum)
  {
    synchronized (lock)
      {
	print(dnum);
	println();
      }
  }

  /**
   * This method prints an <code>Object</code> to the stream.  The actual
   * value printed is determined by calling the <code>String.valueOf()</code>
   * method.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param obj The <code>Object</code> to print.
   */
  public void println(Object obj)
  {
    synchronized (lock)
      {
	print(obj);
	println();
      }
  }

  /**
   * This method prints a <code>String</code> to the stream.  The actual
   * value printed depends on the system default encoding.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param str The <code>String</code> to print.
   */
  public void println(String str)
  {
    synchronized (lock)
      {
	print(str);
	println();
      }
  }

  /**
   * This method prints a char to the stream.  The actual value printed is
   * determined by the character encoding in use.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param ch The <code>char</code> value to be printed
   */
  public void println(char ch)
  {
    synchronized (lock)
      {
	print(ch);
	println();
      }
  }

  /**
   * This method prints an array of characters to the stream.  The actual
   * value printed depends on the system default encoding.
   *
   * This method prints a line termination sequence after printing the value.
   *
   * @param charArray The array of characters to print.
   */
  public void println(char[] charArray)
  {
    synchronized (lock)
      {
	print(charArray);
	println();
      }
  }

  /**
   * This method writes a single char to the stream. 
   * 
   * @param ch The char to be written, passed as a int
   */
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

  /**
   * This method writes <code>count</code> chars from the specified array 
   * starting at index <code>offset</code> into the array.
   *
   * @param charArray The array of chars to write
   * @param offset The index into the array to start writing from
   * @param count The number of chars to write
  */
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

  /**
   * This method writes <code>count</code> chars from the specified
   * <code>String</code> to the output starting at character position
   * <code>offset</code> into the <code>String</code>
   *
   * @param str The <code>String</code> to write chars from
   * @param offset The offset into the <code>String</code> to start writing from
   * @param count The number of chars to write.
   */
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

  /**
   * This method write all the chars in the specified array to the output.
   *
   * @param charArray The array of characters to write
   */
  public void write(char[] charArray)
  {
    write(charArray, 0, charArray.length);
  }  

  /**
   * This method writes the contents of the specified <code>String</code>
   * to the underlying stream.
   *
   * @param str The <code>String</code> to write
   */
  public void write(String str)
  {
    write(str, 0, str.length());
  }  
}

