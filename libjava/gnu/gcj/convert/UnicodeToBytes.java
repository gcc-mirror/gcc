/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert; 
 
public abstract class UnicodeToBytes
{
  /** Buffer to emit bytes to.
   * The locations buf[count] ... buf[buf.length-1] are available. */
  public byte[] buf;
  public int count;

  static Class defaultEncodingClass;

  static synchronized void getDefaultEncodingClass()
  {
    // Test (defaultEncodingClass == null) again in case of race condition.
    if (defaultEncodingClass == null)
      {
	String encoding = System.getProperty("file.encoding");
	String className = "gnu.gcj.convert.Output_"+encoding;
	try
	  {
	    defaultEncodingClass = Class.forName(className);
	  }
	catch (ClassNotFoundException ex)
	  {
	    throw new NoClassDefFoundError("missing default encoding "
					   + encoding + " (class "
					   + className + " not found)");
	    
	  }
      }
  }

  public abstract String getName();

  public static UnicodeToBytes getDefaultEncoder()
  {
    try
      {
	if (defaultEncodingClass == null)
	  getDefaultEncodingClass();
	return (UnicodeToBytes) defaultEncodingClass.newInstance();
      }
    catch (Throwable ex)
      {
	return new Output_8859_1();
      }
  }

  /** Get a char-stream->byte-stream converter given an encoding name. */
  public static UnicodeToBytes getEncoder (String encoding)
    throws java.io.UnsupportedEncodingException
  {
    String className = "gnu.gcj.convert.Output_"+encoding;
    Class encodingClass;
    try 
      { 
	encodingClass = Class.forName(className); 
	return (UnicodeToBytes) encodingClass.newInstance();
      } 
    catch (Throwable ex) 
      { 
	try
	  {
	    return new Output_iconv (encoding);
	  }
	catch (Throwable _)
	  {
	    // Put the original exception in the throwable.
	    throw new java.io.UnsupportedEncodingException(encoding + " ("
							   + ex + ')');
	  }
      }
  }

  public final void setOutput(byte[] buffer, int count)
  {
    this.buf = buffer;
    this.count = count;
  }

  /** Convert chars to bytes.
    * Converted bytes are written to buf, starting at count.
    * @param inbuffer source of characters to convert
    * @param inpos index of initial character in inbuffer to convert
    * @param inlength number of characters to convert
    * @return number of chars converted
    * Also, this.count is increment by the number of bytes converted.
    */
  public abstract int write (char[] inbuffer, int inpos, int inlength);

  /** Convert chars to bytes.
    * Converted bytes are written to buf, starting at count.
    * @param str source of characters to convert
    * @param inpos index of initial character in str to convert
    * @param inlength number of characters to convert
    * @param work if non-null, a buffer than can be used
    * @return number of chars converted
    * Also, this.count is increment by the number of bytes converted.
    */
  public int write (String str, int inpos, int inlength, char[] work)
  {
    if (work == null)
      work = new char[inlength];
    int srcEnd = inpos + (inlength > work.length ? work.length : inlength);
    str.getChars(inpos, srcEnd, work, 0);
    return write(work, inpos, inlength);
  }
}
