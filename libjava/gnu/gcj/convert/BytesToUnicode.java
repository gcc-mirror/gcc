/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

public abstract class BytesToUnicode
{
  /** Buffer to read bytes from.
   * The characters inbuffer[inpos] ... inbuffer[inlength-1] are available. */
  public byte[] inbuffer;
  /** Starting index in buffer to read bytes from. */
  public int inpos;
  /** End of valid bytes in buffer. */
  public int inlength;

  static Class defaultDecodingClass;

  static synchronized void getDefaultDecodingClass()
  {
    // Test (defaultDecodingClass == null) again in case of race condition.
    if (defaultDecodingClass == null)
      {
	String encoding = System.getProperty("file.encoding");
	String className = "gnu.gcj.convert.Input_"+encoding;
	try
	  {
	    defaultDecodingClass = Class.forName(className);
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

  public static BytesToUnicode getDefaultDecoder()
  {
    try
      {
	if (defaultDecodingClass == null)
	  getDefaultDecodingClass();
	return (BytesToUnicode) defaultDecodingClass.newInstance();
      }
    catch (Throwable ex)
      {
	return new Input_8859_1();
      }
  }

  /** Get a byte-stream->char-stream converter given an encoding name. */
  public static BytesToUnicode getDecoder (String encoding)
    throws java.io.UnsupportedEncodingException
  {
    String className = "gnu.gcj.convert.Input_"+encoding;
    Class decodingClass;
    try 
      { 
	decodingClass = Class.forName(className); 
	return (BytesToUnicode) decodingClass.newInstance();
      } 
    catch (Throwable ex) 
      { 
	try
	  {
	    return new Input_iconv (encoding);
	  }
	catch (Throwable _)
	  {
	    throw new java.io.UnsupportedEncodingException(encoding
							   + " (" + ex + ')');
	  }
      }
  }

  /** Make input bytes available to the conversion.
   * @param buffer source of input bytes
   * @param pos index of first available byte
   * @param length one more than index of last available byte
   */
  public final void setInput(byte[] buffer, int pos, int length)
  {
    inbuffer = buffer;
    inpos = pos;
    inlength = length;
  }

  /** Convert bytes to chars.
   * Input bytes are taken from this.inbuffer.  The available input
   * bytes start at inbuffer[inpos], and end at inbuffer[inlength-1].
   * @param outbuffer buffer for the converted character
   * @param outpos position in buffer to start putting converted characters
   * @param count the maximum number of characters to convert
   * @return number of chars placed in outbuffer.
   * Also, this.inpos is incremented by the number of bytes consumed.
   *
   * (Note the asymmetry in that the input upper bound is inbuffer[inlength-1],
   * while the output upper bound is outbuffer[outpos+count-1].  The
   * justification is that inlength is like the count field of a
   * BufferedInputStream, while the count parameter is like the
   * length parameter of a read request.)  The count parameter is
   * also defined to be <= outbuffer.length - outpos (per the specification
   * of the length parameter for a read request).
   */
  public abstract int read (char[] outbuffer, int outpos, int count);
}
