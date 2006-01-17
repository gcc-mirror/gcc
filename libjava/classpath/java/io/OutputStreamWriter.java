/* OutputStreamWriter.java -- Writer that converts chars to bytes
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2005  Free Software Foundation, Inc.

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

import gnu.java.nio.charset.EncodingHelper;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.MalformedInputException;

/**
 * This class writes characters to an output stream that is byte oriented
 * It converts the chars that are written to bytes using an encoding layer,
 * which is specific to a particular encoding standard.  The desired
 * encoding can either be specified by name, or if no encoding is specified,
 * the system default encoding will be used.  The system default encoding
 * name is determined from the system property <code>file.encoding</code>.
 * The only encodings that are guaranteed to be available are "8859_1"
 * (the Latin-1 character set) and "UTF8".  Unfortunately, Java does not
 * provide a mechanism for listing the encodings that are supported in
 * a given implementation.
 * <p>
 * Here is a list of standard encoding names that may be available:
 * <p>
 * <ul>
 * <li>8859_1 (ISO-8859-1/Latin-1)
 * <li>8859_2 (ISO-8859-2/Latin-2)
 * <li>8859_3 (ISO-8859-3/Latin-3)
 * <li>8859_4 (ISO-8859-4/Latin-4)
 * <li>8859_5 (ISO-8859-5/Latin-5)
 * <li>8859_6 (ISO-8859-6/Latin-6)
 * <li>8859_7 (ISO-8859-7/Latin-7)
 * <li>8859_8 (ISO-8859-8/Latin-8)
 * <li>8859_9 (ISO-8859-9/Latin-9)
 * <li>ASCII (7-bit ASCII)
 * <li>UTF8 (UCS Transformation Format-8)
 * <li>More Later
 * </ul>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 * @date April 17, 1998.  
 */
public class OutputStreamWriter extends Writer
{
  /**
   * The output stream.
   */
  private OutputStream out;

  /**
   * The charset encoder.
   */
  private CharsetEncoder encoder;

  /**
   * java.io canonical name of the encoding.
   */
  private String encodingName;

  /**
   * Buffer output before character conversion as it has costly overhead.
   */
  private CharBuffer outputBuffer;
  private final static int BUFFER_SIZE = 1024;

  /**
   * This method initializes a new instance of <code>OutputStreamWriter</code>
   * to write to the specified stream using a caller supplied character
   * encoding scheme.  Note that due to a deficiency in the Java language
   * design, there is no way to determine which encodings are supported.
   *
   * @param out The <code>OutputStream</code> to write to
   * @param encoding_scheme The name of the encoding scheme to use for 
   * character to byte translation
   *
   * @exception UnsupportedEncodingException If the named encoding is 
   * not available.
   */
  public OutputStreamWriter (OutputStream out, String encoding_scheme) 
    throws UnsupportedEncodingException
  {
    this.out = out;
    try 
      {
	// Don't use NIO if avoidable
	if(EncodingHelper.isISOLatin1(encoding_scheme))
	  {
	    encodingName = "ISO8859_1";
	    encoder = null;
	    return;
	  }

	/*
	 * Workraround for encodings with a byte-order-mark.
	 * We only want to write it once per stream.
	 */
	try 
	  {
	    if(encoding_scheme.equalsIgnoreCase("UnicodeBig") || 
	       encoding_scheme.equalsIgnoreCase("UTF-16") ||
	       encoding_scheme.equalsIgnoreCase("UTF16"))
	      {
		encoding_scheme = "UTF-16BE";	  
		out.write((byte)0xFE);
		out.write((byte)0xFF);
	      } 
	    else if(encoding_scheme.equalsIgnoreCase("UnicodeLittle")){
	      encoding_scheme = "UTF-16LE";
	      out.write((byte)0xFF);
	      out.write((byte)0xFE);
	    }
	  }
	catch(IOException ioe)
	  {
	  }
      
	outputBuffer = CharBuffer.allocate(BUFFER_SIZE);

	Charset cs = EncodingHelper.getCharset(encoding_scheme);
	if(cs == null)
	  throw new UnsupportedEncodingException("Encoding "+encoding_scheme+
						 " unknown");
	encoder = cs.newEncoder();
	encodingName = EncodingHelper.getOldCanonical(cs.name());

	encoder.onMalformedInput(CodingErrorAction.REPLACE);
	encoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
      } 
    catch(RuntimeException e) 
      {
	// Default to ISO Latin-1, will happen if this is called, for instance,
	//  before the NIO provider is loadable.
	encoder = null; 
	encodingName = "ISO8859_1";
      }
  }

  /**
   * This method initializes a new instance of <code>OutputStreamWriter</code>
   * to write to the specified stream using the default encoding.
   *
   * @param out The <code>OutputStream</code> to write to
   */
  public OutputStreamWriter (OutputStream out)
  {
    this.out = out;
    outputBuffer = null;
    try 
      {
	String encoding = System.getProperty("file.encoding");
	Charset cs = Charset.forName(encoding);
	encoder = cs.newEncoder();
	encodingName =  EncodingHelper.getOldCanonical(cs.name());
      } 
    catch(RuntimeException e) 
      {
	encoder = null; 
	encodingName = "ISO8859_1";
      }

    if(encoder != null)
      {
	encoder.onMalformedInput(CodingErrorAction.REPLACE);
	encoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
	outputBuffer = CharBuffer.allocate(BUFFER_SIZE);
      }
  }

  /**
   * This method initializes a new instance of <code>OutputStreamWriter</code>
   * to write to the specified stream using a given <code>Charset</code>.
   *
   * @param out The <code>OutputStream</code> to write to
   * @param cs The <code>Charset</code> of the encoding to use
   * 
   * @since 1.5
   */
  public OutputStreamWriter(OutputStream out, Charset cs)
  {
    this.out = out;
    encoder = cs.newEncoder();
    encoder.onMalformedInput(CodingErrorAction.REPLACE);
    encoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
    outputBuffer = CharBuffer.allocate(BUFFER_SIZE);
  }
  
  /**
   * This method initializes a new instance of <code>OutputStreamWriter</code>
   * to write to the specified stream using a given
   * <code>CharsetEncoder</code>.
   *
   * @param out The <code>OutputStream</code> to write to
   * @param enc The <code>CharsetEncoder</code> to encode the output with
   * 
   * @since 1.5
   */
  public OutputStreamWriter(OutputStream out, CharsetEncoder enc)
  {
    this.out = out;
    encoder = enc;
    outputBuffer = CharBuffer.allocate(BUFFER_SIZE);
  }

  /**
   * This method closes this stream, and the underlying 
   * <code>OutputStream</code>
   *
   * @exception IOException If an error occurs
   */
  public void close () throws IOException
  {
    if(out == null)
      return;
    flush();
    out.close ();
    out = null;
  }

  /**
   * This method returns the name of the character encoding scheme currently
   * in use by this stream.  If the stream has been closed, then this method
   * may return <code>null</code>.
   *
   * @return The encoding scheme name
   */
  public String getEncoding ()
  {
    return out != null ? encodingName : null;
  }

  /**
   * This method flushes any buffered bytes to the underlying output sink.
   *
   * @exception IOException If an error occurs
   */
  public void flush () throws IOException
  {
      if(out != null){	  
	  if(outputBuffer != null){
	      char[] buf = new char[outputBuffer.position()];
	      if(buf.length > 0){
		  outputBuffer.flip();
		  outputBuffer.get(buf);
		  writeConvert(buf, 0, buf.length);
		  outputBuffer.clear();
	      }
	  }
	  out.flush ();
      }
  }

  /**
   * This method writes <code>count</code> characters from the specified
   * array to the output stream starting at position <code>offset</code>
   * into the array.
   *
   * @param buf The array of character to write from
   * @param offset The offset into the array to start writing chars from
   * @param count The number of chars to write.
   *
   * @exception IOException If an error occurs
   */
  public void write (char[] buf, int offset, int count) throws IOException
  {
    if(out == null)
      throw new IOException("Stream is closed.");
    if(buf == null)
      throw new IOException("Buffer is null.");

    if(outputBuffer != null)
	{
	    if(count >= outputBuffer.remaining())
		{
		    int r = outputBuffer.remaining();
		    outputBuffer.put(buf, offset, r);
		    writeConvert(outputBuffer.array(), 0, BUFFER_SIZE);
		    outputBuffer.clear();
		    offset += r;
		    count -= r;
		    // if the remaining bytes is larger than the whole buffer, 
		    // just don't buffer.
		    if(count >= outputBuffer.remaining()){
                      writeConvert(buf, offset, count);
		      return;
		    }
		}
	    outputBuffer.put(buf, offset, count);
	} else writeConvert(buf, offset, count);
  }

 /**
  * Converts and writes characters.
  */
  private void writeConvert (char[] buf, int offset, int count) 
      throws IOException
  {
    if(encoder == null)
    {
      byte[] b = new byte[count];
      for(int i=0;i<count;i++)
	b[i] = (byte)((buf[offset+i] <= 0xFF)?buf[offset+i]:'?');
      out.write(b);
    } else {
      try  {
	ByteBuffer output = encoder.encode(CharBuffer.wrap(buf,offset,count));
	encoder.reset();
	if(output.hasArray())
	  out.write(output.array());
	else
	  {
	    byte[] outbytes = new byte[output.remaining()];
	    output.get(outbytes);
	    out.write(outbytes);
	  }
      } catch(IllegalStateException e) {
	throw new IOException("Internal error.");
      } catch(MalformedInputException e) {
	throw new IOException("Invalid character sequence.");
      } catch(CharacterCodingException e) {
	throw new IOException("Unmappable character.");
      }
    }
  }

  /**
   * This method writes <code>count</code> bytes from the specified 
   * <code>String</code> starting at position <code>offset</code> into the
   * <code>String</code>.
   *
   * @param str The <code>String</code> to write chars from
   * @param offset The position in the <code>String</code> to start 
   * writing chars from
   * @param count The number of chars to write
   *
   * @exception IOException If an error occurs
   */
  public void write (String str, int offset, int count) throws IOException
  {
    if(str == null)
      throw new IOException("String is null.");

    write(str.toCharArray(), offset, count);
  }

  /**
   * This method writes a single character to the output stream.
   *
   * @param ch The char to write, passed as an int.
   *
   * @exception IOException If an error occurs
   */
  public void write (int ch) throws IOException
  {
    write(new char[]{ (char)ch }, 0, 1);
  }
} // class OutputStreamWriter

