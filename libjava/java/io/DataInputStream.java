/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 20, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class DataInputStream extends FilterInputStream implements DataInput
{
  // readLine() hack to ensure that an '\r' not followed by an '\n' is
  // handled correctly. If set, readLine() will ignore the first char it sees
  // if that char is a '\n'
  boolean ignoreInitialNewline = false;
  
  public DataInputStream(InputStream in)
  {
    super(in);
  }

  public final int read(byte[] b) throws IOException
  {
    return super.read(b, 0, b.length);
  }

  public final int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    return super.read(b, off, len);
  }

  public final boolean readBoolean() throws IOException
  {
    return (readByte() != 0);
  }

  public final byte readByte() throws IOException
  {
    int i = read();
    if (i < 0)
      throw new EOFException();

    return (byte) i;
  }

  public final char readChar() throws IOException
  {
    return (char) ((readByte() << 8) | readUnsignedByte());
  }

  public final double readDouble() throws IOException
  {
    return Double.longBitsToDouble(readLong());
  }

  public final float readFloat() throws IOException
  {
    return Float.intBitsToFloat(readInt());
  }

  public final void readFully(byte[] b) throws IOException
  {
    readFully(b, 0, b.length);
  }

  public final void readFully(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    while (len > 0)
      {
	// super.read will block until some data is available.
	int numread = super.read(b, off, len);
	if (numread < 0)
	  throw new EOFException();
	len -= numread;
	off += numread;
      }
  }

  public final int readInt() throws IOException
  {
    int retval = 0;
    for (int i = 0; i < 4; i++)
      retval |= readUnsignedByte() << (24 - i * 8);

    return retval;
  }

  // Deprecated as of JDK 1.1
  public final String readLine() throws IOException
  {
    StringBuffer strb = new StringBuffer();

    readloop: while (true)
      {
        int c = 0;
        char ch = ' ';
        boolean getnext = true;
        while (getnext)
          {
	    getnext = false;
	    c = read();
	    if (c < 0)	// got an EOF
	      return strb.length() > 0 ? strb.toString() : null;
	    ch = (char) c;
	    if ((ch &= 0xFF) == '\n')
	      // hack to correctly handle '\r\n' sequences
	      if (ignoreInitialNewline)
		{
		  ignoreInitialNewline = false;
		  getnext = true;
		}
	      else
		break readloop;
	  }

	if (ch == '\r')
	  {
	    // FIXME: The following code tries to adjust the stream back one
	    // character if the next char read is '\n'.  As a last resort,
	    // it tries to mark the position before reading but the bottom
	    // line is that it is possible that this method will not properly
	    // deal with a '\r' '\n' combination thus not fulfilling the
	    // DataInput contract for readLine.  It's not a particularly
	    // safe approach threadwise since it is unsynchronized and
	    // since it might mark an input stream behind the users back.
	    // Along the same vein it could try the same thing for
	    // ByteArrayInputStream and PushbackInputStream, but that is
	    // probably overkill since this is deprecated & BufferedInputStream
	    // is the most likely type of input stream.
	    //
	    // The alternative is to somehow push back the next byte if it
	    // isn't a '\n' or to have the reading methods of this class
	    // keep track of whether the last byte read was '\r' by readLine
	    // and then skip the very next byte if it is '\n'.  Either way,
	    // this would increase the complexity of the non-deprecated methods
	    // and since it is undesirable to make non-deprecated methods
	    // less efficient, the following seems like the most reasonable
	    // approach.
	    int next_c = 0;
            char next_ch = ' ';
	    if (in instanceof BufferedInputStream)
	      {
	        next_c = read();
	        next_ch = (char) (next_c & 0xFF);
		if ((next_ch != '\n') && (next_c >= 0)) 
		  {
	            BufferedInputStream bin = (BufferedInputStream) in;
		    if (bin.pos > 0)
                      bin.pos--;
		  }
	      }
	    else if (markSupported())
	      {
	        next_c = read();
	        next_ch = (char) (next_c & 0xFF);
		if ((next_ch != '\n') && (next_c >= 0)) 
		  {
		    mark(1);
		    if ((read() & 0xFF) != '\n')
		      reset();
		  }
	      } 
	    // In order to catch cases where 'in' isn't a BufferedInputStream
	    // and doesn't support mark() (such as reading from a Socket), set 
	    // a flag that instructs readLine() to ignore the first character 
	    // it sees _if_ that character is a '\n'.
	    else ignoreInitialNewline = true;
	    break;
	  }
	strb.append(ch);
      }

    return strb.length() > 0 ? strb.toString() : "";
  }

  public final long readLong() throws IOException
  {
    long retval = 0L;
    for (int i = 0; i < 8; i++)
      retval |= (long) readUnsignedByte() << (56 - i * 8);

    return retval;
  }

  public final short readShort() throws IOException
  {
    return (short) ((readByte() << 8) | readUnsignedByte());
  }

  public final int readUnsignedByte() throws IOException
  {
    int i = read();
    if (i < 0)
      throw new EOFException();

    return (i & 0xFF);
  }

  public final int readUnsignedShort() throws IOException
  {
    return (readUnsignedByte() << 8) | readUnsignedByte();
  }

  public final String readUTF() throws IOException
  {
    return readUTF(this);
  }

  public final static String readUTF(DataInput in) throws IOException
  {
    final int UTFlen = in.readUnsignedShort();
    byte[] buf = new byte[UTFlen];
    StringBuffer strbuf = new StringBuffer();

    // This blocks until the entire string is available rather than
    // doing partial processing on the bytes that are available and then
    // blocking.  An advantage of the latter is that Exceptions
    // could be thrown earlier.  The former is a bit cleaner.
    in.readFully(buf, 0, UTFlen);
    for (int i = 0; i < UTFlen; )
      {
	if ((buf[i] & 0x80) == 0)		// bit pattern 0xxxxxxx
	  strbuf.append((char) (buf[i++] & 0xFF));
	else if ((buf[i] & 0xE0) == 0xC0)	// bit pattern 110xxxxx
	  {
	    if (i + 1 >= UTFlen || (buf[i+1] & 0xC0) != 0x80)
	      throw new UTFDataFormatException();

	    strbuf.append((char) (((buf[i++] & 0x1F) << 6) |
				  (buf[i++] & 0x3F)));
	  }
	else if ((buf[i] & 0xF0) == 0xE0)	// bit pattern 1110xxxx
	  {
	    if (i + 2 >= UTFlen ||
		(buf[i+1] & 0xC0) != 0x80 || (buf[i+2] & 0xC0) != 0x80)
	      throw new UTFDataFormatException();

	    strbuf.append((char) (((buf[i++] & 0x0F) << 12) |
				  ((buf[i++] & 0x3F) << 6) |
				  (buf[i++] & 0x3F)));
	  }
	else // must be ((buf[i] & 0xF0) == 0xF0 || (buf[i] & 0xC0) == 0x80)
	  throw new UTFDataFormatException();	// bit patterns 1111xxxx or
						// 		10xxxxxx
      }

    return strbuf.toString();
  }

  public final int skipBytes(int n) throws IOException
  {
    // The contract in the Java Lang. Spec. says that this never
    // throws an EOFException and infers that it doesn't block (since
    // it may skip less than the requested number of bytes).
    // BUT, the JCL book specifically says that this method blocks
    // and can throw an EOFException.  Finally, the Java 1.2 online
    // doc simply refers to the general contract.  As such, we will
    // stick to the contract and assume for now that the JCL book
    // is incorrect.

    // Since we're only skipping at most an int number of bytes, the cast
    // of return value to an int is fine.
    if (n > 0)
      {
	n = Math.min(n, available());
        return (int) super.skip((long) n);
      }

    return 0;
  }
}
