// DataOutputStream.java - Output filter that implements DataOutput

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

public class DataOutputStream extends FilterOutputStream implements DataOutput
{
  public DataOutputStream (OutputStream out)
  {
    super (out);
    written = 0;
  }

  public void flush () throws IOException
  {
    out.flush();
  }

  public final int size ()
  {
    return written;
  }

  public synchronized void write (int b) throws IOException
  {
    out.write(b);
    ++written;
  }

  public synchronized void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    out.write(b, off, len);
    written += len - off;
  }

  public final void writeBoolean (boolean v) throws IOException
  {
    write (v ? 1 : 0);
  }

  public final void writeByte (int v) throws IOException
  {
    write (v & 0xff);
  }

  public final void writeShort (int v) throws IOException
  {
    write ((byte) (0xff & (v >> 8)));
    write ((byte) (0xff & v));
  }

  public final void writeChar (int v) throws IOException
  {
    write ((byte) (0xff & (v >> 8)));
    write ((byte) (0xff & v));
  }

  public final void writeInt (int v) throws IOException
  {
    write ((byte) (0xff & (v >> 24)));
    write ((byte) (0xff & (v >> 16)));
    write ((byte) (0xff & (v >>  8)));
    write ((byte) (0xff & v));
  }

  public final void writeLong (long v) throws IOException
  {
    write ((byte) (0xff & (v >> 56)));
    write ((byte) (0xff & (v >> 48)));
    write ((byte) (0xff & (v >> 40)));
    write ((byte) (0xff & (v >> 32)));
    write ((byte) (0xff & (v >> 24)));
    write ((byte) (0xff & (v >> 16)));
    write ((byte) (0xff & (v >>  8)));
    write ((byte) (0xff & v));
  }

  public final void writeFloat (float v) throws IOException
  {
    writeInt (Float.floatToIntBits(v));
  }

  public final void writeDouble (double v) throws IOException
  {
    writeLong (Double.doubleToLongBits(v));
  }

  public final void writeBytes (String s) throws IOException
  {
    int len = s.length();
    for (int i = 0; i < len; ++i)
      writeByte (s.charAt(i));
  }

  public final void writeChars (String s) throws IOException
  {
    int len = s.length();
    for (int i = 0; i < len; ++i)
      writeChar (s.charAt(i));
  }

  public final void writeUTF (String s) throws IOException
  {
    int len = s.length();
    int sum = 0;

    for (int i = 0; i < len && sum <= 65535; ++i)
      {
	char c = s.charAt(i);
	if (c >= '\u0001' && c <= '\u007f')
	  sum += 1;
	else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07ff'))
	  sum += 2;
	else
	  sum += 3;
      }

    if (sum > 65535)
      throw new UTFDataFormatException ();

    writeShort (sum);

    for (int i = 0; i < len; ++i)
      {
	char c = s.charAt(i);
	if (c >= '\u0001' && c <= '\u007f')
	  write (c);
	else if (c == '\u0000' || (c >= '\u0080' && c <= '\u07ff'))
	  {
	    write (0xc0 | (0x1f & (c >> 6)));
	    write (0x80 | (0x3f & c));
	  }
	else
	  {
	    // JSL says the first byte should be or'd with 0xc0, but
	    // that is a typo.  Unicode says 0xe0, and that is what is
	    // consistent with DataInputStream.
	    write (0xe0 | (0x0f & (c >> 12)));
	    write (0x80 | (0x3f & (c >> 6)));
	    write (0x80 | (0x3f & c));
	  }
      }
  }

  // Number of bytes written so far.
  protected int written;
}
