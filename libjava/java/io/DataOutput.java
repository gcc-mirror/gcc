// DataOutput.java - Interface for data output conversions.

/* Copyright (C) 1998, 1999  Free Software Foundation

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

public interface DataOutput
{
  public abstract void write (int b) throws IOException;
  public abstract void write (byte[] b)
    throws IOException, NullPointerException;
  public abstract void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException;
  public abstract void writeBoolean (boolean v) throws IOException;
  public abstract void writeByte (int v) throws IOException;
  public abstract void writeShort (int v) throws IOException;
  public abstract void writeChar (int v) throws IOException;
  public abstract void writeInt (int v) throws IOException;
  public abstract void writeLong (long v) throws IOException;
  public abstract void writeFloat (float v) throws IOException;
  public abstract void writeDouble (double v) throws IOException;
  public abstract void writeBytes (String s)
    throws IOException, NullPointerException;
  public abstract void writeChars (String s)
    throws IOException, NullPointerException;
  public abstract void writeUTF (String s)
    throws IOException, NullPointerException;
}
