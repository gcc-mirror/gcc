/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 2, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public interface DataInput
{
  public boolean readBoolean() throws IOException;
  public byte readByte() throws IOException;
  public char readChar() throws IOException;
  public double readDouble() throws IOException;
  public float readFloat() throws IOException;
  public void readFully(byte[] b)
    throws IOException, NullPointerException;
  public void readFully(byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException;
  public int readInt() throws IOException;
  public String readLine() throws IOException;
  public long readLong() throws IOException;
  public short readShort() throws IOException;
  public int readUnsignedByte() throws IOException;
  public int readUnsignedShort() throws IOException;
  public String readUTF() throws IOException;
  public int skipBytes(int n) throws IOException;
}
