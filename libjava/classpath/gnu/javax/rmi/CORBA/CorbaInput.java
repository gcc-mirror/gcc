/* CorbaInput.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.javax.rmi.CORBA;

import gnu.CORBA.CDR.gnuRuntime;

import org.omg.CORBA_2_3.portable.InputStream;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.Serializable;

/**
 * Converts calls on java ObjectOutputStream to calls on CORBA OutputStream. A
 * class to substitute for objects using readObject method.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class CorbaInput
  extends ObjectInputStream
  implements ObjectInput
{

  /**
   * The underlying CORBA stream from where the actual input is taken.
   */
  public InputStream stream;

  /**
   * The utility class to write the object fields in default way.
   */
  final RmiUtilities util;

  /**
   * The object currently being read.
   */
  Object current;

  /**
   * The offset of the object currently being read.
   */
  int offset;

  /**
   * The repository id of the object currently being read.
   */
  String rid;

  /**
   * The runtime, related to the object currently being read.
   */
  gnuRuntime runtime;

  /**
   * Create an instance, delegating calls to the given CORBA stream.
   */
  public CorbaInput(InputStream an_input, Object firstObject,
                          RmiUtilities an_util, int an_offset, String a_rid,
                          gnuRuntime a_runtime)
    throws Exception
  {
    stream = an_input;
    current = firstObject;
    util = an_util;

    offset = an_offset;
    rid = a_rid;
    runtime = a_runtime;
  }

  /** @inheritDoc */
  public int available()
    throws IOException
  {
    return stream.available();
  }

  /**
   * No action.
   */
  public void close()
    throws IOException
  {
  }

  /** @inheritDoc */
  public void defaultReadObject()
    throws IOException, ClassNotFoundException
  {
    util.readFields(offset, rid, (Serializable) current, stream, runtime);
  }

  /** @inheritDoc */
  public void mark(int readlimit)
  {
    stream.mark(readlimit);
  }

  /** @inheritDoc */
  public boolean markSupported()
  {
    return stream.markSupported();
  }

  /** @inheritDoc */
  public int read()
    throws IOException
  {
    return stream.read();
  }

  /** @inheritDoc */
  public int read(byte[] buf, int off, int len)
    throws IOException
  {
    return stream.read(buf, off, len);
  }

  /** @inheritDoc */
  public int read(byte[] b)
    throws IOException
  {
    return stream.read(b);
  }

  /** @inheritDoc */
  public boolean readBoolean()
    throws IOException
  {
    return stream.read_boolean();
  }

  /** @inheritDoc */
  public byte readByte()
    throws IOException
  {
    return (byte) stream.read();
  }

  /** @inheritDoc */
  public char readChar()
    throws IOException
  {
    return stream.read_char();
  }

  /** @inheritDoc */
  public double readDouble()
    throws IOException
  {
    return stream.read_double();
  }

  /** @inheritDoc */
  public float readFloat()
    throws IOException
  {
    return stream.read_float();
  }

  /** @inheritDoc */
  public void readFully(byte[] buf, int off, int len)
    throws IOException
  {
    // This class only reads from the buffered streams.
    stream.read(buf, off, len);
  }

  /** @inheritDoc */
  public void readFully(byte[] buf)
    throws IOException
  {
    // This class only reads from the buffered streams.
    stream.read(buf);
  }

  /** @inheritDoc */
  public int readInt()
    throws IOException
  {
    return stream.read_long();
  }

  /** @inheritDoc */
  public String readLine()
    throws IOException
  {
    return new DataInputStream(this).readLine();
  }

  /** @inheritDoc */
  public long readLong()
    throws IOException
  {
    return stream.read_longlong();
  }

  /** @inheritDoc */
  public short read_short()
    throws IOException
  {
    return stream.read_short();
  }

  /** @inheritDoc */
  public int readUnsignedByte()
    throws IOException
  {
    return (stream.read() & 0xFF);
  }

  /** @inheritDoc */
  public int readUnsignedShort()
    throws IOException
  {
    return (stream.read_short() & 0xFFFF);
  }

  /**
   * Read as wide string (not as UTF).
   */
  public String readUTF()
    throws IOException
  {
    return stream.read_wstring();
  }

  /** @inheritDoc */
  public void reset()
    throws IOException
  {
    stream.reset();
  }

  /** @inheritDoc */
  public long skip(long n)
    throws IOException
  {
    return stream.skip(n);
  }

  /** @inheritDoc */
  public int skipBytes(int len)
    throws IOException
  {
    return (int) stream.skip(len);
  }

  /**
   * Objects are read as abstract interfaces.
   */
  protected Object readObjectOverride()
    throws IOException, ClassNotFoundException
  {
    current = stream.read_abstract_interface();
    return current;
  }

}
