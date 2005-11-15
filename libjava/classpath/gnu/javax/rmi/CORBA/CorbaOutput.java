/* CorbaOutput.java --
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

import org.omg.CORBA_2_3.portable.OutputStream;

import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * A class to substitute as an ObjectOutputStream for objects using writeObject
 * method.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class CorbaOutput
  extends ObjectOutputStream
  implements ObjectOutput
{
  /**
   * A CORBA stream where the output is forwarded.
   */
  final OutputStream stream;

  /**
   * The utility class to write the object fields in default way.
   */
  final RmiUtilities util;

  /**
   * The object currently being written.
   */
  Object current;

  /**
   * Create an instance, delegating calls to the given CORBA stream.
   */
  public CorbaOutput(OutputStream an_output, Object firstObject,
                           RmiUtilities an_util)
    throws Exception
  {
    stream = an_output;
    current = firstObject;
    util = an_util;
  }

  /**
   * No action.
   */
  public void close()
    throws IOException
  {
  }

  /** @inheritDoc */
  public void flush()
    throws IOException
  {
    stream.flush();
  }

  /** @inheritDoc */
  public void write(byte[] buf, int off, int len)
    throws IOException
  {
    stream.write(buf, off, len);
  }

  /** @inheritDoc */
  public void write(byte[] buf)
    throws IOException
  {
    stream.write(buf);
  }

  /** @inheritDoc */
  public void write(int val)
    throws IOException
  {
    stream.write(val);
  }

  /** @inheritDoc */
  public void writeBoolean(boolean val)
    throws IOException
  {
    stream.write_boolean(val);
  }

  /** @inheritDoc */
  public void writeByte(int val)
    throws IOException
  {
    stream.write(val);
  }

  /** @inheritDoc */
  public void writeBytes(String str)
    throws IOException
  {
    stream.write_string(str);
  }

  /** @inheritDoc */
  public void writeChar(int val)
    throws IOException
  {
    stream.write_wchar((char) val);
  }

  /** @inheritDoc */
  public void writeChars(String str)
    throws IOException
  {
    stream.write_char_array(str.toCharArray(), 0, str.length());
  }

  /** @inheritDoc */
  public void writeDouble(double val)
    throws IOException
  {
    stream.write_double(val);
  }

  /** @inheritDoc */
  public void writeFloat(float val)
    throws IOException
  {
    stream.write_float(val);
  }

  /** @inheritDoc */
  public void writeInt(int val)
    throws IOException
  {
    stream.write_long(val);
  }

  /** @inheritDoc */
  public void writeLong(long val)
    throws IOException
  {
    stream.write_longlong(val);
  }

  /**
   * Objects are written as abstract interfaces.
   */
  protected void writeObjectOverride(Object obj)
    throws IOException
  {
    current = obj;
    stream.write_abstract_interface(obj);
  }

  /** @inheritDoc */
  public void writeShort(int val)
    throws IOException
  {
    stream.write_short((short) val);
  }

  /**
   * Such strings are written as wide strings, not as UTF.
   */
  public void writeUTF(String str)
    throws IOException
  {
    stream.write_wstring(str);
  }

  /**
   * @inheritDoc
   */
  public void defaultWriteObject()
    throws IOException
  {
    util.writeFields(stream, (Serializable) current);
  }

}
