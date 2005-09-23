/* noHeaderInput.java --
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


package gnu.CORBA.CDR;

import org.omg.CORBA.CustomMarshal;
import org.omg.CORBA.DataInputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.Streamable;
import org.omg.CORBA.portable.ValueFactory;

import java.io.Serializable;

/**
 * Substitutes the main stream in factories when the header is already
 * behind. Overrides methods that may be invoked from the factory,
 * forcing not to read the header if called first time on this stream.
 *
 * This stream reverts to default behavior if one or more call are
 * made (reading value types that are nested fields of the value type).
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
class noHeaderInput
  extends cdrBufInput
  implements DataInputStream
{
  /**
   * If true, this is not the first call.
   */
  boolean notFirst;

  /**
   * Create an instance, reading from the given buffer.
   */
  public noHeaderInput(byte[] buffer)
  {
    super(buffer);
  }

  /**
   * Read when knowning the class instance.
   */
  public Serializable read_value(Class clz)
  {
    if (notFirst)
      return super.read_value(clz);
    else
      {
        try
          {
            notFirst = true;
            return read_value((Serializable) clz.newInstance());
          }
        catch (Exception ex)
          {
            MARSHAL m = new MARSHAL("Unable to create an instance");
            m.initCause(ex);
            throw m;
          }
      }
  }

  /**
   * Tries to read using boxed value helper.
   */
  public Serializable read_value(BoxedValueHelper helper)
  {
    if (notFirst)
      return super.read_value(helper);
    else
      {
        notFirst = true;
        return helper.read_value(this);
      }
  }

  /**
   * Tries to locate a factory using repository id.
   */
  public Serializable read_value(String repository_id)
  {
    if (notFirst)
      return super.read_value(repository_id);
    else
      {
        notFirst = true;

        ValueFactory factory =
          ((org.omg.CORBA_2_3.ORB) orb()).lookup_value_factory(repository_id);
        if (factory == null)
          throw new MARSHAL("No factory");
        return factory.read_value(this);
      }
  }

  /**
   * Try to read when having an unitialised value.
   */
  public Serializable read_value(Serializable value)
  {
    if (notFirst)
      return super.read_value(value);
    else
      {
        notFirst = true;

        // The user-defines io operations are implemented.
        if (value instanceof CustomMarshal)
          {
            CustomMarshal marsh = (CustomMarshal) value;
            try
              {
                marsh.unmarshal((DataInputStream) this);
              }
            catch (ClassCastException ex)
              {
                Vio.incorrect_plug_in(ex);
              }
          }
        else
        // The IDL-generated io operations are implemented.
        if (value instanceof Streamable)
          {
            ((Streamable) value)._read(this);
          }
        return value;
      }
  }
}