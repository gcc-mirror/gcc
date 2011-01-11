/* ValueHandlerDelegateImpl.java --
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

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CustomMarshal;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;
import org.omg.SendingContext.RunTime;

import java.io.Externalizable;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.rmi.Remote;

import javax.rmi.CORBA.ValueHandler;
import javax.rmi.CORBA.ValueHandlerMultiFormat;

/**
 * Implementation of the ValueHandler.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) (implementation)
 */
public class ValueHandlerDelegateImpl
  extends RmiUtilities
  implements ValueHandler, ValueHandlerMultiFormat
{
  /**
   * Return the maximal supported stream format version. We currently
   * support the version 1.
   *
   * TODO Support the version 2.
   */
  public byte getMaximumStreamFormatVersion()
  {
    return 1;
  }

  /**
   * Write value using the given stream format version.
   */
  public void writeValue(OutputStream output, Serializable value, byte version)
  {
    if (version!=1)
      throw new BAD_PARAM("Unsupported stream format version "+version);
    else
      writeValue(output, value);
  }

  /**
   * This implementation associates RunTime with stream rather than with the
   * value handler and this method is not used in the implementation. It is
   * implemented just for the sake of compatibility.
   */
  public RunTime getRunTimeCodeBase()
  {
    return new gnuRuntime(null, null);
  }

  /**
   * Checks if an instance of this class can write its fields itself.
   */
  public boolean isCustomMarshaled(Class clz)
  {
    return CustomMarshal.class.isAssignableFrom(clz)
      || Streamable.class.isAssignableFrom(clz);
  }

  /**
   * No replacement, returns the passed parameter.
   */
  public Serializable writeReplace(Serializable value)
  {
    return value;
  }

  /**
   * Compute the repository id in the RMI hashed format.
   */
  public String getRMIRepositoryID(final Class cx)
  {
    long hash = 0;
    Class of = cx.isArray() ? cx.getComponentType() : null;

    if (cx.equals(String[].class))
      return RMI_STRING_ARRAY_ID;
    else if (cx.equals(String.class))
      return RMI_STRING_ID;
    else if (cx.equals(Class.class))
      return RMI_CLASS_ID;
    else if (Remote.class.isAssignableFrom(cx)
      || !Serializable.class.isAssignableFrom(cx)
      || cx.isInterface()
      || (cx.isArray() && (!Serializable.class.isAssignableFrom(of)
        || of.isPrimitive() || Remote.class.isAssignableFrom(of)))

    )
      // Some classes that have zero hash code and serial no version id
      // included.
      return "RMI:" + cx.getName() + ":" + toHex(hash);
    else if (cx.isArray())
      // Arrays have the same hashcode and uid as they components.
      return "RMI:" + cx.getName() + ":" + toHex(getHashCode(of)) + ":"
        + toHex(getSid(of));
    else
      {
        if (Externalizable.class.isAssignableFrom(cx))
          hash = 1;
        else
          hash = getHashCode(cx);

        return "RMI:" + cx.getName() + ":" + toHex(hash) + ":"
          + toHex(getSid(cx));
      }
  }

  /**
   * Get the class serial version UID.
   */
  long getSid(Class cx)
  {
    ObjectStreamClass osc = ObjectStreamClass.lookup(cx);
    return osc.getSerialVersionUID();
  }
}
