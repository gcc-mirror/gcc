/* UnknownExceptionCtxHandler.java --
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

import gnu.CORBA.Minor;
import gnu.CORBA.ObjectCreator;
import gnu.CORBA.GIOP.ServiceContext;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.StringValueHelper;
import org.omg.CORBA.portable.OutputStream;

import java.lang.reflect.Constructor;
import java.util.StringTokenizer;

import javax.rmi.CORBA.Util;

/**
 * Reads the data about an unknown exception from the UnknownExceptionInfo.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class UnknownExceptionCtxHandler
  extends Vio
{
  /**
   * Encode exception and add its recored to the message service contexts.
   */
  public static ServiceContext[] addExceptionContext(ServiceContext[] current,
    Throwable exception, Object details)
  {
    try
      {
        ServiceContext[] c = new ServiceContext[current.length + 1];
        if (current.length > 0)
          System.arraycopy(current, 0, c, 0, current.length);

        BufferedCdrOutput output = new BufferedCdrOutput();

        if (details instanceof OutputStream)
          output.setOrb(((OutputStream) output).orb());

        if (details instanceof AbstractCdrOutput)
          ((AbstractCdrOutput) details).cloneSettings(output);

        write(output, exception);

        ServiceContext xc = new ServiceContext();
        xc.context_id = ServiceContext.UnknownExceptionInfo;
        xc.context_data = output.buffer.toByteArray();
        c[current.length] = xc;
        return c;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        return current;
      }
  }

  /**
   * Write data about unknown exception.
   */
  public static void write(BufferedCdrOutput output, Throwable t)
  {
    t.fillInStackTrace();
    output.write_Value(t);
  }

  /**
   * Read the data about an unknown exception from the UnknownExceptionInfo.
   * Following the documentation, this must be just value type, but it seems
   * that in Sun's implementation is is not, as starts from 0x0. For value type,
   * this would be null.
   *
   * TODO Implement reading and writing in Sun format, making Classpath IIOP
   * interoperable with Sun's implementation. Current inmplementation reads and
   * reproduces the exception class type only.
   *
   * @param input the input stream to read the context (orb and other settings
   * are inherited from the main stream that received the message).
   *
   * @param contexts all service contexts that were present in the message.
   *
   * @return the Throwable, extracted from context, on null, if this has failed.
   */
  public static Throwable read(BufferredCdrInput input, ServiceContext[] contexts)
  {
    input.mark(Integer.MAX_VALUE);

    int h = input.read_long();
    if (h == 0)
      {
        // This block reads exception info in the Sun specific format.
        // (currently we read the exception name only).
        try
          {
            // We may need to jump back if the value is read via value
            // factory.
            input.mark(512);

            int value_tag = input.read_long();
            checkTag(value_tag);

            String codebase = null;
            String[] ids = null;
            String id = null;

            // Check for the agreed null value.
            if (value_tag == vt_NULL)
              return null;
            else if (value_tag == vt_INDIRECTION)
              return (Throwable) readIndirection(input);
            else
              {
                // Read the value.
                if ((value_tag & vf_CODEBASE) != 0)
                  {
                    // The codebase is present. The codebase is a space
                    // separated list of URLs from where the implementing
                    // code can be downloaded.
                    codebase = read_string(input);
                  }

                if ((value_tag & vf_MULTIPLE_IDS) != 0)
                  {
                    // Multiple supported repository ids are present.
                    ids = read_string_array(input);
                  }
                else if ((value_tag & vf_ID) != 0)
                  {
                    // Single supported repository id is present.
                    id = read_string(input);
                  }
              }

            java.lang.Object ox = createInstance(id, ids, codebase);

            return (Throwable) ox;
          }
        catch (Exception ex)
          {
            ex.printStackTrace();
            return null;
          }
      }
    else
      {
        input.reset();
        // Read as defined in OMG documentation.
        return (Throwable) input.read_Value();
      }
  }

  /**
   * Load exception by name and create the instance. The reason why this is
   * different from Vio is because some exceptions have no parameterless
   * constructor, but have a constructor with the string parameter instead.
   */
  static Object createInstance(String id, String[] ids, String codebase)
  {
    Object o = _createInstance(id, codebase);

    if (ids != null)
      for (int i = 0; i < ids.length && o == null; i++)
        o = _createInstance(ids[i], codebase);
    return o;
  }

  static Object _createInstance(String id, String codebase)
  {
    if (id == null)
      return null;
    if (id.equals(StringValueHelper.id()))
      return "";
    StringTokenizer st = new StringTokenizer(id, ":");

    String prefix = st.nextToken();
    if (prefix.equalsIgnoreCase("IDL"))
      return ObjectCreator.Idl2Object(id);
    else if (prefix.equalsIgnoreCase("RMI"))
      {
        String className = st.nextToken();
        String hashCode = st.nextToken();
        String sid = null;
        if (st.hasMoreElements())
          sid = st.nextToken();

        try
          {
            Class objectClass = Util.loadClass(className, codebase,
              Vio.class.getClassLoader());

            String rid = ObjectCreator.getRepositoryId(objectClass);

            if (!rid.equals(id))
              {
                // If direct string comparison fails, compare by meaning.
                StringTokenizer st2 = new StringTokenizer(rid, ":");
                if (!st2.nextToken().equals("RMI"))
                  throw new InternalError("RMI format expected: '" + rid + "'");
                if (!st2.nextToken().equals(className))
                  throwIt("Class name mismatch", id, rid, null);

                try
                  {
                    long h1 = Long.parseLong(hashCode, 16);
                    long h2 = Long.parseLong(st2.nextToken(), 16);
                    if (h1 != h2)
                      throwIt("Hashcode mismatch", id, rid, null);

                    if (sid != null && st2.hasMoreTokens())
                      {
                        long s1 = Long.parseLong(hashCode, 16);
                        long s2 = Long.parseLong(st2.nextToken(), 16);
                        if (s1 != s2)
                          throwIt("serialVersionUID mismatch", id, rid, null);
                      }
                  }
                catch (NumberFormatException e)
                  {
                    throwIt("Invalid hashcode or svuid format: ", id, rid, e);
                  }
              }

            // Some RemoteExceptions have no public parameterless constructor,
            // but they have constructor taking string as parameter.
            try
              {
                return objectClass.newInstance();
              }
            catch (Exception ex)
              {
                // Try instantiate passing string as parameter.
                Constructor c = objectClass.getConstructor(new Class[] { String.class });
                return c.newInstance(new Object[] { "<message unavailable>" });
              }
          }
        catch (MARSHAL m)
          {
            m.minor = Minor.Instantiation;
            throw m;
          }
        catch (Exception ex)
          {
            MARSHAL m = new MARSHAL("Unable to instantiate " + id);
            m.minor = Minor.Instantiation;
            m.initCause(ex);
            throw m;
          }
      }
    else
      throw new NO_IMPLEMENT("Unsupported prefix " + prefix + ":");
  }
}
