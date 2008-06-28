/* ObjectCreator.java --
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


package gnu.CORBA;

import gnu.CORBA.CDR.UnknownExceptionCtxHandler;
import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.CDR.BufferedCdrOutput;
import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.GIOP.ServiceContext;
import gnu.CORBA.typecodes.RecordTypeCode;
import gnu.classpath.VMStackWalker;

import org.omg.CORBA.Any;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.CompletionStatusHelper;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.UNKNOWN;
import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ValueBase;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.WeakHashMap;

import javax.rmi.CORBA.Util;

/**
 * Creates java objects from the agreed IDL names for the simple case when the
 * CORBA object is directly mapped into the locally defined java class.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ObjectCreator
{
  /**
   * The standard OMG prefix.
   */
  public static final String OMG_PREFIX = "omg.org/";

  /**
   * The standard java prefix.
   */
  public static final String JAVA_PREFIX = "org.omg.";

  /**
   * The prefix for classes that are placed instide the gnu.CORBA namespace.
   */
  public static final String CLASSPATH_PREFIX = "gnu.CORBA.";

  /**
   * Maps classes to they IDL or RMI names. Computing RMI name is an expensive
   * operations, so frequently used RMI keys are reused. The map must be weak to
   * ensure that the class can be unloaded, when applicable.
   */
  public static Map m_names = new WeakHashMap();

  /**
   * Maps IDL strings into known classes. The map must be weak to ensure that
   * the class can be unloaded, when applicable.
   */
  public static Map m_classes = new WeakHashMap();

  /**
   * Maps IDL types to they helpers.
   */
  public static Map m_helpers = new WeakHashMap();

  /**
   * Try to instantiate an object with the given IDL name. The object must be
   * mapped to the local java class. The omg.org domain must be mapped into the
   * object in either org/omg or gnu/CORBA namespace.
   * 
   * @param idl name
   * @return instantiated object instance or null if no such available.
   */
  public static java.lang.Object createObject(String idl, String suffix)
  {
    synchronized (m_classes)
      {
        Class known = (Class) (suffix == null ? m_classes.get(idl)
          : m_classes.get(idl + 0xff + suffix));
        Object object;

        if (known != null)
          {
            try
              {
                return known.newInstance();
              }
            catch (Exception ex)
              {
                RuntimeException rex = new RuntimeException(idl + " suffix "
                  + suffix, ex);
                throw rex;
              }
          }
        else
          {
            if (suffix == null)
              suffix = "";
            try
              {
                known = forName(toClassName(JAVA_PREFIX, idl) + suffix);
                object = known.newInstance();
              }
            catch (Exception ex)
              {
                try
                  {
                    known = forName(toClassName(CLASSPATH_PREFIX, idl)
                      + suffix);
                    object = known.newInstance();
                  }
                catch (Exception exex)
                  {
                    return null;
                  }
              }
            m_classes.put(idl + 0xff + suffix, known);
            return object;
          }
      }
  }

  /**
   * Read the system exception from the given stream.
   * 
   * @param input the CDR stream to read from.
   * @param contexts the service contexts in request/reply header/
   * 
   * @return the exception that has been stored in the stream (IDL name, minor
   * code and completion status).
   */
  public static SystemException readSystemException(InputStream input,
    ServiceContext[] contexts)
  {
    SystemException exception;

    String idl = input.read_string();
    int minor = input.read_ulong();
    CompletionStatus completed = CompletionStatusHelper.read(input);

    try
      {
        exception = (SystemException) createObject(idl, null);
        exception.minor = minor;
        exception.completed = completed;
      }
    catch (Exception ex)
      {
        UNKNOWN u = new UNKNOWN("Unsupported system exception " + idl, minor,
          completed);
        u.initCause(ex);
        throw u;
      }

    try
      {
        // If UnknownExceptionInfo is present in the contexts, read it and
        // set as a cause of this exception.
        ServiceContext uEx = ServiceContext.find(
          ServiceContext.UnknownExceptionInfo, contexts);

        if (uEx != null)
          {
            BufferredCdrInput in = new BufferredCdrInput(uEx.context_data);
            in.setOrb(in.orb());
            if (input instanceof AbstractCdrInput)
              {
                ((AbstractCdrInput) input).cloneSettings(in);
              }

            Throwable t = UnknownExceptionCtxHandler.read(in, contexts);
            exception.initCause(t);
          }
      }
    catch (Exception ex)
      {
        // Unsupported context format. Do not terminate as the user program may
        // not need it.
      }

    return exception;
  }

  /**
   * Reads the user exception, having the given Id, from the input stream. The
   * id is expected to be in the form like
   * 'IDL:test/org/omg/CORBA/ORB/communication/ourUserException:1.0'
   * 
   * @param idl the exception idl name.
   * @param input the stream to read from.
   * 
   * @return the loaded exception.
   * @return null if the helper class cannot be found.
   */
  public static UserException readUserException(String idl, InputStream input)
  {
    try
      {
        Class helperClass = findHelper(idl);

        Method read = helperClass.getMethod("read",
          new Class[] { org.omg.CORBA.portable.InputStream.class });

        return (UserException) read.invoke(null, new Object[] { input });
      }
    catch (MARSHAL mex)
      {
        // This one is ok to throw
        throw mex;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        return null;
      }
  }

  /**
   * Gets the helper class name from the string like
   * 'IDL:test/org/omg/CORBA/ORB/communication/ourUserException:1.0'
   * 
   * @param IDL the idl name.
   */
  public static String toHelperName(String IDL)
  {
    String s = IDL;
    int a = s.indexOf(':') + 1;
    int b = s.lastIndexOf(':');

    s = IDL.substring(a, b);

    if (s.startsWith(OMG_PREFIX))
      s = JAVA_PREFIX + s.substring(OMG_PREFIX.length());

    return s.replace('/', '.') + "Helper";
  }

  /**
   * Writes the system exception data to CDR output stream.
   * 
   * @param output a stream to write data to.
   * @param ex an exception to write.
   */
  public static void writeSystemException(OutputStream output,
    SystemException ex)
  {
    String exIDL = getRepositoryId(ex.getClass());
    output.write_string(exIDL);
    output.write_ulong(ex.minor);
    CompletionStatusHelper.write(output, ex.completed);
  }

  /**
   * Converts the given IDL name to class name.
   * 
   * @param IDL the idl name.
   * 
   */
  protected static String toClassName(String prefix, String IDL)
  {
    String s = IDL;
    int a = s.indexOf(':') + 1;
    int b = s.lastIndexOf(':');

    s = IDL.substring(a, b);

    if (s.startsWith(OMG_PREFIX))
      s = prefix + s.substring(OMG_PREFIX.length());

    return s.replace('/', '.');
  }

  /**
   * Converts the given IDL name to class name and tries to load the matching
   * class. The OMG prefix (omg.org) is replaced by the java prefix org.omg. No
   * other prefixes are added.
   * 
   * @param IDL the idl name.
   * 
   * @return the matching class or null if no such is available.
   */
  public static Class Idl2class(String IDL)
  {
    synchronized (m_classes)
      {
        Class c = (Class) m_classes.get(IDL);

        if (c != null)
          return c;
        else
          {
            String s = IDL;
            int a = s.indexOf(':') + 1;
            int b = s.lastIndexOf(':');

            s = IDL.substring(a, b);

            if (s.startsWith(OMG_PREFIX))
              s = JAVA_PREFIX + s.substring(OMG_PREFIX.length());

            String cn = s.replace('/', '.');

            try
              {
                c = forName(cn);
                m_classes.put(IDL, c);
                return c;
              }
            catch (ClassNotFoundException ex)
              {
                return null;
              }
          }
      }
  }

  /**
   * Converts the given IDL name to class name, tries to load the matching class
   * and create an object instance with parameterless constructor. The OMG
   * prefix (omg.org) is replaced by the java prefix org.omg. No other prefixes
   * are added.
   * 
   * @param IDL the idl name.
   * 
   * @return instantiated object instance or null if such attempt was not
   * successful.
   */
  public static java.lang.Object Idl2Object(String IDL)
  {
    Class cx = Idl2class(IDL);

    try
      {
        if (cx != null)
          return cx.newInstance();
        else
          return null;
      }
    catch (Exception ex)
      {
        return null;
      }
  }

  /**
   * Convert the class name to IDL or RMI name (repository id). If the class
   * inherits from IDLEntity, ValueBase or SystemException, returns repository
   * Id in the IDL:(..) form. If it does not, returns repository Id in the
   * RMI:(..) form.
   * 
   * @param cx the class for that the name must be computed.
   * 
   * @return the idl or rmi name.
   */
  public static synchronized String getRepositoryId(Class cx)
  {
    String name = (String) m_names.get(cx);
    if (name != null)
      return name;

    String cn = cx.getName();
    if (!(IDLEntity.class.isAssignableFrom(cx)
      || ValueBase.class.isAssignableFrom(cx) || SystemException.class.isAssignableFrom(cx)))
      {
        // Not an IDL entity.
        name = Util.createValueHandler().getRMIRepositoryID(cx);
      }
    else
      {
        if (cn.startsWith(JAVA_PREFIX))
          cn = OMG_PREFIX
            + cn.substring(JAVA_PREFIX.length()).replace('.', '/');
        else if (cn.startsWith(CLASSPATH_PREFIX))
          cn = OMG_PREFIX
            + cn.substring(CLASSPATH_PREFIX.length()).replace('.', '/');

        name = "IDL:" + cn + ":1.0";
      }
    m_names.put(cx, name);
    return name;
  }

  /**
   * Insert the passed parameter into the given Any, assuming that the helper
   * class is available. The helper class must have the "Helper" suffix and be
   * in the same package as the class of the object being inserted.
   * 
   * @param into the target to insert.
   * 
   * @param object the object to insert. It can be any object as far as the
   * corresponding helper is provided.
   * 
   * @return true on success, false otherwise.
   */
  public static boolean insertWithHelper(Any into, Object object)
  {
    try
      {
        String helperClassName = object.getClass().getName() + "Helper";
        Class helperClass = forName(helperClassName);

        Method insert = helperClass.getMethod("insert", new Class[] {
          Any.class, object.getClass() });

        insert.invoke(null, new Object[] { into, object });

        return true;
      }
    catch (Exception exc)
      {
        // Failed due some reason.
        return false;
      }
  }

  /**
   * Insert the system exception into the given Any.
   */
  public static boolean insertSysException(Any into, SystemException exception)
  {
    try
      {
        BufferedCdrOutput output = new BufferedCdrOutput();

        String m_exception_id = getRepositoryId(exception.getClass());
        output.write_string(m_exception_id);
        output.write_ulong(exception.minor);
        CompletionStatusHelper.write(output, exception.completed);

        String name = getDefaultName(m_exception_id);

        GeneralHolder h = new GeneralHolder(output);

        into.insert_Streamable(h);

        RecordTypeCode r = new RecordTypeCode(TCKind.tk_except);
        r.setId(m_exception_id);
        r.setName(name);
        into.type(r);

        return true;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        return false;
      }
  }

  /**
   * Get the type name from the IDL string.
   */
  public static String getDefaultName(String idl)
  {
    int f1 = idl.lastIndexOf("/");
    int p1 = (f1 < 0) ? 0 : f1;
    int p2 = idl.indexOf(":", p1);
    if (p2 < 0)
      p2 = idl.length();

    String name = idl.substring(f1 + 1, p2);
    return name;
  }

  /**
   * Insert this exception into the given Any. On failure, insert the UNKNOWN
   * exception.
   */
  public static void insertException(Any into, Throwable exception)
  {
    boolean ok = false;
    if (exception instanceof SystemException)
      ok = insertSysException(into, (SystemException) exception);
    else if (exception instanceof UserException)
      ok = insertWithHelper(into, exception);

    if (!ok)
      ok = insertSysException(into, new UNKNOWN());
    if (!ok)
      throw new InternalError("Exception wrapping broken");
  }

  /**
   * Find helper for the class with the given name.
   */
  public static Class findHelper(String idl)
  {
    synchronized (m_helpers)
      {
        Class c = (Class) m_helpers.get(idl);
        if (c != null)
          return c;
        try
          {
            String helper = toHelperName(idl);
            c = forName(helper);

            m_helpers.put(idl, c);
            return c;
          }
        catch (Exception ex)
          {
            return null;
          }
      }
  }
  
  /**
   * Load the class with the given name. This method tries to use the context
   * class loader first. If this fails, it searches for the suitable class
   * loader in the caller stack trace. This method is a central point where all
   * requests to find a class by name are delegated.
   */
  public static Class forName(String className) throws ClassNotFoundException
  {
    try
      {
        return Class.forName(className, true,
                             Thread.currentThread().getContextClassLoader());
      }
    catch (ClassNotFoundException nex)
      {
        /**
         * Returns the first user defined class loader on the call stack, or
         * null when no non-null class loader was found.
         */
        Class[] ctx = VMStackWalker.getClassContext();
        for (int i = 0; i < ctx.length; i++)
          {
            // Since we live in a class loaded by the bootstrap
            // class loader, getClassLoader is safe to call without
            // needing to be wrapped in a privileged action.
            ClassLoader cl = ctx[i].getClassLoader();
            try
              {
                if (cl != null)
                  return Class.forName(className, true, cl);
              }
            catch (ClassNotFoundException nex2)
              {
                // Try next.
              }
          }
      }
    throw new ClassNotFoundException(className);
  }
}