/* ExceptionCreator.java --
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

import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.CompletionStatusHelper;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.UNKNOWN;
import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 * Creates java objects from the agreed IDL names for the simple
 * case when the CORBA object is directly mapped into the locally
 * defined java class.
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
   * The prefix for classes that are placed instide the
   * gnu.CORBA namespace.
   */
  public static final String CLASSPATH_PREFIX = "gnu.CORBA.";

  /**
   * Try to instantiate an object with the given IDL name.
   * The object must be mapped to the local java class.
   * The omg.org domain must be mapped into the object in either
   * org/omg or gnu/CORBA namespace.
   *
   * @param IDL name
   * @return instantiated object instance or null if no such
   * available.
   */
  public static java.lang.Object createObject(String idl, String suffix)
  {
    try
      {
        return Class.forName(toClassName(JAVA_PREFIX, idl) + suffix)
                    .newInstance();
      }
    catch (Exception ex)
      {
        try
          {
            return Class.forName(toClassName(CLASSPATH_PREFIX, idl) + suffix)
                        .newInstance();
          }
        catch (Exception exex)
          {
            return null;
          }
      }
  }

  /**
   * Create the system exception with the given idl name.
   *
   * @param idl the exception IDL name, must match the syntax
   * "IDL:<class/name>:1.0".
   * @param minor the exception minor code.
   * @param completed the exception completion status.
   *
   * @return the created exception.
   */
  public static SystemException createSystemException(String idl, int minor,
                                                      CompletionStatus completed
                                                     )
  {
    try
      {
        String cl = toClassName(JAVA_PREFIX, idl);
        Class exClass = Class.forName(cl);

        Constructor constructor =
          exClass.getConstructor(new Class[]
                                 {
                                   String.class, int.class,
                                   CompletionStatus.class
                                 }
                                );

        Object exception =
          constructor.newInstance(new Object[]
                                  {
                                    " Remote exception " + idl + ", minor " +
                                    minor + ", " + completed + ".",
                                    new Integer(minor), completed
                                  }
                                 );

        return (SystemException) exception;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        return new UNKNOWN("Unsupported system exception", minor, completed);
      }
  }

  /**
   * Read the system exception from the given stream.
   * @param input the CDR stream to read from.
   * @return the exception that has been stored in the stream
   * (IDL name, minor code and completion status).
   */
  public static SystemException readSystemException(InputStream input)
  {
    String idl = input.read_string();
    int minor = input.read_ulong();
    CompletionStatus status = CompletionStatusHelper.read(input);

    SystemException exception =
      ObjectCreator.createSystemException(idl, minor, status);

    return exception;
  }

  /**
   * Reads the user exception, having the given Id, from the
   * input stream. The id is expected to be in the form like
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
        String helper = toHelperName(idl);
        Class helperClass = Class.forName(helper);

        Method read =
          helperClass.getMethod("read",
                                new Class[]
                                {
                                  org.omg.CORBA.portable.InputStream.class
                                }
                               );

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
                                          SystemException ex
                                         )
  {
    String exIDL = toIDL(ex.getClass().getName());
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
   * Converts the given IDL name to class name and tries to load the
   * matching class. The OMG prefix (omg.org) is replaced by
   * the java prefix org.omg. No other prefixes are added.
   *
   * @param IDL the idl name.
   *
   * TODO Cache the returned classes, avoiding these string manipulations
   * each time the conversion is required.
   *
   * @return the matching class or null if no such is available.
   */
  public static Class Idl2class(String IDL)
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
        return Class.forName(cn);
      }
    catch (ClassNotFoundException ex)
      {
        return null;
      }
  }

  /**
   * Converts the given IDL name to class name, tries to load the
   * matching class and create an object instance with parameterless
   * constructor. The OMG prefix (omg.org) is replaced by
   * the java prefix org.omg. No other prefixes are added.
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
   * Convert the class name to IDL name.
   *
   * @param cn the class name.
   *
   * @return the idl name.
   */
  public static String toIDL(String cn)
  {
    if (cn.startsWith(JAVA_PREFIX))
      cn = OMG_PREFIX + cn.substring(JAVA_PREFIX.length()).replace('.', '/');
    else if (cn.startsWith(CLASSPATH_PREFIX))
      cn =
        OMG_PREFIX + cn.substring(CLASSPATH_PREFIX.length()).replace('.', '/');

    return "IDL:" + cn + ":1.0";
  }
}