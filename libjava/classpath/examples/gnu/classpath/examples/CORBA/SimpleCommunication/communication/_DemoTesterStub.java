/* _DemoTesterStub.java --
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


package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.ByteHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

/**
 * The stub (proxy) class, representing the remote object on the client
 * side. It has all the same methods as the actual implementation
 * on the server side. These methods contain the code for remote
 * invocation.
 * 
 * Following CORBA standards, the name of this class must start from
 * underscore and end by the "Stub".
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _DemoTesterStub
  extends ObjectImpl
  implements DemoTester
{
  /**
   * A string array of DemoTester repository ids.
   */
  public static String[] _ids =
    {
      "IDL:gnu/classpath/examples/CORBA/SimpleCommunication/communication/DemoTester:1.0"
    };

  /**
   * Return an array of DemoTester repository ids.
   */
  public String[] _ids()
  {
    return _ids;
  }

  /**
   * Passes wide (UTF-16) string and narrow (ISO8859_1) string.
   * @see gnu.CORBA.GIOP.CharSets_OSF for supported and default
   * encodings.
   */
  public String passCharacters(String wide, String narrow)
  {
    InputStream in = null;
    try
      {
        // Get the output stream.
        OutputStream out = _request("passCharacters", true);

        // Write the parameters.

        // The first string is passed as "wide"
        // (usually 16 bit UTF-16) string.
        out.write_wstring(wide);

        // The second string is passed as "narrow"
        // (usually 8 bit ISO8859_1) string.
        out.write_string(narrow);

        // Do the invocation.
        in = _invoke(out);

        // Read the method return value.
        String result = in.read_wstring();
        return result;
      }
    catch (ApplicationException ex)
      {
        // The exception has been throws on remote side, but we
        // do not expect any. Throw the MARSHAL exception.
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        // This exception means that the parameters must be re-written.
        return passCharacters(wide, narrow);
      }
    finally
      {
        // Release the resources, associated with the reply stream.
        _releaseReply(in);
      }
  }

  /**
   * Passes various parameters in both directions. The parameters that
   * shoud also return the values are wrapped into holders.
   */
  public int passSimple(ByteHolder an_octet, int a_long, ShortHolder a_short,
                        StringHolder a_string, DoubleHolder a_double
                       )
  {
    InputStream in = null;
    try
      {
        // Get the stream where the parameters must be written:
        OutputStream out = _request("passSimple", true);

        // Write the parameters.
        out.write_octet(an_octet.value);
        out.write_long(a_long);
        out.write_short(a_short.value);
        out.write_string(a_string.value);

        // Invoke the method.
        in = _invoke(out);

        // Read the returned values.
        int result = in.read_long();

        // Read the inout and out parameters.
        an_octet.value = in.read_octet();
        a_short.value = in.read_short();
        a_string.value = in.read_string();
        a_double.value = in.read_double();
        return result;
      }
    catch (ApplicationException ex)
      {
        // Handle excepion on remote side.
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        // Handle instruction to resend the parameters.
        return passSimple(an_octet, a_long, a_short, a_string, a_double);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
    Passes and returns the string sequence.
    */
  public String[] passStrings(String[] arg)
  {
    InputStream in = null;
    try
      {
        // Get the stream where the parameters must be written:
        OutputStream out = _request("passStrings", true);

        // Wrap the string array using the string sequence helper.
        StringSeqHelper.write(out, arg);

        // Invoke the method.
        in = _invoke(out);

        // Read the returned result using the string sequence helper.
        String[] result = StringSeqHelper.read(in);
        return result;
      }
    catch (ApplicationException ex)
      {
        // Handle the exception, thrown on remote side.
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        return passStrings(arg);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
    Passes and returns the structures.
    */
  public StructureToReturn passStructure(StructureToPass in_structure)
  {
    InputStream in = null;
    try
      {
        // Get the stream where the parameters must be written.
        OutputStream out = _request("passStructure", true);

        // Write the structure, using its helper.
        StructureToPassHelper.write(out, in_structure);

        // Invoke the method.
        in = _invoke(out);

        // Read the returned structer, using another helper.
        StructureToReturn result = StructureToReturnHelper.read(in);
        return result;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        return passStructure(in_structure);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Pass and return the tree structure
   */
  public void passTree(TreeNodeHolder tree)
  {
    InputStream in = null;
    try
      {
        // Get the stream where the parameters must be written.
        OutputStream out = _request("passTree", true);

        // Write the tree (TreeNode with its chilred, grandchildren and so on),
        // using the appropriate helper.
        TreeNodeHelper.write(out, tree.value);

        // Call the method.
        in = _invoke(out);

        // Read the returned tree.
        tree.value = TreeNodeHelper.read(in);
      }
    catch (ApplicationException ex)
      {
        // Handle eception on remote side.
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        passTree(tree);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * One way call of the remote method.
   */
  public void sayHello()
  {
    InputStream in = null;
    try
      {
        // As we do not expect any response, the second
        // parameter is 'false'.
        OutputStream out = _request("sayHello", false);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        sayHello();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Get the field value.
   */
  public int theField()
  {
    InputStream in = null;
    try
      {
        // The special name of operation instructs just to get
        // the field value rather than calling the method.
        OutputStream out = _request("_get_theField", true);
        in = _invoke(out);

        int result = in.read_long();
        return result;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        return theField();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Set the field value.
   */
  public void theField(int newTheField)
  {
    InputStream in = null;
    try
      {
        // The special name of operation instructs just to set
        // the field value rather than calling the method.
        OutputStream out = _request("_set_theField", true);
        out.write_long(newTheField);
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException _rm)
      {
        theField(newTheField);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * The server side exception tests.
   *
   * @param parameter the server throws the user exception in the case
   * of the positive value of this argument, and system
   * exception otherwise.
   *
   * @throws WeThrowThisException
   */
  public void throwException(int parameter)
                      throws WeThrowThisException
  {
    InputStream in = null;
    try
      {
        // Get stream.
        OutputStream out = _request("throwException", true);

        // Write parameter.
        out.write_long(parameter);

        // Call method.
        in = _invoke(out);
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        // Get the exception id.
        String id = ex.getId();

        // If this is the user exception we expect to catch, read and throw
        // it here. The system exception, if thrown, is handled by _invoke.
        if (id.equals("IDL:gnu/classpath/examples/CORBA/SimpleCommunication/communication/WeThrowThisException:1.0")
           )
          throw WeThrowThisExceptionHelper.read(in);
        else
          throw new MARSHAL(id);
      }
    catch (RemarshalException _rm)
      {
        throwException(parameter);
      }
    finally
      {
        _releaseReply(in);
      }
  }
}
