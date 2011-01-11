/* _DemoTesterImplBase.java --
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

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ByteHolder;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;

/**
 * The base for the class that is actually implementing the functionality
 * of the object on the server side ({@link DemoServant} of our case).
 *
 * Following CORBA standards, the name of this class must start from
 * underscore and end by the "ImplBase".
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class _DemoTesterImplBase
  extends ObjectImpl
  implements DemoTester, InvokeHandler
{
/**
 * When the server receives the request message from client, it
 * calls this method.
 *
 * @param a_method the method name.
 * @param in the CDR stream, from where the implementing code must
 * read the method parameters.
 * @param rh the response handler, used to get the stream where
 * the returned values must be written.
 *
 * @return the stream, obtained from the response handler.
 */
  public OutputStream _invoke(String a_method, InputStream in,
                              ResponseHandler rh
                             )
  {
    OutputStream out;

    /* Get the field value. */
    if (a_method.equals("_get_theField"))
      {
        int result = (int) 0;
        result = theField();
        out = rh.createReply();
        out.write_long(result);
      }
    else
    /* Set the field value. */
    if (a_method.equals("_set_theField"))
      {
        int newTheField = in.read_long();
        theField(newTheField);
        out = rh.createReply();
      }
    else
    /* Logs calls to the file. */
    if (a_method.equals("sayHello"))
      {
        sayHello();
        out = rh.createReply();
      }
    else
    /* Passes various parameters in both directions. */
    if (a_method.equals("passSimple"))
      {
        ByteHolder an_octet = new ByteHolder();
        an_octet.value = in.read_octet();

        int a_long = in.read_long();
        ShortHolder a_short = new ShortHolder();
        a_short.value = in.read_short();

        StringHolder a_string = new StringHolder();
        a_string.value = in.read_string();

        DoubleHolder a_double = new DoubleHolder();
        int result = passSimple(an_octet, a_long, a_short, a_string, a_double);
        out = rh.createReply();
        out.write_long(result);
        out.write_octet(an_octet.value);
        out.write_short(a_short.value);
        out.write_string(a_string.value);
        out.write_double(a_double.value);
      }
    else
    /* Passes the 'wide' (usually Unicode) string and the ordinary string. */
    if (a_method.equals("passCharacters"))
      {
        String wide = in.read_wstring();
        String narrow = in.read_string();
        String result = null;
        result = passCharacters(wide, narrow);
        out = rh.createReply();
        out.write_wstring(result);
      }
    else
    /*
      Throws either 'WeThrowThisException' with the 'ourField' field
      initialised to the passed positive value
      or system exception (if the parameter is zero or negative).
     */
    if (a_method.equals("throwException"))
      {
        try
          {
            int parameter = in.read_long();
            throwException(parameter);
            out = rh.createReply();
          }
        catch (WeThrowThisException exception)
          {
            out = rh.createExceptionReply();
            WeThrowThisExceptionHelper.write(out, exception);
          }
      }
    else
    /* Passes and returns the structures. */
    if (a_method.equals("passStructure"))
      {
        StructureToPass in_structure = StructureToPassHelper.read(in);
        StructureToReturn result = null;
        result = passStructure(in_structure);
        out = rh.createReply();
        StructureToReturnHelper.write(out, result);
      }
    else
    /* Passes and returns the string sequence. */
    if (a_method.equals("passStrings"))
      {
        String[] arg = StringSeqHelper.read(in);
        String[] result = null;
        result = passStrings(arg);
        out = rh.createReply();
        StringSeqHelper.write(out, result);
      }
    else
    /** Pass and return the tree structure */
    if (a_method.equals("passTree"))
      {
        TreeNodeHolder tree = new TreeNodeHolder();
        tree.value = TreeNodeHelper.read(in);
        passTree(tree);
        out = rh.createReply();
        TreeNodeHelper.write(out, tree.value);
      }

    else
      throw new BAD_OPERATION("No method: " + a_method, 0,
                              CompletionStatus.COMPLETED_MAYBE
                             );

    return out;
  }

  /**
   * Return an array of this object repository ids.
   */
  public String[] _ids()
  {
    // They are the same as for the stub.
    return _DemoTesterStub._ids;
  }
}
