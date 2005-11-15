/* ClassTypeCommandSet.java -- class to implement the ClassType
   Command Set
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.processor;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.VMVirtualMachine;
import gnu.classpath.jdwp.exception.InvalidFieldException;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ReferenceTypeId;
import gnu.classpath.jdwp.util.MethodResult;
import gnu.classpath.jdwp.util.Value;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;

/**
 * A class representing the ClassType Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ClassTypeCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
      throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ClassType.SUPERCLASS:
            executeSuperclass(bb, os);
            break;
          case JdwpConstants.CommandSet.ClassType.SET_VALUES:
            executeSetValues(bb, os);
            break;
          case JdwpConstants.CommandSet.ClassType.INVOKE_METHOD:
            executeInvokeMethod(bb, os);
            break;
          case JdwpConstants.CommandSet.ClassType.NEW_INSTANCE:
            executeNewInstance(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
              " not found in ClassType Command Set.");
          }
      }
    catch (IOException ex)
      {
        // The DataOutputStream we're using isn't talking to a socket at all
        // So if we throw an IOException we're in serious trouble
        throw new JdwpInternalErrorException(ex);
      }

    return false;
  }

  private void executeSuperclass(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();
    Class superClazz = clazz.getSuperclass();

    ReferenceTypeId clazzId = idMan.getReferenceTypeId(superClazz);
    clazzId.write(os);
  }

  private void executeSetValues(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);

    // We don't actually seem to need this...
    Class clazz = refId.getType();

    int numValues = bb.getInt();

    for (int i = 0; i < numValues; i++)
      {
        ObjectId fieldId = idMan.readObjectId(bb);
        Field field = (Field) (fieldId.getObject());
        Object value = Value.getUntaggedObj(bb, field.getType());
        try
          {
            field.setAccessible(true); // Might be a private field
            field.set(null, value);
          }
        catch (IllegalArgumentException ex)
          {
            throw new InvalidFieldException(ex);
          }
        catch (IllegalAccessException ex)
          { // Since we set it as accessible this really shouldn't happen
            throw new JdwpInternalErrorException(ex);
          }
      }
  }

  private void executeInvokeMethod(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    MethodResult mr = invokeMethod(bb);

    Object value = mr.getReturnedValue();
    Exception exception = mr.getThrownException();
    ObjectId eId = idMan.getObjectId(exception);

    Value.writeTaggedValue(os, value);
    eId.writeTagged(os);
  }

  private void executeNewInstance(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    MethodResult mr = invokeMethod(bb);

    Object obj = mr.getReturnedValue();
    ObjectId oId = idMan.getObjectId(obj);
    Exception exception = mr.getThrownException();
    ObjectId eId = idMan.getObjectId(exception);

    oId.writeTagged(os);
    eId.writeTagged(os);
  }

  /**
   * Execute the static method and return the resulting MethodResult.
   */
  private MethodResult invokeMethod(ByteBuffer bb) throws JdwpException,
      IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();

    ObjectId tId = idMan.readObjectId(bb);
    Thread thread = (Thread) tId.getObject();

    ObjectId mId = idMan.readObjectId(bb);
    Method method = (Method) mId.getObject();

    int args = bb.getInt();
    Object[] values = new Object[args];

    for (int i = 0; i < args; i++)
      {
        values[i] = Value.getObj(bb);
      }

    int invokeOpts = bb.getInt();
    boolean suspend = ((invokeOpts
			& JdwpConstants.InvokeOptions.INVOKE_SINGLE_THREADED)
		       != 0);
    try
      {
        if (suspend)
	  VMVirtualMachine.suspendAllThreads ();

        MethodResult mr = VMVirtualMachine.executeMethod(null, thread,
							 clazz, method,
							 values, false);
        if (suspend)
	  VMVirtualMachine.resumeAllThreads ();

        return mr;
      }
    catch (Exception ex)
      {
        if (suspend)
	  VMVirtualMachine.resumeAllThreads ();

        throw new JdwpInternalErrorException(ex);
      }
  }
}
