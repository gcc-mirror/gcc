/* ObjectReferenceCommandSet.java -- class to implement the ObjectReference
   Command Set
   Copyright (C) 2005, 2007 Free Software Foundation

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


package gnu.classpath.jdwp.processor;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.VMMethod;
import gnu.classpath.jdwp.VMVirtualMachine;
import gnu.classpath.jdwp.exception.InvalidFieldException;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ReferenceTypeId;
import gnu.classpath.jdwp.util.MethodResult;
import gnu.classpath.jdwp.util.MonitorInfo;
import gnu.classpath.jdwp.value.Value;
import gnu.classpath.jdwp.value.ValueFactory;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;

/**
 * A class representing the ObjectReference Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ObjectReferenceCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
    throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ObjectReference.REFERENCE_TYPE:
            executeReferenceType(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.GET_VALUES:
            executeGetValues(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.SET_VALUES:
            executeSetValues(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.MONITOR_INFO:
            executeMonitorInfo(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.INVOKE_METHOD:
            executeInvokeMethod(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.DISABLE_COLLECTION:
            executeDisableCollection(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.ENABLE_COLLECTION:
            executeEnableCollection(bb, os);
            break;
          case JdwpConstants.CommandSet.ObjectReference.IS_COLLECTED:
            executeIsCollected(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
              " not found in ObjectReference Command Set.");
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

  private void executeReferenceType(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object obj = oid.getObject();
    Class clazz = obj.getClass();
    ReferenceTypeId refId = idMan.getReferenceTypeId(clazz);
    refId.writeTagged(os);
  }

  private void executeGetValues(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object obj = oid.getObject();

    int numFields = bb.getInt();

    os.writeInt(numFields); // Looks pointless but this is the protocol

    for (int i = 0; i < numFields; i++)
      {
        Field field = (Field) idMan.readObjectId(bb).getObject();
        try
          {
            field.setAccessible(true); // Might be a private field
            Object value = field.get(obj);
            Value val = ValueFactory.createFromObject(value, 
                                                      field.getType());
            val.writeTagged(os);
          }
        catch (IllegalArgumentException ex)
          {
            // I suppose this would best qualify as an invalid field then
            throw new InvalidFieldException(ex);
          }
        catch (IllegalAccessException ex)
          {
            // Since we set it as accessible this really shouldn't happen
            throw new JdwpInternalErrorException(ex);
          }
      }
  }

  private void executeSetValues(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object obj = oid.getObject();

    int numFields = bb.getInt();

    for (int i = 0; i < numFields; i++)
      {
        Field field = (Field) idMan.readObjectId(bb).getObject();
        Object value = Value.getUntaggedObject(bb, field.getType());
        try
          {
            field.setAccessible(true); // Might be a private field
            field.set(obj, value);
          }
        catch (IllegalArgumentException ex)
          {
            // I suppose this would best qualify as an invalid field then
            throw new InvalidFieldException(ex);
          }
        catch (IllegalAccessException ex)
          {
            // Since we set it as accessible this really shouldn't happen
            throw new JdwpInternalErrorException(ex);
          }
      }
  }

  private void executeMonitorInfo(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    if (!VMVirtualMachine.canGetMonitorInfo)
      {
	String msg = "getting monitor info not supported";
	throw new NotImplementedException(msg);
      }

    ObjectId oid = idMan.readObjectId(bb);
    Object obj = oid.getObject();
    MonitorInfo info = VMVirtualMachine.getMonitorInfo(obj);
    info.write(os);
  }

  private void executeInvokeMethod(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object obj = oid.getObject();

    ObjectId tid = idMan.readObjectId(bb);
    Thread thread = (Thread) tid.getObject();

    ReferenceTypeId rid = idMan.readReferenceTypeId(bb);
    Class clazz = rid.getType();

    VMMethod method = VMMethod.readId(clazz, bb);

    int args = bb.getInt();
    Value[] values = new Value[args];

    for (int i = 0; i < args; i++)
      values[i] = ValueFactory.createFromTagged(bb);

    int invokeOptions = bb.getInt();
    MethodResult mr = VMVirtualMachine.executeMethod(obj, thread,
						     clazz, method,
						     values, invokeOptions);
    Throwable exception = mr.getThrownException();
    ObjectId eId = idMan.getObjectId(exception);
    mr.getReturnedValue().writeTagged(os);
    eId.writeTagged(os);
  }

  private void executeDisableCollection(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    oid.disableCollection();
  }

  private void executeEnableCollection(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    oid.enableCollection();
  }

  private void executeIsCollected(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    boolean collected = (oid.getReference().get () == null);
    os.writeBoolean(collected);
  }
}
