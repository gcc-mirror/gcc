/* ObjectReferenceCommandSet.java -- lass to implement the ObjectReference
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
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.classpath.jdwp.processor;

import gnu.classpath.jdwp.IVirtualMachine;
import gnu.classpath.jdwp.Jdwp;
import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.exception.InvalidFieldException;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.IdManager;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ReferenceTypeId;
import gnu.classpath.jdwp.util.Value;
import gnu.classpath.jdwp.util.MethodInvoker;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;

/**
 * A class representing the ObjectReference Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ObjectReferenceCommandSet implements CommandSet
{
  // Our hook into the jvm
  private final IVirtualMachine vm = Jdwp.getIVirtualMachine();

  // Manages all the different ids that are assigned by jdwp
  private final IdManager idMan = Jdwp.getIdManager();

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
              " not found in String Reference Command Set.");
          }
      }
    catch (IOException ex)
      {
        // The DataOutputStream we're using isn't talking to a socket at all
        // So if we throw an IOException we're in serious trouble
        throw new JdwpInternalErrorException(ex);
      }
    return true;
  }

  private void executeReferenceType(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    Object obj = oid.getObject();
    Class clazz = obj.getClass();
    ReferenceTypeId refId = idMan.getReferenceTypeId(clazz);
    refId.writeTagged(os);
  }

  private void executeGetValues(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    Object obj = oid.getObject();

    int numFields = bb.getInt();

    os.writeInt(numFields); // Looks pointless but this is the protocol

    for (int i = 0; i < numFields; i++)
      {
        Field field = (Field) idMan.readId(bb).getObject();
        Value.writeValueFromField(os, field, obj);
      }
  }

  private void executeSetValues(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    Object obj = oid.getObject();

    int numFields = bb.getInt();

    for (int i = 0; i < numFields; i++)
      {
        Field field = (Field) idMan.readId(bb).getObject();
        Object value = Value.getObj(bb, field);
        try
          {
            field.set(obj, value);
          }
        catch (IllegalArgumentException ex)
          {
            // I suppose this would best qualify as an invalid field then
            throw new InvalidFieldException(ex);
          }
        catch (IllegalAccessException ex)
          {
            // We should be able to access any field
            throw new JdwpInternalErrorException(ex);
          }
      }
  }

  private void executeMonitorInfo(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    // This command is optional, determined by VirtualMachines CapabilitiesNew
    // so we'll leave it till later to implement
    throw new NotImplementedException(
      "Command ExecuteMonitorInfo not implemented.");

  }

  private void executeInvokeMethod(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    Object obj = oid.getObject();

    ObjectId tid = idMan.readId(bb);
    Thread thread = (Thread) tid.getObject();

    ReferenceTypeId rid = idMan.readReferenceTypeId(bb);
    Class clazz = rid.getType();

    ObjectId mid = idMan.readId(bb);
    Method method = (Method) mid.getObject();

    int args = bb.getInt();
    Object[] values = new Object[args];

    for (int i = 0; i < args; i++)
      {
        values[i] = Value.getObj(bb);
      }

    int invokeOptions = bb.getInt();

    if ((invokeOptions & JdwpConstants.InvokeOptions.INVOKE_SINGLE_THREADED) != 0)
      { // We must suspend all other running threads first
        vm.suspendAllThreads();
      }
    boolean nonVirtual;
    if ((invokeOptions & JdwpConstants.InvokeOptions.INVOKE_NONVIRTUAL) != 0)
      nonVirtual = true;
    else
      nonVirtual = false;
    MethodInvoker vmi = new MethodInvoker(vm);

    vmi.executeMethod(obj, thread, clazz, method, values, nonVirtual);
    Object value = vmi.getReturnedValue();
    ObjectId exceptionId = vmi.getExceptionId();
    
    Value.writeValue(os, value);
    exceptionId.writeTagged(os);
  }

  private void executeDisableCollection(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    oid.disableCollection();
  }

  private void executeEnableCollection(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    oid.enableCollection();
  }

  private void executeIsCollected(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readId(bb);
    boolean collected = oid.isCollected();
    os.writeBoolean(collected);
  }
}
