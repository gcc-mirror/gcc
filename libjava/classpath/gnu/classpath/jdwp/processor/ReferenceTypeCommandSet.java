/* ReferenceTypeCommandSet.java -- class to implement the ReferenceType
   Command Set
   Copyright (C) 2005, 2006, 2007 Free Software Foundation

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
import gnu.classpath.jdwp.util.JdwpString;
import gnu.classpath.jdwp.util.Signature;
import gnu.classpath.jdwp.value.Value;
import gnu.classpath.jdwp.value.ValueFactory;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;

/**
 * A class representing the ReferenceType Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ReferenceTypeCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
    throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ReferenceType.SIGNATURE:
            executeSignature(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.CLASS_LOADER:
            executeClassLoader(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.MODIFIERS:
            executeModifiers(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.FIELDS:
            executeFields(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.METHODS:
            executeMethods(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.GET_VALUES:
            executeGetValues(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.SOURCE_FILE:
            executeSourceFile(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.NESTED_TYPES:
            executeNestedTypes(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.STATUS:
            executeStatus(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.INTERFACES:
            executeInterfaces(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.CLASS_OBJECT:
            executeClassObject(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.SOURCE_DEBUG_EXTENSION:
            executeSourceDebugExtension(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.SIGNATURE_WITH_GENERIC:
            executeSignatureWithGeneric(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.FIELDS_WITH_GENERIC:
            executeFieldWithGeneric(bb, os);
            break;
          case JdwpConstants.CommandSet.ReferenceType.METHODS_WITH_GENERIC:
            executeMethodsWithGeneric(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
              " not found in ReferenceType Command Set.");
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

  private void executeSignature(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    String sig = Signature.computeClassSignature(refId.getType());
    JdwpString.writeString(os, sig);
  }

  private void executeClassLoader(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);

    Class clazz = refId.getType();
    ClassLoader loader = clazz.getClassLoader();
    ObjectId oid = idMan.getObjectId(loader);
    oid.write(os);
  }

  private void executeModifiers(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);

    Class clazz = refId.getType();
    os.writeInt(clazz.getModifiers());
  }

  private void executeFields(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();

    Field[] fields = clazz.getFields();
    os.writeInt(fields.length);
    for (int i = 0; i < fields.length; i++)
      {
        Field field = fields[i];
        idMan.getObjectId(field).write(os);
        JdwpString.writeString(os, field.getName());
        JdwpString.writeString(os, Signature.computeFieldSignature(field));
        os.writeInt(field.getModifiers());
      }
  }

  private void executeMethods(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();

    VMMethod[] methods = VMVirtualMachine.getAllClassMethods(clazz);
    os.writeInt (methods.length);
    for (int i = 0; i < methods.length; i++)
      {
        VMMethod method = methods[i];
        method.writeId(os);
        JdwpString.writeString(os, method.getName());
        JdwpString.writeString(os, method.getSignature());
        os.writeInt(method.getModifiers());
      }
  }

  private void executeGetValues(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();

    int numFields = bb.getInt();
    os.writeInt(numFields); // Looks pointless but this is the protocol
    for (int i = 0; i < numFields; i++)
      {
        ObjectId fieldId = idMan.readObjectId(bb);
        Field field = (Field) (fieldId.getObject());
        Class fieldClazz = field.getDeclaringClass();

        // We don't actually need the clazz to get the field but we might as
        // well check that the debugger got it right
        if (fieldClazz.isAssignableFrom(clazz))
          {
            try
              {
                field.setAccessible(true); // Might be a private field
                Object value = field.get(null);
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
        else
          throw new InvalidFieldException(fieldId.getId());
      }
  }

  private void executeSourceFile(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();

    // We'll need to go into the jvm for this unless there's an easier way
    String sourceFileName = VMVirtualMachine.getSourceFile(clazz);
    JdwpString.writeString(os, sourceFileName);
    // clazz.getProtectionDomain().getCodeSource().getLocation();
  }

  private void executeNestedTypes(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();
    Class[] declaredClazzes = clazz.getDeclaredClasses();
    os.writeInt(declaredClazzes.length);
    for (int i = 0; i < declaredClazzes.length; i++)
      {
        Class decClazz = declaredClazzes[i];
        ReferenceTypeId clazzId = idMan.getReferenceTypeId(decClazz);
        clazzId.writeTagged(os);
      }
  }

  private void executeStatus(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();

    // I don't think there's any other way to get this
    int status = VMVirtualMachine.getClassStatus(clazz);
    os.writeInt(status);
  }

  private void executeInterfaces(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();
    Class[] interfaces = clazz.getInterfaces();
    os.writeInt(interfaces.length);
    for (int i = 0; i < interfaces.length; i++)
      {
        Class interfaceClass = interfaces[i];
        ReferenceTypeId intId = idMan.getReferenceTypeId(interfaceClass);
        intId.write(os);
      }
  }

  private void executeClassObject(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ReferenceTypeId refId = idMan.readReferenceTypeId(bb);
    Class clazz = refId.getType();
    ObjectId clazzObjectId = idMan.getObjectId(clazz);
    clazzObjectId.write(os);
  }

  private void executeSourceDebugExtension(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    if (!VMVirtualMachine.canGetSourceDebugExtension)
      {
	String msg = "source debug extension is not supported";
	throw new NotImplementedException(msg);
      }

    ReferenceTypeId id = idMan.readReferenceTypeId(bb);
    String ext = VMVirtualMachine.getSourceDebugExtension (id.getType());
    JdwpString.writeString(os, ext);
  }

  private void executeSignatureWithGeneric(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    // We don't have generics yet
    throw new NotImplementedException(
      "Command SignatureWithGeneric not implemented.");
  }

  private void executeFieldWithGeneric(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    // We don't have generics yet
    throw new NotImplementedException(
      "Command executeFieldWithGeneric not implemented.");
  }

  private void executeMethodsWithGeneric(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    // We don't have generics yet
    throw new NotImplementedException(
      "Command executeMethodsWithGeneric not implemented.");
  }
}
