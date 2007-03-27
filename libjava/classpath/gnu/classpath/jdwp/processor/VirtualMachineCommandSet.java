/* VirtualMachineCommandSet.java -- class to implement the VirtualMachine
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
import gnu.classpath.jdwp.VMFrame;
import gnu.classpath.jdwp.VMVirtualMachine;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ReferenceTypeId;
import gnu.classpath.jdwp.util.JdwpString;
import gnu.classpath.jdwp.util.Signature;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;

/**
 * A class representing the VirtualMachine Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class VirtualMachineCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
    throws JdwpException
  {
    boolean shutdown = false;
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.VirtualMachine.VERSION:
            executeVersion(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.CLASSES_BY_SIGNATURE:
            executeClassesBySignature(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.ALL_CLASSES:
            executeAllClasses(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.ALL_THREADS:
            executeAllThreads(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.TOP_LEVEL_THREAD_GROUPS:
            executeTopLevelThreadGroups(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.IDSIZES:
            executeIDsizes(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.DISPOSE:
            shutdown = true;
            executeDispose(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.SUSPEND:
            executeSuspend(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.RESUME:
            executeResume(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.EXIT:
	    shutdown = true;
            executeExit(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.CREATE_STRING:
            executeCreateString(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.CAPABILITIES:
            executeCapabilities(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.CLASS_PATHS:
            executeClassPaths(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.DISPOSE_OBJECTS:
            executeDisposeObjects(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.HOLD_EVENTS:
            executeHoldEvents(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.RELEASE_EVENTS:
            executeReleaseEvents(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.CAPABILITIES_NEW:
            executeCapabilitiesNew(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.REDEFINE_CLASSES:
            executeRedefineClasses(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.SET_DEFAULT_STRATUM:
            executeSetDefaultStratum(bb, os);
            break;
          case JdwpConstants.CommandSet.VirtualMachine.ALL_CLASSES_WITH_GENERIC:
            executeAllClassesWithGeneric(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
            " not found in VirtualMachine Command Set.");
          }
      }
    catch (IOException ex)
      {
        // The DataOutputStream we're using isn't talking to a socket at all
        // So if we throw an IOException we're in serious trouble
        throw new JdwpInternalErrorException(ex);
      }

    return shutdown;
  }

  private void executeVersion(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {

    Properties props = System.getProperties();

    int jdwpMajor = JdwpConstants.Version.MAJOR;
    int jdwpMinor = JdwpConstants.Version.MINOR;
    // The description field is pretty loosely defined
    String description = "JDWP version " + jdwpMajor + "." + jdwpMinor
                         + ", JVM version " + props.getProperty("java.vm.name")
                         + " " + props.getProperty("java.vm.version") + " "
                         + props.getProperty("java.version");
    String vmVersion = props.getProperty("java.version");
    String vmName = props.getProperty("java.vm.name");
    JdwpString.writeString(os, description);
    os.writeInt(jdwpMajor);
    os.writeInt(jdwpMinor);
    JdwpString.writeString(os, vmName);
    JdwpString.writeString(os, vmVersion);
  }

  private void executeClassesBySignature(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    String sig = JdwpString.readString(bb);
    ArrayList allMatchingClasses = new ArrayList();

    // This will be an Iterator over all loaded Classes
    Collection classes = VMVirtualMachine.getAllLoadedClasses();
    Iterator iter = classes.iterator ();

    while (iter.hasNext())
      {
        Class clazz = (Class) iter.next();
        String clazzSig = Signature.computeClassSignature(clazz);
        if (clazzSig.equals(sig))
          allMatchingClasses.add(clazz);
      }

    os.writeInt(allMatchingClasses.size());
    for (int i = 0; i < allMatchingClasses.size(); i++)
      {
        Class clazz = (Class) allMatchingClasses.get(i);
        ReferenceTypeId id = idMan.getReferenceTypeId(clazz);
        id.writeTagged(os);
        int status = VMVirtualMachine.getClassStatus(clazz);
        os.writeInt(status);
      }
  }

  private void executeAllClasses(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    Collection classes = VMVirtualMachine.getAllLoadedClasses();
    os.writeInt(classes.size ());

    Iterator iter = classes.iterator ();
    while (iter.hasNext())
      {
        Class clazz = (Class) iter.next();
        ReferenceTypeId id = idMan.getReferenceTypeId(clazz);
        id.writeTagged(os);
        String sig = Signature.computeClassSignature(clazz);
        JdwpString.writeString(os, sig);
        int status = VMVirtualMachine.getClassStatus(clazz);
        os.writeInt(status);
      }
  }

  private void executeAllThreads(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ThreadGroup jdwpGroup = Thread.currentThread().getThreadGroup();
    ThreadGroup root = getRootThreadGroup(jdwpGroup);

    int numThreads = root.activeCount();
    Thread allThreads[] = new Thread[numThreads];
    root.enumerate(allThreads);

    // We need to loop through for the true count since some threads may have
    // been destroyed since we got
    // activeCount so those spots in the array will be null. As well we must
    // ignore any threads that belong to jdwp
    numThreads = 0;
    for (int i = 0; i < allThreads.length; i++)
      {
        Thread thread = allThreads[i];
        if (thread == null)
          break; // No threads after this point
        if (!thread.getThreadGroup().equals(jdwpGroup))
          numThreads++;
      }

    os.writeInt(numThreads);

    for (int i = 0; i < allThreads.length; i++)
      {
        Thread thread = allThreads[i];
        if (thread == null)
          break; // No threads after this point
        if (!thread.getThreadGroup().equals(jdwpGroup))
          idMan.getObjectId(thread).write(os);
      }
  }

  private void executeTopLevelThreadGroups(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ThreadGroup jdwpGroup = Thread.currentThread().getThreadGroup ();
    ThreadGroup root = getRootThreadGroup(jdwpGroup);

    os.writeInt(1); // Just one top level group allowed?
    idMan.getObjectId(root);
  }

  private void executeDispose(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    // resumeAllThreads isn't sufficient as a thread may have been
    // suspended multiple times, we likely need a way to keep track of how many
    // times a thread has been suspended or else a stronger resume method for
    // this purpose
    // VMVirtualMachine.resumeAllThreads ();

    // Simply shutting down the jdwp layer will take care of the rest of the
    // shutdown other than disabling debugging in the VM
    // VMVirtualMachine.disableDebugging();

    // Don't implement this until we're sure how to remove all the debugging
    // effects from the VM.
    throw new NotImplementedException(
      "Command VirtualMachine.Dispose not implemented");

  }

  private void executeIDsizes(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    os.writeInt(ObjectId.SIZE); // fieldId FIXME
    os.writeInt(ObjectId.SIZE); // methodId FIXME
    os.writeInt(ObjectId.SIZE); // objectId
    os.writeInt(ReferenceTypeId.SIZE); // referenceTypeId
    os.writeInt(VMFrame.SIZE); // frameId
  }

  private void executeSuspend(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    VMVirtualMachine.suspendAllThreads ();
  }

  private void executeResume(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    VMVirtualMachine.resumeAllThreads ();
  }

  private void executeExit(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    int exitCode = bb.getInt();
    System.exit (exitCode);
  }

  private void executeCreateString(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    String string = JdwpString.readString(bb);
    ObjectId stringId = idMan.getObjectId(string);
    
    // Since this string isn't referenced anywhere we'll disable garbage
    // collection on it so it's still around when the debugger gets back to it.
    stringId.disableCollection();
    stringId.write(os);
  }

  private void executeCapabilities(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    os.writeBoolean(VMVirtualMachine.canWatchFieldModification);
    os.writeBoolean(VMVirtualMachine.canWatchFieldAccess);
    os.writeBoolean(VMVirtualMachine.canGetBytecodes);
    os.writeBoolean(VMVirtualMachine.canGetSyntheticAttribute);
    os.writeBoolean(VMVirtualMachine.canGetOwnedMonitorInfo);
    os.writeBoolean(VMVirtualMachine.canGetCurrentContendedMonitor);
    os.writeBoolean(VMVirtualMachine.canGetMonitorInfo);
  }

  private void executeClassPaths(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    String baseDir = System.getProperty("user.dir");
    JdwpString.writeString(os, baseDir);

    // Find and write the classpath
    String classPath = System.getProperty("java.class.path");
    String[] paths = classPath.split(":");

    os.writeInt(paths.length);
    for (int i = 0; i < paths.length; i++)
      JdwpString.writeString(os, paths[i]);

    // Now the bootpath
    String bootPath = System.getProperty("sun.boot.class.path");
    paths = bootPath.split(":");
    os.writeInt(paths.length);
    for (int i = 0; i < paths.length; i++)
      JdwpString.writeString(os, paths[i]);
  }

  private void executeDisposeObjects(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    // Instead of going through the list of objects they give us it's probably
    // better just to find the garbage collected objects ourselves
    //idMan.update();
  }

  private void executeHoldEvents(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    // Going to have to implement a send queue somewhere and do this without
    // triggering events
    // Until then just don't implement
    throw new NotImplementedException(
      "Command VirtualMachine.HoldEvents not implemented");
  }

  // Opposite of executeHoldEvents
  private void executeReleaseEvents(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    throw new NotImplementedException(
      "Command VirtualMachine.ReleaseEvents not implemented");
  }

  private void executeCapabilitiesNew(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    final int CAPABILITIES_NEW_SIZE = 32;

    executeCapabilities(bb, os);
    os.writeBoolean(VMVirtualMachine.canRedefineClasses);
    os.writeBoolean(VMVirtualMachine.canAddMethod);
    os.writeBoolean(VMVirtualMachine.canUnrestrictedlyRedefineClasses);
    os.writeBoolean(VMVirtualMachine.canPopFrames);
    os.writeBoolean(VMVirtualMachine.canUseInstanceFilters);
    os.writeBoolean(VMVirtualMachine.canGetSourceDebugExtension);
    os.writeBoolean(VMVirtualMachine.canRequestVMDeathEvent);
    os.writeBoolean(VMVirtualMachine.canSetDefaultStratum);
    for (int i = 15; i < CAPABILITIES_NEW_SIZE; i++)
      {
	// Future capabilities (currently unused)
	os.writeBoolean(false);
      }
  }

  private void executeRedefineClasses(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    if (!VMVirtualMachine.canRedefineClasses)
      {
	String msg = "redefinition of classes is not supported";
	throw new NotImplementedException(msg);
      }

    int classes = bb.getInt();
    Class[] types = new Class[classes];
    byte[][] bytecodes = new byte[classes][];
    for (int i = 0; i < classes; ++i)
      {
	ReferenceTypeId id = idMan.readReferenceTypeId(bb);
	int classfile = bb.getInt();
	byte[] bytecode = new byte[classfile];
	bb.get(bytecode);
	types[i] = id.getType();
	bytecodes[i] = bytecode;
      }

    VMVirtualMachine.redefineClasses (types, bytecodes);
  }

  private void executeSetDefaultStratum(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    if (!VMVirtualMachine.canSetDefaultStratum)
      {
	String msg = "setting the default stratum is not supported";
	throw new NotImplementedException(msg);
      }

    String stratum = JdwpString.readString(bb);
    VMVirtualMachine.setDefaultStratum(stratum);
  }

  private void executeAllClassesWithGeneric(ByteBuffer bb, DataOutputStream os)
    throws JdwpException
  {
    // We don't handle generics
    throw new NotImplementedException(
      "Command VirtualMachine.AllClassesWithGeneric not implemented");
  }

  /**
   * Find the root ThreadGroup of this ThreadGroup
   */
  private ThreadGroup getRootThreadGroup(ThreadGroup group)
  {
    ThreadGroup parent = group.getParent();

    while (parent != null)
      {
        group = parent;
        parent = group.getParent();
      }
    return group; // This group was the root
  }
}
