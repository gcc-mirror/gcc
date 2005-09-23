/* ThreadReferenceCommandSet.java -- class to implement the ThreadReference
   Command Set Copyright (C) 2005 Free Software Foundation
 
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
import gnu.classpath.jdwp.VMFrame;
import gnu.classpath.jdwp.VMVirtualMachine;
import gnu.classpath.jdwp.exception.InvalidObjectException;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.id.ThreadId;
import gnu.classpath.jdwp.util.JdwpString;
import gnu.classpath.jdwp.util.Location;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;

/**
 * A class representing the ThreadReference Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ThreadReferenceCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
      throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ThreadReference.NAME:
            executeName(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.SUSPEND:
            executeSuspend(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.RESUME:
            executeResume(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.STATUS:
            executeStatus(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.THREAD_GROUP:
            executeThreadGroup(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.FRAMES:
            executeFrames(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.FRAME_COUNT:
            executeFrameCount(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.OWNED_MONITORS:
            executeOwnedMonitors(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.CURRENT_CONTENDED_MONITOR:
            executeCurrentContendedMonitor(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.STOP:
            executeStop(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.INTERRUPT:
            executeInterrupt(bb, os);
            break;
          case JdwpConstants.CommandSet.ThreadReference.SUSPEND_COUNT:
            executeSuspendCount(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command + 
              " not found in Thread Reference Command Set.");
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

  private void executeName(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    JdwpString.writeString(os, thread.getName());
  }

  private void executeSuspend(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    VMVirtualMachine.suspendThread(thread);
  }

  private void executeResume(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    VMVirtualMachine.suspendThread(thread);
  }

  private void executeStatus(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    int threadStatus = VMVirtualMachine.getThreadStatus(thread);
    // There's only one possible SuspendStatus...
    int suspendStatus = JdwpConstants.SuspendStatus.SUSPENDED;

    os.writeInt(threadStatus);
    os.writeInt(suspendStatus);
  }

  private void executeThreadGroup(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    ThreadGroup group = thread.getThreadGroup();
    ObjectId groupId = idMan.getObjectId(group);
    groupId.write(os);
  }

  private void executeFrames(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    int startFrame = bb.getInt();
    int length = bb.getInt();

    ArrayList frames = VMVirtualMachine.getFrames(thread, startFrame, length);
    os.writeInt(frames.size());
    for (int i = 0; i < frames.size(); i++)
      {
        VMFrame frame = (VMFrame) frames.get(i);
        os.writeLong(frame.getId());
        Location loc = frame.getLocation();
        loc.write(os);
      }
  }

  private void executeFrameCount(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();

    int frameCount = VMVirtualMachine.getFrameCount(thread);
    os.writeInt(frameCount);
  }

  private void executeOwnedMonitors(ByteBuffer bb, DataOutputStream os)
      throws JdwpException
  {
    // This command is optional, determined by VirtualMachines CapabilitiesNew
    // so we'll leave it till later to implement
    throw new NotImplementedException(
      "Command OwnedMonitors not implemented.");
  }

  private void executeCurrentContendedMonitor(ByteBuffer bb,
                                              DataOutputStream os)
      throws JdwpException
  {
    // This command is optional, determined by VirtualMachines CapabilitiesNew
    // so we'll leave it till later to implement
    throw new NotImplementedException(
      "Command CurrentContentedMonitors not implemented.");
  }

  private void executeStop(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    ObjectId exception = idMan.readObjectId(bb);
    Throwable throwable = (Throwable) exception.getObject();
    thread.stop (throwable);
  }

  private void executeInterrupt(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    thread.interrupt();
  }

  private void executeSuspendCount(ByteBuffer bb, DataOutputStream os)
      throws JdwpException, IOException
  {
    ThreadId tid = (ThreadId) idMan.readObjectId(bb);
    Thread thread = tid.getThread();
    int suspendCount = VMVirtualMachine.getSuspendCount(thread);
    os.writeInt(suspendCount);
  }
}
