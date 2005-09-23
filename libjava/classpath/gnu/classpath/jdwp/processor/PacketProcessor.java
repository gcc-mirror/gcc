/* PacketProcessor.java -- a thread which processes command packets
   from the debugger
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

import gnu.classpath.jdwp.Jdwp;
import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.transport.JdwpCommandPacket;
import gnu.classpath.jdwp.transport.JdwpConnection;
import gnu.classpath.jdwp.transport.JdwpPacket;
import gnu.classpath.jdwp.transport.JdwpReplyPacket;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.security.PrivilegedAction;

/**
 * This class is responsible for processing packets from the
 * debugger. It waits for an available packet from the connection
 * ({@link gnu.classpath.jdwp.transport.JdwpConnection}) and then
 * processes the packet and any reply.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class PacketProcessor
  implements PrivilegedAction
{
  // The connection to the debugger
  private JdwpConnection _connection;
  
  // Shutdown this thread?
  private boolean _shutdown;
  
  // A Mapping of the command set (Byte) to the specific CommandSet
  private CommandSet[] _sets;

  // Contents of the ReplyPackets data field
  private ByteArrayOutputStream _outputBytes;

  // Output stream around _outputBytes
  private DataOutputStream _os;
  
  /**
   * Constructs a new <code>PacketProcessor</code> object
   * Connection must be validated before getting here!
   *
   * @param con  the connection
   */
  public PacketProcessor (JdwpConnection con)
  {
    _connection = con;
    _shutdown = false;
    
    // MAXIMUM is the value of the largest command set we may receive 
    _sets = new CommandSet[JdwpConstants.CommandSet.MAXIMUM + 1];
    _outputBytes = new ByteArrayOutputStream();
    _os = new DataOutputStream (_outputBytes);

    // Create all the Command Sets and add them to our array
    _sets[JdwpConstants.CommandSet.VirtualMachine.CS_VALUE] =
      new VirtualMachineCommandSet();
    _sets[JdwpConstants.CommandSet.ReferenceType.CS_VALUE] =
      new ReferenceTypeCommandSet();
    _sets[JdwpConstants.CommandSet.ClassType.CS_VALUE] =
      new ClassTypeCommandSet();
    _sets[JdwpConstants.CommandSet.ArrayType.CS_VALUE] =
      new ArrayTypeCommandSet();
    _sets[JdwpConstants.CommandSet.InterfaceType.CS_VALUE] =
      new InterfaceTypeCommandSet();
    _sets[JdwpConstants.CommandSet.Method.CS_VALUE] =
      new MethodCommandSet();
    _sets[JdwpConstants.CommandSet.Field.CS_VALUE] =
      new FieldCommandSet();
    _sets[JdwpConstants.CommandSet.ObjectReference.CS_VALUE] =
      new ObjectReferenceCommandSet();
    _sets[JdwpConstants.CommandSet.StringReference.CS_VALUE] =
      new StringReferenceCommandSet();
    _sets[JdwpConstants.CommandSet.ThreadReference.CS_VALUE] =
      new ThreadReferenceCommandSet();
    _sets[JdwpConstants.CommandSet.ThreadGroupReference.CS_VALUE] =
      new ThreadGroupReferenceCommandSet();
    _sets[JdwpConstants.CommandSet.ArrayReference.CS_VALUE] =
      new ArrayReferenceCommandSet();
    _sets[JdwpConstants.CommandSet.ClassLoaderReference.CS_VALUE] =
      new ClassLoaderReferenceCommandSet();
    _sets[JdwpConstants.CommandSet.EventRequest.CS_VALUE] =
      new EventRequestCommandSet();
    _sets[JdwpConstants.CommandSet.StackFrame.CS_VALUE] =
      new StackFrameCommandSet();
    _sets[JdwpConstants.CommandSet.ClassObjectReference.CS_VALUE] =
      new ClassObjectReferenceCommandSet();
  }
  
  /**
   * Main run routine for this thread. Will loop getting packets
   * from the connection and processing them.
   */
  public Object run ()
  {
    try
      {
        while (!_shutdown)
          {
            _processOnePacket ();
          }
      }
    catch (IOException ex)
      {
        ex.printStackTrace();
      }
    // Time to shutdown, tell Jdwp to shutdown
    Jdwp.getDefault().shutdown();
    return null;
  }
  
  /**
   * Shutdown the packet processor
   */
  public void shutdown ()
  {
    _shutdown = true;
  }
  
  // Helper function which actually does all the work of waiting
  // for a packet and getting it processed.
  private void _processOnePacket ()
    throws IOException
  {
    JdwpPacket pkt = _connection.getPacket ();
    
    if (!(pkt instanceof JdwpCommandPacket))
      {
        // We're not supposed to get these from the debugger!
        // Drop it on the floor
        return;
      }
    
    if (pkt != null)
      {
        JdwpCommandPacket commandPkt = (JdwpCommandPacket) pkt;
        JdwpReplyPacket reply = new JdwpReplyPacket(commandPkt);
        
        // Reset our output stream
        _outputBytes.reset();
        
        // Create a ByteBuffer around the command packet 
        ByteBuffer bb = ByteBuffer.wrap(commandPkt.getData());
        byte command = commandPkt.getCommand();
        byte commandSet = commandPkt.getCommandSet();
        
        CommandSet set = null;
        try
          {
            // There is no command set with value 0
            if (commandSet > 0 && commandSet < _sets.length)
              {
                set = _sets[commandPkt.getCommandSet()];
              }
            if (set != null) 
              {
                _shutdown = set.runCommand(bb, _os, command);
                reply.setData(_outputBytes.toByteArray());
              }
            else
              {
                // This command set wasn't in our tree
                reply.setErrorCode(JdwpConstants.Error.NOT_IMPLEMENTED);
              }
          }
          catch (JdwpException ex)
            {
            reply.setErrorCode(ex.getErrorCode ());
            }
          _connection.sendPacket (reply);
      }
  }
}
