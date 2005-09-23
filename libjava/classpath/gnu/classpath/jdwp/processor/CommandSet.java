/* CommandSet.java -- An interface defining JDWP Command Sets
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

import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.VMIdManager;

import java.io.DataOutputStream;
import java.nio.ByteBuffer;

/**
 * A class representing a JDWP Command Set. This class serves as a generic
 * interface for all Command Sets types used by JDWP.
 *
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public abstract class CommandSet
{
  /**
   * The VM's ID manager
   */
  protected final VMIdManager idMan = VMIdManager.getDefault ();

  /**
   * Runs the given command with the data in distr and writes the data for the
   * reply packet to ostr.
   * 
   * @param bb holds the data portion of the Command Packet
   * @param os data portion of the Reply Packet will be written here
   * @param command the command field of the Command Packet
   * @return true if the JDWP layer should shut down in response to this packet
   * @throws JdwpException command wasn't carried out successfully
   */
  public abstract boolean runCommand(ByteBuffer bb, DataOutputStream os,
				     byte command) 
    throws JdwpException;
}
