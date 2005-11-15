/* NO_RESOURCES.java --
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


package org.omg.CORBA;

import java.io.Serializable;

/**
 * Means that the ORB has reached some general resource limitation like maximal
 * number of the opened connections.
 * 
 * In GNU Classpath, this exception may have the following minor codes:
 * 
 * <table border="1">
 * <tr>
 * <td>Hex</td>
 * <td>Dec</td>
 * <td>Minor</td>
 * <td>Name</td>
 * <td>Case</td>
 * </tr>
 * <tr>
 * <td>47430014</td>
 * <td>1195573268</td>
 * <td>20</td>
 * <td>Ports</td>
 * <td>No more free ports available for the new objects. The port control, if
 * turned on, prevents malicios client from knocking the server out by suddenly
 * requiring to allocate a very large number of objects.</td>
 * </tr>
 * <tr>
 * <td>47430015</td>
 * <td>1195573269</td>
 * <td>21</td>
 * <td>Threads</td>
 * <td> Too many parallel calls (too many parallel threads). The thread control,
 * if turned on, prevents malicios client from knocking the server out by
 * suddenly submitting a very large number of requests. </td>
 * </tr>
 * </table> 
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class NO_RESOURCES
  extends SystemException
  implements Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 8129246118235803597L;

  /**
   * Creates a NO_RESOURCES with the default minor code of 0, completion state
   * COMPLETED_NO and the given explaining message.
   * 
   * @param reasom the explaining message.
   */
  public NO_RESOURCES(String message)
  {
    super(message, 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Creates NO_RESOURCES with the default minor code of 0 and a completion
   * state COMPLETED_NO.
   */
  public NO_RESOURCES()
  {
    super("", 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Creates a NO_RESOURCES exception with the specified minor code and
   * completion status.
   * 
   * @param a_minor additional error code.
   * @param a_completed the method completion status.
   */
  public NO_RESOURCES(int a_minor, CompletionStatus a_completed)
  {
    super("", a_minor, a_completed);
  }

  /**
   * Created NO_RESOURCES exception, providing full information.
   * 
   * @param a_reason explaining message.
   * @param a_minor additional error code (the "minor").
   * @param a_completed the method completion status.
   */
  public NO_RESOURCES(String a_reason, int a_minor, CompletionStatus a_completed)
  {
    super(a_reason, a_minor, a_completed);
  }
}
