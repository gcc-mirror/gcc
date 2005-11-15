/* BAD_OPERATION.java --
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
 * Means that the object exists but does not support the operation that was
 * invoked on it.
 * 
 * In GNU Classpath, this exception may have the following Minor codes:
 * 
 * <table border="1">
 * <tr>
 * <th>Hex</th>
 * <th>Dec</th>
 * <th>Minor</th>
 * <th>Name</th>
 * <th>Case</th>
 * </tr>
 * <tr>
 * <td>47430000</td>
 * <td>1195573248 </td>
 * <td>0</td>
 * <td>Method</td>
 * <td> The remote side requested to invoke the method that is not available on
 * that target (client and server probably disagree in the object definition).
 * This code is set when the problem arises in the Classpath core; the idlj and
 * rmic may generate the user code that sets 0x0 or other value.</td>
 * </tr>
 * <tr>
 * <td>47430009</td>
 * <td>1195573257</td>
 * <td>9</td>
 * <td>Any</td>
 * <td> Attempt to extract from the Any value of the different type that was
 * stored into that Any. </td>
 * </tr>
 * <tr>
 * <td>4743000a</td>
 * <td>1195573258</td>
 * <td>10</td>
 * <td>Activation</td>
 * <td>Failed to activate the inactive object due any reason.</td>
 * </tr>
 * <tr>
 * <td>4743000b</td>
 * <td>1195573259</td>
 * <td>11</td>
 * <td>Policy</td>
 * <td> The policies, applying to ORB or POA prevent the requested operation.
 * </td>
 * </tr>
 * <tr>
 * <td>4743000c</td>
 * <td>1195573260</td>
 * <td>12</td>
 * <td>Socket</td>
 * <td> Socket related errors like failure to open socket on the expected port.</td>
 * </tr>
 * <tr>
 * <td>4743000e</td>
 * <td>1195573262</td>
 * <td>14</td>
 * <td>Enumeration</td>
 * <td> The passed value for enumeration is outside the valid range for that
 * enumeration. </td>
 * </tr>
 * <tr>
 * <td>4743000f</td>
 * <td>1195573263</td>
 * <td>15</td>
 * <td>PolicyType</td>
 * <td> The passed policy code is outside the valid range of the possible
 * policies for the given policy type. </td>
 * </tr>
 * </table>
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class BAD_OPERATION
  extends SystemException
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1654621651720499682L;

  /**
   * Creates a BAD_OPERATION with the default minor code of 0, completion state
   * COMPLETED_NO and the given explaining message.
   * 
   * @param reasom the explaining message.
   */
  public BAD_OPERATION(String message)
  {
    super(message, 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Creates BAD_OPERATION with the default minor code of 0 and a completion
   * state COMPLETED_NO.
   */
  public BAD_OPERATION()
  {
    super("", 0, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Creates a BAD_OPERATION exception with the specified minor code and
   * completion status.
   * 
   * @param minor additional error code.
   * @param completed the method completion status.
   */
  public BAD_OPERATION(int minor, CompletionStatus completed)
  {
    super("", minor, completed);
  }

  /**
   * Created BAD_OPERATION exception, providing full information.
   * 
   * @param reason explaining message.
   * @param minor additional error code (the "minor").
   * @param completed the method completion status.
   */
  public BAD_OPERATION(String reason, int minor, CompletionStatus completed)
  {
    super(reason, minor, completed);
  }
}
