/* CupsIppOperation.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.print;


/**
 * Operations as provided by CUPS up to version 1.1
 * <p>
 * See: CUPS Implementation of IPP, chapter 3.2<br>
 * http://www.cups.org/doc-1.1/ipp.html
 * </p>
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class CupsIppOperation
{

  /** Get the default destination - since CUPS 1.0 */
  public static final int CUPS_GET_DEFAULT = 0x4001;  
 
  /** Get all of the available printers - since CUPS 1.0 */
  public static final int CUPS_GET_PRINTERS =  0x4002;
  
  /** Add or modify a printer - since CUPS 1.0 */
  public static final int CUPS_ADD_MODIFY_PRINTER =  0x4003;
  
  /** Delete a printer - since CUPS 1.0 */
  public static final int CUPS_DELETE_PRINTER =  0x4004;
  
  /** Get all of the available printer classes - since CUPS 1.0 */
  public static final int CUPS_GET_CLASSES =  0x4005;
  
  /** Add or modify a printer class - since CUPS 1.0 */
  public static final int CUPS_ADD_MODIFY_CLASS =  0x4006;
  
  /** Delete a printer class - since CUPS 1.0 */
  public static final int CUPS_DELETE_CLASS =  0x4007;
  
  /** Accept jobs on a printer or printer class - since CUPS 1.0 */
  public static final int CUPS_ACCEPT_JOBS = 0x4008;
  
  /** Reject jobs on a printer or printer class - since CUPS 1.0 */
  public static final int CUPS_REJECT_JOBS = 0x4009;
  
  /** Set the default destination - since CUPS 1.0 */
  public static final int CUPS_SET_DEFAULT = 0x400A;
  
  /** Get all of the available PPDs - since CUPS 1.1 */
  public static final int CUPS_GET_DEVICES = 0x400B;
  
  /** Get all of the available PPDs - since CUPS 1.1 */
  public static final int CUPS_GET_PPDS = 0x400C;
  
  /** Move a job to a different printer - since CUPS 1.1 */
  public static final int CUPS_MOVE_JOB = 0x400D;
  
  
  private CupsIppOperation()
  { 
    // not to be instantiated
  }
  
}
