/* XAException.java --
   Copyright (C) 2001, 2002, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package javax.transaction.xa;
 
/**
 * @author Tom Tromey (tromey@redhat.com)
 * @date April 18, 2001
 */

public class XAException extends Exception
{
  public int errorCode;
  public static final int XA_RBBASE = 100;
  public static final int XA_RBROLLBACK = 100;
  public static final int XA_RBCOMMFAIL = 101;
  public static final int XA_RBDEADLOCK = 102;
  public static final int XA_RBINTEGRITY = 103;
  public static final int XA_RBOTHER = 104;
  public static final int XA_RBPROTO = 105;
  public static final int XA_RBTIMEOUT = 106;
  public static final int XA_RBTRANSIENT = 107;
  public static final int XA_RBEND = 107;
  public static final int XA_NOMIGRATE = 9;
  public static final int XA_HEURHAZ = 8;
  public static final int XA_HEURCOM = 7;
  public static final int XA_HEURRB = 6;
  public static final int XA_HEURMIX = 5;
  public static final int XA_RETRY = 4;
  public static final int XA_RDONLY = 3;
  public static final int XAER_ASYNC = -2;
  public static final int XAER_RMERR = -3;
  public static final int XAER_NOTA = -4;
  public static final int XAER_INVAL = -5;
  public static final int XAER_PROTO = -6;
  public static final int XAER_RMFAIL = -7;
  public static final int XAER_DUPID = -8;
  public static final int XAER_OUTSIDE = -9;

  public XAException ()
  {
    super ();
  }

  public XAException (String msg)
  {
    super (msg);
  }

  public XAException (int errcode)
  {
    super ();
    this.errorCode = errcode;
  }
}
