/* SYNC_WITH_TRANSPORT.java --
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


package org.omg.Messaging;


/**
 * A CORBA synchronization mode, defining how far the request shall
 * progress before control is returned to the client for one way
 * operations (when no response is required). OMG specification
 * defines the following modes:
 * <ul>
 * <li>
 * SYNC_NONE (0) - The ORB returns control before sending the request message.
 * </li><li>
 * SYNC_WITH_TRANSPORT (1) - The ORB returns control to the client only after the
 * transport has accepted the request message.
 * </li><li>
 * SYNC_WITH_SERVER (2) - The ORB waits for the reply message from the
 * server side ORB.
 * </li><li>
 * SYNC_WITH_TARGET (3) is equivalent for the synchronous, no one
 * way operations. It is the most realiable, also the slowest one.
 * </ul>
 * The java API specification up till 1.4 inclusive defines only one
 * constant, SYNC_WITH_TRANSPORT.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface SYNC_WITH_TRANSPORT
{
  /**
   * The mode, indicating, that the ORB returns control to the client only
   * after the transport has accepted the request message. There is stil no
   * guarantee that the request will be delivered, but the server should
   * send a reply message.
   */
  short value = 1;
}
