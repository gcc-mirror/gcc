/*
  Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

package gnu.java.rmi.server;

public interface ProtocolConstants
{
  int PROTOCOL_HEADER = 0x4a524d49; // JRMI
  int PROTOCOL_VERSION = 2;

  int STREAM_PROTOCOL = 0x4b;
  int SINGLE_OP_PROTOCOL = 0x4c;
  int MULTIPLEX_PROTOCOL = 0x4d;

  int PROTOCOL_ACK = 0x4e;
  int PROTOCOL_NACK = 0x4f;

  int MESSAGE_CALL = 0x50;
  int MESSAGE_CALL_ACK = 0x51;
  int MESSAGE_PING = 0x52;
  int MESSAGE_PING_ACK = 0x53;
  int MESSAGE_DGCACK = 0x54;

  int RETURN_ACK = 0x01;
  int RETURN_NACK = 0x02;

  int DEFAULT_PROTOCOL = STREAM_PROTOCOL;
}
