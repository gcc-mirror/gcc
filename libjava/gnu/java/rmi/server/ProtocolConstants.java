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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package gnu.java.rmi.server;

public interface ProtocolConstants {

final public static int PROTOCOL_HEADER = 0x4a524d49; // JRMI
final public static int PROTOCOL_VERSION = 2;

final public static int STREAM_PROTOCOL = 0x4b;
final public static int SINGLE_OP_PROTOCOL = 0x4c;
final public static int MULTIPLEX_PROTOCOL = 0x4d;

final public static int PROTOCOL_ACK = 0x4e;
final public static int PROTOCOL_NACK = 0x4f;

final public static int MESSAGE_CALL = 0x50;
final public static int MESSAGE_CALL_ACK = 0x51;
final public static int MESSAGE_PING = 0x52;
final public static int MESSAGE_PING_ACK = 0x53;
final public static int MESSAGE_DGCACK = 0x54;

final public static int RETURN_ACK = 0x01;
final public static int RETURN_NACK = 0x02;

final public static int DEFAULT_PROTOCOL = STREAM_PROTOCOL;

};
