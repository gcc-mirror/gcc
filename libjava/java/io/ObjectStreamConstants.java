/* ObjectStreamConstants.java -- Interface containing constant values
   used in reading and writing serialized objects
   Copyright (C) 1998, 1999, 2003 Free Software Foundation, Inc.

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


package java.io;

/**
 * This interface contains constants that are used in object
 * serialization.  This interface is used by <code>ObjectOutputStream</code>,
 * <code>ObjectInputStream</code>, and <code>ObjectStreamClass</code>.
 * The values for these constants are specified by the Java library
 * specification.
 */
public interface ObjectStreamConstants
{
  // FIXME: Javadoc comment these values.
  int PROTOCOL_VERSION_1 = 1;
  int PROTOCOL_VERSION_2 = 2;

  short STREAM_MAGIC = (short)0xaced;
  short STREAM_VERSION = 5;

  byte TC_NULL = (byte)112;            //0x70
  byte TC_REFERENCE = (byte)113;       //0x71
  byte TC_CLASSDESC = (byte)114;       //0x72
  byte TC_OBJECT = (byte)115;          //0x73
  byte TC_STRING = (byte)116;          //0x74
  byte TC_ARRAY = (byte)117;           //0x75
  byte TC_CLASS = (byte)118;           //0x76
  byte TC_BLOCKDATA = (byte)119;       //0x77
  byte TC_ENDBLOCKDATA = (byte)120;    //0x78
  byte TC_RESET = (byte)121;           //0x79
  byte TC_BLOCKDATALONG = (byte)122;   //0x7A
  byte TC_EXCEPTION = (byte)123;       //0x7B
  byte TC_LONGSTRING = (byte)124;      //0x7C
  byte TC_PROXYCLASSDESC = (byte)125;  //0x7D

  byte TC_BASE = TC_NULL;
  byte TC_MAX = TC_PROXYCLASSDESC;

  int baseWireHandle = 0x7e0000;

  byte SC_WRITE_METHOD = 0x01;
  byte SC_SERIALIZABLE = 0x02;
  byte SC_EXTERNALIZABLE = 0x04;
  byte SC_BLOCK_DATA = 0x08;

  SerializablePermission SUBSTITUTION_PERMISSION
    = new SerializablePermission("enableSubstitution");

  SerializablePermission SUBCLASS_IMPLEMENTATION_PERMISSION
    = new SerializablePermission("enableSubclassImplementation");
}

