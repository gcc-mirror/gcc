/* ObjectStreamConstants.java -- Interface containing constant values
   used in reading and writing serialized objects
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
   This interface contains constants that are used in object
   serialization.  This interface is used by ObjectOutputStream,
   ObjectInputStream, ObjectStreamClass, and possibly other classes.
   The values for these constants are specified in Javasoft's "Object
   Serialization Specification" TODO: add reference
*/
public interface ObjectStreamConstants
{
  public final static int PROTOCOL_VERSION_1 = 1;
  public final static int PROTOCOL_VERSION_2 = 2;

  final static short STREAM_MAGIC = (short)0xaced;
  final static short STREAM_VERSION = 5;

  final static byte TC_NULL = (byte)112;            //0x70
  final static byte TC_REFERENCE = (byte)113;       //0x71
  final static byte TC_CLASSDESC = (byte)114;       //0x72
  final static byte TC_OBJECT = (byte)115;          //0x73
  final static byte TC_STRING = (byte)116;          //0x74
  final static byte TC_ARRAY = (byte)117;           //0x75
  final static byte TC_CLASS = (byte)118;           //0x76
  final static byte TC_BLOCKDATA = (byte)119;       //0x77
  final static byte TC_ENDBLOCKDATA = (byte)120;    //0x78
  final static byte TC_RESET = (byte)121;           //0x79
  final static byte TC_BLOCKDATALONG = (byte)122;   //0x7A
  final static byte TC_EXCEPTION = (byte)123;       //0x7B
  final static byte TC_LONGSTRING = (byte)124;      //0x7C
  final static byte TC_PROXYCLASSDESC = (byte)125;  //0x7D

  final static byte TC_BASE = TC_NULL;
  final static byte TC_MAX = TC_PROXYCLASSDESC;

  final static int baseWireHandle = 0x7e0000;

  final static byte SC_WRITE_METHOD = 0x01;
  final static byte SC_SERIALIZABLE = 0x02;
  final static byte SC_EXTERNALIZABLE = 0x04;
  final static byte SC_BLOCK_DATA = 0x08;

  final static SerializablePermission SUBSTITUTION_PERMISSION
    = new SerializablePermission("enableSubstitution");

  final static SerializablePermission SUBCLASS_IMPLEMENTATION_PERMISSION
    = new SerializablePermission("enableSubclassImplementation");
}
