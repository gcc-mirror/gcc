/* ObjectStreamConstants.java -- Interface containing constant values
   used in reading and writing serialized objects
   Copyright (C) 1998, 1999, 2003, 2006 Free Software Foundation, Inc.

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


package java.io;

/**
 * This interface contains constants that are used in object
 * serialization.  This interface is used by <code>ObjectOutputStream</code>,
 * <code>ObjectInputStream</code>, and <code>ObjectStreamClass</code>.
 * The values for these constants are specified by the Java library
 * specification.
 *
 * @since 1.1
 */
public interface ObjectStreamConstants
{
  /**
   * The serialization stream protocol version 1. This version was
   * the default serialization protocol before JDK 1.2.
   *
   * @see ObjectOutputStream#useProtocolVersion(int)
   * @since 1.2
   */
  int PROTOCOL_VERSION_1 = 1;

  /**
   * The serialization stream protocol version 2. This version is
   * used as the default serialization protocol since JDK 1.2.
   *
   * @see ObjectOutputStream#useProtocolVersion(int)
   * @since 1.2
   */
  int PROTOCOL_VERSION_2 = 2;

  /**
   * The magic number that is written as part of the stream header.
   */
  short STREAM_MAGIC = (short)0xaced;

  /**
   * The stream version number that is written as part of the stream header.
   * Note that this is different from the protocol version that specifies
   * the data format for the stream.
   */
  short STREAM_VERSION = 5;

  /**
   * Token value to designate a <code>null</code> reference in the stream.
   */
  byte TC_NULL = (byte)112;            //0x70

  /**
   * Token value to designate a reference to an already serialized object.
   */
  byte TC_REFERENCE = (byte)113;       //0x71

  /**
   * Token value to designate a class descriptor is next in the stream.
   */
  byte TC_CLASSDESC = (byte)114;       //0x72

  /**
   * Token value to designate a new object is next in the stream.
   */
  byte TC_OBJECT = (byte)115;          //0x73

  /**
   * Token value to designate a new string is next in the stream.
   */
  byte TC_STRING = (byte)116;          //0x74

  /**
   * Token value to designate a new array is next in the stream.
   */
  byte TC_ARRAY = (byte)117;           //0x75

  /**
   * Token reference to designate a reference to a class.
   */
  byte TC_CLASS = (byte)118;           //0x76

  /**
   * Token value to designate a block of primitive data is next in the stream.
   * The next byte in the stream holds the size of the block (in bytes).
   */
  byte TC_BLOCKDATA = (byte)119;       //0x77

  /**
   * Token value to designate the end of a block of primitve data.
   */
  byte TC_ENDBLOCKDATA = (byte)120;    //0x78

  /**
   * Token value to designate a reset of the stream state.
   */
  byte TC_RESET = (byte)121;           //0x79

  /**
   * Token value to designate a long block of primitive data is next in the
   * stream. The next long in the stream holds the size of the block
   * (in bytes).
   */
  byte TC_BLOCKDATALONG = (byte)122;   //0x7A

  /**
   * Token value to designate an exception occured during serialization.
   */
  byte TC_EXCEPTION = (byte)123;       //0x7B

  /**
   * Token value to designate a long string is next in the stream.
   */
  byte TC_LONGSTRING = (byte)124;      //0x7C

  /**
   * Token value to designate a proxy class descriptor is next in the stream.
   */
  byte TC_PROXYCLASSDESC = (byte)125;  //0x7D

  /**
   * Token value to designate an enum constant is next in the stream.
   *
   * @since 1.5
   */
  byte TC_ENUM = (byte)126;            //0x7E

  /**
   * The first token value.
   */
  byte TC_BASE = TC_NULL;

  /**
   * The last token value.
   */
  byte TC_MAX = TC_ENUM;

  /**
   * The first handle that will be assigned to an object, for later references.
   */
  int baseWireHandle = 0x7e0000;

  /**
   * Flag used in <code>ObjectStreamClass</code> to designate that the class
   * defines the <code>writeObject</code> method.
   */
  byte SC_WRITE_METHOD = 0x01;

  /**
   * Flag used in <code>ObjectStreamClass</code> to designate that the class
   * is serializeable.
   */
  byte SC_SERIALIZABLE = 0x02;

  /**
   * Flag used in <code>ObjectStreamClass</code> to designate that the class
   * is externalizable.
   */
  byte SC_EXTERNALIZABLE = 0x04;

  /**
   * Flag used in <code>ObjectStreamClass</code> to designate that
   * externalizable data is written in block data mode.
   *
   * @since 1.2
   */
  byte SC_BLOCK_DATA = 0x08;

  /**
   * Flag used in <code>ObjectStreamClass</code> to designate that the class
   * is an enum constant.
   *
   * @since 1.5
   */
  byte SC_ENUM = 0x10;

  /**
   * Constant for use with a <code>SecurityManager</code> to check if
   * substitution of objects is allowed.
   */
  SerializablePermission SUBSTITUTION_PERMISSION
    = new SerializablePermission("enableSubstitution");

  /**
   * Constant for use with a <code>SecurityManager</code> to check if
   * overriding of the <code>writeObject</code> and <code>readObject</code>
   * methods is allowed.
   */
  SerializablePermission SUBCLASS_IMPLEMENTATION_PERMISSION
    = new SerializablePermission("enableSubclassImplementation");
}
