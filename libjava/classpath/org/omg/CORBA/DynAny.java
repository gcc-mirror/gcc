/* DynAny.java --
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

import org.omg.CORBA.DynAnyPackage.Invalid;
import org.omg.CORBA.DynAnyPackage.InvalidValue;
import org.omg.CORBA.DynAnyPackage.TypeMismatch;

import java.io.Serializable;

/**
 * The DynAny interface provides possibility to access the components of
 * the CORBA object, stored inside the {@link Any}. The {@link Any} itself
 * allows to read, write and pass as parameter the stored value without
 * knowning its exact data type. The DynAny and derived classes additionally
 * allows to access the members of the sequence, structure, union and get the
 * data about enumeration, value type and CORBA <code>fixed</code> without
 * knowing the exact type at the run time. The returned members are also
 * wrapped into DynAny objects, allowing them to be the nested structures.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynAny
  extends org.omg.CORBA.Object
{
  /**
   * Copy one DynAny into another.
   *
   * @param from the DynAny to copy from.
   * @throws Invalid if the source DynAny is invalid.
   */
  void assign(DynAny from)
       throws Invalid;

  /**
   * Clones this DynAny.
   */
  DynAny copy();

  /**
   * Returns the focused component of this DynAny. The DynAny has the internal
   * pointer (reference) that can point to one of its components. The returned
   * DynAny can be used to get or set the value of the focused component.
   * If the DynAny holds a primitive type with no components, this
   * implementation returns <code>this</code>.
   */
  DynAny current_component();

  /**
   * Destroys this DynAny, freeing the used resources. In java, resources
   * are freed by the garbage collectors, so this method typically returns
   * without action.
   */
  void destroy();

  /**
   * Makes a DynAny from the {@link Any}. The passed {@link Any} becomes the
   * enclosed instance of this DynAny, allowing to change/traverse the
   * {@link Any} fields by the {@link DynAny} methods.
   */
  void from_any(Any an_any)
         throws Invalid;

  /**
   * Retrieves the {@link Any}, stored inside this DynAny.
   *
   * @throws TypeMismatch if the typecode of the accessed Any
   * is not the same as the typecode of this DynAny.
   */
  Any get_any()
       throws TypeMismatch;

  /**
   * Extract the boolean value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  boolean get_boolean()
               throws TypeMismatch;

  /**
   * Extract the char value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  char get_char()
         throws TypeMismatch;

  /**
   * Extract the <code>double</code> value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  double get_double()
             throws TypeMismatch;

  /**
   * Extract the <code>float</code> value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  float get_float()
           throws TypeMismatch;

  /**
   * Extract the int (CORBA long) value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  int get_long()
        throws TypeMismatch;

  /**
   * Extract the long (CORBA long long) value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  long get_longlong()
             throws TypeMismatch;

  /**
   * Extract the byte (CORBA octet) value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  byte get_octet()
          throws TypeMismatch;

  /**
   * Extract the CORBA object reference that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  Object get_reference()
                throws TypeMismatch;

  /**
   * Extract the <code>short</code> value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  short get_short()
           throws TypeMismatch;

  /**
   * Extract the string value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  String get_string()
             throws TypeMismatch;

  /**
   * Extract the {@link TypeCode} value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  TypeCode get_typecode()
                 throws TypeMismatch;

  /**
   * Extract the unsigned int (CORBA ulong) value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  int get_ulong()
         throws TypeMismatch;

  /**
   * Extract the unsingel long (CORBA unsigned long long )value that
   * is expected to be stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  long get_ulonglong()
              throws TypeMismatch;

  /**
   * Extract the unsigned short value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  short get_ushort()
            throws TypeMismatch;

  /**
   * Extract the value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  Serializable get_val()
                throws TypeMismatch;

  /**
   * Extract the wide (usually UTF-16) character value that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  char get_wchar()
          throws TypeMismatch;

  /**
   * Extract the wide (usually UFT-16) string that is expected to be
   * stored in this DynAny.
   *
   * @throws TypeMismatch if this DynAny holds the value of the
   * different type.
   */
  String get_wstring()
              throws TypeMismatch;

  /**
   * Insert the {@link Any} value into the enclosed
   * {@link Any} inside this DynAny.
   *
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_any(Any an_any)
           throws InvalidValue;

  /**
   * Insert the boolean value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_boolean(boolean a_x)
               throws InvalidValue;

  /**
   * Insert the char value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_char(char a_x)
            throws InvalidValue;

  /**
   * Insert the double value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_double(double a_x)
              throws InvalidValue;

  /**
   * Insert the float value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_float(float a_x)
             throws InvalidValue;

  /**
   * Insert the int (CORBA long) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_long(int a_x)
            throws InvalidValue;

  /**
   * Insert the long (CORBA long long) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_longlong(long a_x)
                throws InvalidValue;

  /**
   * Insert the byte (CORBA octet) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_octet(byte a_x)
             throws InvalidValue;

  /**
   * Insert the object reference into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_reference(Object a_x)
                 throws InvalidValue;

  /**
   * Insert the <code>short</code> value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_short(short a_x)
             throws InvalidValue;

  /**
   * Insert the string value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_string(String a_x)
              throws InvalidValue;

  /**
   * Insert the {@link TypeCode} value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_typecode(TypeCode a_x)
                throws InvalidValue;

  /**
   * Insert the int (CORBA unsinged long) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_ulong(int a_x)
             throws InvalidValue;

  /**
   * Insert the long (CORBA unsigned long long) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_ulonglong(long a_x)
                 throws InvalidValue;

  /**
   * Insert the short (CORBA unsigned short) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_ushort(short a_x)
              throws InvalidValue;

  /**
   * Insert the value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_val(Serializable a_x)
           throws InvalidValue;

  /**
   * Insert the wide char (usually UTF-16) value into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_wchar(char a_x)
             throws InvalidValue;

  /**
   * Insert the wide string (usually UTF-16) into the enclosed
   * {@link Any} inside this DynAny
   * @param a_x the value being inserted.
   * @throws InvalidValue if the value type does not match the
   * typecode of the enclosed {@link Any}.
   */
  void insert_wstring(String a_x)
               throws InvalidValue;

  /**
   * Advances the internal pointer, described in the {@link current_component},
   * one position forward.
   *
   * @return true if the pointer now points to the new component,
   * false if there are no more components of this DynAny holds
   * a basic type that is not divided into components.
   */
  boolean next();

  /**
   * Moves the internal pointer, described in the {@link current_component},
   * to the first component.
   */
  void rewind();

  /**
   * Moves the internal pointer, described in the {@link current_component},
   * to the given position.
   *
   * @param p the number of the internal component on that the internal
   * pointer must be focused.
   *
   * @return true on success or false if there is no component with the
   * given number. If the DynAny holds the basic type, this method returs
   * false p values other than 0.
   */
  boolean seek(int p);

  /**
   * Returns the enclosed {@link Any}.
   *
   * @return the enclosed {@link Any}.
   */
  Any to_any()
      throws Invalid;

  /**
   * Returns the typecode of the object, inserted into this
   * DynAny.
   *
   * @return the typecode of the inserted {@link Any} or null typecode
   * if no {@link Any has been yet inserted}.
   */
  TypeCode type();
}
