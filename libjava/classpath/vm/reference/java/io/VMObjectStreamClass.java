/* VMObjectStreamClass.java -- VM helper functions for ObjectStreamClass
   Copyright (C) 2003, 2005  Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;
import java.lang.reflect.Field;

final class VMObjectStreamClass
{
  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javaio");
      }
  }

  /**
    * Returns true if CLAZZ has a static class initializer
    * (a.k.a. <clinit>).
    */
  static native boolean hasClassInitializer (Class clazz);

  /**
   * Sets the value of the specified field. This method handles "double".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setDoubleNative(Field field, Object obj, double val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "float".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setFloatNative(Field field, Object obj, float val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "long".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setLongNative(Field field, Object obj, long val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "int".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setIntNative(Field field, Object obj, int val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "short".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setShortNative(Field field, Object obj, short val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "char".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setCharNative(Field field, Object obj, char val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "byte".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setByteNative(Field field, Object obj, byte val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "boolean".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setBooleanNative(Field field, Object obj, boolean val)
    throws InternalError;

  /**
   * Sets the value of the specified field. This method handles "object".
   * Warning ! The types are not truely checked here and final values may be
   * assigned.
   *
   * @param field Field to set the value.
   * @param obj Instance which will have its field set.
   * @param val Value to put in the field.
   */
  static native void setObjectNative(Field field, Object obj, Object val)
    throws InternalError;

}
