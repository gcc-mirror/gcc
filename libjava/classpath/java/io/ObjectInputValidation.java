/* ObjectInputValidation.java -- Validate an object
   Copyright (C) 1998, 2003 Free Software Foundation, Inc.

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
  * This class allows an object to validate that it is valid after
  * deserialization has run completely for it and all dependent objects.
  * This allows an object to determine if it is invalid even if all
  * state data was correctly deserialized from the stream.  It can also
  * be used to perform re-initialization type activities on an object
  * after it has been completely deserialized.
  *
  * Since this method functions as a type of callback, it must be
  * registered through <code>ObjectInputStream.registerValidation</code>
  * in order to be invoked.  This is typically done in the
  * <code>readObject</code> method.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  *
  * @see ObjectInputStream#registerValidation
  */
public interface ObjectInputValidation
{
  /**
    * This method is called to validate an object after serialization
    * is complete.  If the object is invalid an exception is thrown.
    *
    * @exception InvalidObjectException If the object is invalid
    */
  void validateObject() throws InvalidObjectException;
}
