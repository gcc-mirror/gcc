/* Replaceable.java -- Replace an object with another object
   Copyright (C) 1998 Free Software Foundation, Inc.

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
executable file might be covered by the GNU General Public License. */


package java.io;

/**
  * This interface is used to indicate that an object may want to have
  * another object serialized instead of itself.  It contains one method
  * that is to be called when an object is to be serialized.  That method
  * is reponsible for returning the real object that should be serialized
  * instead of object being queried.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Replaceable extends Serializable
{

/**
  * This method returns the object that should be serialized instead of
  * this object
  *
  * @return The real object that should be serialized
  */
public abstract Object
writeReplace();

} // interface Replaceable

