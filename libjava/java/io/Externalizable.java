/* Externalizable.java -- Interface for saving and restoring object data
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
  * This interface provides a way that classes can completely control how
  * the data of their object instances  are written and read to and from 
  * streams.  It has two methods which are used to write the data to a stream 
  * and to read the data from a stream.  The read method must read the data 
  * in exactly the way it was written by the write method. 
  * <p>
  * Note that classes which implement this interface must take into account
  * that all superclass data must also be written to the stream as well.  
  * The class implementing this interface must figure out how to make that
  * happen.
  * <p>
  * This interface can be used to provide object persistence.  When an 
  * object is to be stored externally, the <code>writeExternal</code> method is
  * called to save state.  When the object is restored, an instance is
  * created using the default no-argument constructor and the 
  * <code>readExternal</code> method is used to restore the state.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Externalizable extends Serializable
{
  static final long serialVersionUID = -282491828744381764L;

/**
  * This method restores an object's state by reading in the instance data
  * for the object from the passed in stream.  Note that this stream is not
  * a subclass of <code>InputStream</code>, but rather is a class that implements
  * the <code>ObjectInput</code> interface.  That interface provides a mechanism for
  * reading in Java data types from a stream.
  * <p>
  * Note that this method must be compatible with <code>writeExternal</code>.
  * It must read back the exact same types that were written by that
  * method in the exact order they were written.
  * <p>
  * If this method needs to read back an object instance, then the class
  * for that object must be found and loaded.  If that operation fails,
  * then this method throws a <code>ClassNotFoundException</code>
  *
  * @param in An <code>ObjectInput</code> instance for reading in the object state
  *
  * @exception ClassNotFoundException If the class of an object being restored cannot be found
  * @exception IOException If any other error occurs
  */
public abstract void
readExternal(ObjectInput in) throws ClassNotFoundException, IOException;

/*************************************************************************/

/**
  * This method is responsible for writing the instance data of an object
  * to the passed in stream.  Note that this stream is not a subclass of
  * <code>OutputStream</code>, but rather is a class that implements the
  * <code>ObjectOutput</code> interface.  That interface provides a number of methods
  * for writing Java data values to a stream.
  * <p>
  * Not that the implementation of this method must be coordinated with
  * the implementation of <code>readExternal</code>.
  *
  * @param out An <code>ObjectOutput</code> instance for writing the object state
  *
  * @exception IOException If an error occurs
  */
public abstract void
writeExternal(ObjectOutput out) throws IOException;

} // interface Externalizable

