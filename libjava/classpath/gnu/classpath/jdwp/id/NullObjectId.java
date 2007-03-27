/* NullObjectId.java -- special objectId for null values
   Copyright (C) 2007 Free Software Foundation

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

package gnu.classpath.jdwp.id;

import gnu.classpath.jdwp.exception.InvalidObjectException;
import gnu.classpath.jdwp.util.NullObject;

import java.lang.ref.SoftReference;

/**
 * This is a special case of an ObjectId.  When a varaible slot contains
 * null as its value, this is a valid value despite the fact that it does
 * not reference an object.  To represent this, this will always be the id
 * of the NullObject (0).
 *
 * @author Kyle Galloway  <kgallowa@redhat.com>
 */
public class NullObjectId
  extends ObjectId
{
  /**
   * The object class that this id represents
   */
  public static final Class typeClass = NullObject.class;
  
  /**
   * Constructs a new <code>NullObjectId</code>
   */
  public NullObjectId()
  {
    super();
    setId((long) 0);
    _reference = new SoftReference<NullObject>(new NullObject());
    try
      {
        disableCollection();
      }
    catch(InvalidObjectException ex)
      {
        //This will not happen
      }
  }

}
