/* ObjectIdentityWrapper.java -- Wrapper class used to override equals()
    and hashCode() to be as discriminating as possible
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


package gnu.java.io;

/**
   This class is a thin wrapper around <code>Object</code> that makes
   the methods <code>hashCode()</code> and <code>equals(Object)</code>
   as discriminating as possible.
*/
public class ObjectIdentityWrapper
{

  /**
     Constructs a <code>ObjectIdentityWrapper</code> that is wrapped
     around o.
  */
  public ObjectIdentityWrapper( Object o )
  {
    object = o;
  }

  /**
     Uses <code>System.identityHashCode(Object)</code> to compute a
     hash code for the object wrapped by this
     <code>ObjectIdentityWrapper</code>.

     @see java.lang.System#identityHashCode(java.lang.Object)
     @see java.util.Hashtable
     @see java.lang.Object#hashCode()
  */
  public int hashCode()
  {
    return System.identityHashCode( object );
  }

  /**
     Uses the <code>==</code> operator to test for equality between
     the object wrapped by this <code>ObjectIdentityWrapper</code> and
     the object wrapped by the <code>ObjectIdentityWrapper</code> o.
     Returns false if o is not a <code>ObjectIdentityWrapper</code>.

     @see java.util.Hashtable
     @see java.lang.Object#equals()
  */
  public boolean equals( Object o )
  {
    if( o instanceof ObjectIdentityWrapper )
      return object == ((ObjectIdentityWrapper)o).object;
    else
      return false;
  }

  public String toString()
  {
    return "ObjectIdentityWrapper< " + object + ", " + hashCode() + " >";
  }

  /**
     The <code>Object</code> wrapped by this
     <code>ObjectIdentityWrapper</code>.
  */
  public Object object;
}
