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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


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
