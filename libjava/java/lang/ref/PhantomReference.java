/* java.lang.ref.PhantomReference
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.lang.ref;

/**
 * A phantom reference is useful, to get notified, when an object got
 * finalized.  You can't access that object though, since it is
 * finalized.  This is the reason, why <code>get()</code> always
 * returns null.
 *
 * @author Jochen Hoenicke 
 */
public class PhantomReference 
  extends Reference
{
  /**
   * Creates a new phantom reference.
   * @param referent the object that should be watched.
   * @param q the queue that should be notified, if the referent was
   * finalized.  This mustn't be <code>null</code>.
   * @exception NullPointerException if q is null.
   */
  public PhantomReference(Object referent, ReferenceQueue q)
  {
    super(referent, q);
  }
  
  /**
   * Returns the object, this reference refers to.
   * @return <code>null</code>, since the refered object may be
   * finalized and thus not accessible.  
   */
  public Object get()
  {
    return null;
  }
}
