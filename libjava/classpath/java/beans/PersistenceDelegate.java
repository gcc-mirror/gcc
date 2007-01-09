/* java.beans.PersistenceDelegate
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

package java.beans;

/** <p>A <code>PersistenceDelegate</code> describes how a another object
 * has to constructed and transformed in order to create a complete
 * replicate.</p>
 * 
 * <p>For custom classes you will need to implement
 * <code>PersistenceDelegate</code> in a way that is suitable for them.
 * To make use of the implementation you have to register it with an
 * {@link Encoder} using the {Encoder#setPersistenceDelegate} method.</p>
 * 
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.4
 */
public abstract class PersistenceDelegate
{

  protected void initialize(Class<?> type, Object oldInstance,
                            Object newInstance, Encoder out)
  {
    if (type != Object.class)
      {
        type = type.getSuperclass();

        PersistenceDelegate pd = out.getPersistenceDelegate(type);
        
        pd.initialize(type, oldInstance, newInstance, out);
      }
  }

  public void writeObject(Object oldInstance, Encoder out)
  {
    Object streamCandidate = out.get(oldInstance);

    if (mutatesTo(oldInstance, streamCandidate))
      {
        initialize(oldInstance.getClass(), oldInstance, streamCandidate, out);
      }
    else
      {
        out.remove(oldInstance);
        out.writeExpression(instantiate(oldInstance, out));
      }
  }

  protected boolean mutatesTo(Object oldInstance, Object newInstance)
  {
    return (newInstance != null)
           && oldInstance.getClass() == newInstance.getClass();
  }

  protected abstract Expression instantiate(Object oldInstance, Encoder out);
}
