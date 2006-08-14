/* ExpirableObject.java -- an object that is automatically destroyed.
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.security.util;

import java.util.Timer;
import java.util.TimerTask;

import javax.security.auth.DestroyFailedException;
import javax.security.auth.Destroyable;

/**
 * The base class for objects with sensitive data that are automatically
 * destroyed after a timeout elapses. On creation, an object that extends this
 * class will automatically be added to a {@link Timer} object that, once a
 * timeout elapses, will automatically call the {@link Destroyable#destroy()}
 * method.
 * <p>
 * Concrete subclasses must implement the {@link #doDestroy()} method instead of
 * {@link Destroyable#destroy()}; the behavior of that method should match
 * exactly the behavior desired of <code>destroy()</code>.
 * <p>
 * Note that if a {@link DestroyFailedException} occurs when the timeout
 * expires, it will not be reported.
 * 
 * @see Destroyable
 */
public abstract class ExpirableObject
    implements Destroyable
{
  /**
   * The default timeout, used in the default constructor.
   */
  public static final long DEFAULT_TIMEOUT = 3600000L;

  /**
   * The timer that expires instances.
   */
  private static final Timer EXPIRER = new Timer(true);

  /**
   * A reference to the task that will destroy this object when the timeout
   * expires.
   */
  private final Destroyer destroyer;

  /**
   * Create a new expirable object that will expire after one hour.
   */
  protected ExpirableObject()
  {
    this(DEFAULT_TIMEOUT);
  }

  /**
   * Create a new expirable object that will expire after the specified timeout.
   * 
   * @param delay The delay before expiration.
   * @throws IllegalArgumentException If <i>delay</i> is negative, or if
   *           <code>delay + System.currentTimeMillis()</code> is negative.
   */
  protected ExpirableObject(final long delay)
  {
    destroyer = new Destroyer(this);
    EXPIRER.schedule(destroyer, delay);
  }

  /**
   * Destroys this object. This method calls {@link #doDestroy}, then, if no
   * exception is thrown, cancels the task that would destroy this object when
   * the timeout is reached.
   * 
   * @throws DestroyFailedException If this operation fails.
   */
  public final void destroy() throws DestroyFailedException
  {
    doDestroy();
    destroyer.cancel();
  }

  /**
   * Subclasses must implement this method instead of the {@link
   * Destroyable#destroy()} method.
   * 
   * @throws DestroyFailedException If this operation fails.
   */
  protected abstract void doDestroy() throws DestroyFailedException;

  /**
   * The task that destroys the target when the timeout elapses.
   */
  private final class Destroyer
      extends TimerTask
  {
    private final ExpirableObject target;

    Destroyer(final ExpirableObject target)
    {
      super();
      this.target = target;
    }

    public void run()
    {
      try
        {
          if (! target.isDestroyed())
            target.doDestroy();
        }
      catch (DestroyFailedException dfe)
        {
        }
    }
  }
}
