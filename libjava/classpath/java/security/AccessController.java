/* AccessController.java --- Access control context and permission checker
   Copyright (C) 2001, 2004  Free Software Foundation, Inc.

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

package java.security;

/**
 * Access control context and permission checker.
 * Can check permissions in the access control context of the current thread
 * through the <code>checkPermission()</code> method.
 * Manipulates the access control context for code that needs to be executed
 * the protection domain of the calling class (by explicitly ignoring the
 * context of the calling code) in the <code>doPrivileged()</code> methods.
 * And provides a <code>getContext()</code> method which gives the access
 * control context of the current thread that can be used for checking
 * permissions at a later time and/or in another thread.
 *
 * @author Mark Wielaard (mark@klomp.org)
 * @since 1.2
 */
public final class AccessController
{
  /**
   * This class only has static methods so there is no public contructor.
   */
  private AccessController()
  {
  }

  /**
   * Checks wether the access control context of the current thread allows
   * the given Permission. Throws an <code>AccessControlException</code>
   * when the permission is not allowed in the current context. Otherwise
   * returns silently without throwing an exception.
   *
   * @param perm the permission to be checked.
   * @exception AccessControlException thrown if the current context does not
   * allow the given permission.
   */
  public static void checkPermission(Permission perm)
    throws AccessControlException
  {
    getContext().checkPermission(perm);
  }

  /**
   * Calls the <code>run()</code> method of the given action with as
   * (initial) access control context only the protection domain of the
   * calling class. Calls to <code>checkPermission()</code> in the
   * <code>run()</code> method ignore all earlier protection domains of
   * classes in the call chain. Note that the protection domains of classes
   * called by the code in the <code>run()</code> method are not ignored.
   *
   * @param action the <code>PrivilegedAction</code> whose <code>run()</code>
   * should be be called.
   * @return the result of the <code>action.run()</code> method.
   */
  public static <T> T doPrivileged(PrivilegedAction<T> action)
  {
    VMAccessController.pushContext(null);
    try
      {
        return action.run();
      }
    finally
      {
        VMAccessController.popContext();
      }
  }

  /**
   * Calls the <code>run()</code> method of the given action with as
   * (initial) access control context the given context combined with the
   * protection domain of the calling class. Calls to
   * <code>checkPermission()</code> in the <code>run()</code> method ignore
   * all earlier protection domains of classes in the call chain, but add
   * checks for the protection domains given in the supplied context.
   *
   * @param action the <code>PrivilegedAction</code> whose <code>run()</code>
   * should be be called.
   * @param context the <code>AccessControlContext</code> whose protection
   * domains should be added to the protection domain of the calling class.
   * @return the result of the <code>action.run()</code> method.
   */
  public static <T> T doPrivileged(PrivilegedAction<T> action,
                                   AccessControlContext context)
  {
    VMAccessController.pushContext(context);
    try
      {
        return action.run();
      }
    finally
      {
        VMAccessController.popContext();
      }
  }

  /**
   * Calls the <code>run()</code> method of the given action with as
   * (initial) access control context only the protection domain of the
   * calling class. Calls to <code>checkPermission()</code> in the
   * <code>run()</code> method ignore all earlier protection domains of
   * classes in the call chain. Note that the protection domains of classes
   * called by the code in the <code>run()</code> method are not ignored.
   * If the <code>run()</code> method throws an exception then this method
   * will wrap that exception in an <code>PrivilegedActionException</code>.
   *
   * @param action the <code>PrivilegedExceptionAction</code> whose
   * <code>run()</code> should be be called.
   * @return the result of the <code>action.run()</code> method.
   * @exception PrivilegedActionException wrapped around any checked exception
   * that is thrown in the <code>run()</code> method.
   */
  public static <T> T doPrivileged(PrivilegedExceptionAction<T> action)
    throws PrivilegedActionException
  {
    VMAccessController.pushContext(null);
    try
      {
        return action.run();
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        throw new PrivilegedActionException(e);
      }
    finally
      {
        VMAccessController.popContext();
      }
  }

  /**
   * Calls the <code>run()</code> method of the given action with as
   * (initial) access control context the given context combined with the
   * protection domain of the calling class. Calls to
   * <code>checkPermission()</code> in the <code>run()</code> method ignore
   * all earlier protection domains of classes in the call chain, but add
   * checks for the protection domains given in the supplied context.
   * If the <code>run()</code> method throws an exception then this method
   * will wrap that exception in an <code>PrivilegedActionException</code>.
   *
   * @param action the <code>PrivilegedExceptionAction</code> whose
   * <code>run()</code> should be be called.
   * @param context the <code>AccessControlContext</code> whose protection
   * domains should be added to the protection domain of the calling class.
   * @return the result of the <code>action.run()</code> method.
   * @exception PrivilegedActionException wrapped around any checked exception
   * that is thrown in the <code>run()</code> method.
   */
  public static <T> T doPrivileged(PrivilegedExceptionAction<T> action,
                                   AccessControlContext context)
    throws PrivilegedActionException
  {
    VMAccessController.pushContext(context);
    try
      {
        return action.run();
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        throw new PrivilegedActionException(e);
      }
    finally
      {
        VMAccessController.popContext();
      }
  }

  /**
   * Returns the complete access control context of the current thread.
   * The returned object encompasses all {@link ProtectionDomain} objects
   * for all classes in the current call stack, or the set of protection
   * domains until the last call to {@link
   * #doPrivileged(java.security.PrivilegedAction)}.
   *
   * <p>Additionally, if a call was made to {@link
   * #doPrivileged(java.security.PrivilegedAction,java.security.AccessControlContext)}
   * that supplied an {@link AccessControlContext}, then that context
   * will be intersected with the calculated one.
   *
   * @return The context.
   */
  public static AccessControlContext getContext()
  {
    return VMAccessController.getContext();
  }
}
