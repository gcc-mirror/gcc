/* VMAccessController.java -- VM-specific access controller methods.
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
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

import java.util.HashSet;
import java.util.LinkedList;

final class VMAccessController
{

  // Fields.
  // -------------------------------------------------------------------------

  /**
   * This is a per-thread stack of AccessControlContext objects (which can
   * be null) for each call to AccessController.doPrivileged in each thread's
   * call stack. We use this to remember which context object corresponds to
   * which call.
   */
  private static final ThreadLocal contexts = new ThreadLocal();

  /**
   * This is a Boolean that, if set, tells getContext that it has already
   * been called once, allowing us to handle recursive permission checks
   * caused by methods getContext calls.
   */
  private static final ThreadLocal inGetContext = new ThreadLocal();

  /**
   * And we return this all-permissive context to ensure that privileged
   * methods called from getContext succeed.
   */
  private static final AccessControlContext DEFAULT_CONTEXT;
  static
  {
    CodeSource source = new CodeSource(null, null);
    Permissions permissions = new Permissions();
    permissions.add(new AllPermission());
    ProtectionDomain[] domain = new ProtectionDomain[] {
      new ProtectionDomain(source, permissions)
    };
    DEFAULT_CONTEXT = new AccessControlContext(domain);
  }

  private static final boolean DEBUG = false;
  private static void debug(String msg)
  {
    System.err.print(">>> VMAccessController: ");
    System.err.println(msg);
  }

  // Constructors.
  // -------------------------------------------------------------------------

  private VMAccessController() { }

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * Relate a class (which should be an instance of {@link PrivilegedAction}
   * with an access control context. This method is used by {@link
   * AccessController#doPrivileged(java.security.PrivilegedAction,java.security.AccessControlContext)}
   * to set up the context that will be returned by {@link #getContext()}.
   * This method relates the class to the current thread, so contexts
   * pushed from one thread will not be available to another.
   *
   * @param acc The access control context.
   */
  static void pushContext (AccessControlContext acc)
  {
    if (DEBUG)
      debug("pushing " + acc);
    LinkedList stack = (LinkedList) contexts.get();
    if (stack == null)
      {
        stack = new LinkedList();
        contexts.set(stack);
      }
    stack.addFirst(acc);
  }

  /**
   * Removes the relation of a class to an {@link AccessControlContext}.
   * This method is used by {@link AccessController} when exiting from a
   * call to {@link
   * AccessController#doPrivileged(java.security.PrivilegedAction,java.security.AccessControlContext)}.
   */
  static void popContext()
  {
    if (DEBUG)
      debug("popping context");

    // Stack should never be null, nor should it be empty, if this method
    // and its counterpart has been called properly.
    LinkedList stack = (LinkedList) contexts.get();
    if (stack != null)
      {
        stack.removeFirst();
        if (stack.isEmpty())
          contexts.set(null);
      }
  }

  /**
   * Examine the method stack of the currently running thread, and create
   * an {@link AccessControlContext} filled in with the appropriate {@link
   * ProtectionDomain} objects given this stack.
   *
   * @return The context.
   */
  static AccessControlContext getContext()
  {
    // If we are already in getContext, but called a method that needs
    // a permission check, return the all-permissive context so methods
    // called from here succeed.
    //
    // XXX is this necessary? We should verify if there are any calls in
    // the stack below this method that require permission checks.
    Boolean inCall = (Boolean) inGetContext.get();
    if (inCall != null && inCall.booleanValue())
      {
        if (DEBUG)
          debug("already in getContext");
        return DEFAULT_CONTEXT;
      }

    inGetContext.set(Boolean.TRUE);

    Object[][] stack = getStack();
    Class[] classes = (Class[]) stack[0];
    String[] methods = (String[]) stack[1];

    if (DEBUG)
      debug(">>> got trace of length " + classes.length);

    HashSet domains = new HashSet();
    HashSet seenDomains = new HashSet();
    AccessControlContext context = null;
    int privileged = 0;

    // We walk down the stack, adding each ProtectionDomain for each
    // class in the call stack. If we reach a call to doPrivileged,
    // we don't add any more stack frames. We skip the first three stack
    // frames, since they comprise the calls to getStack, getContext,
    // and AccessController.getContext.
    for (int i = 3; i < classes.length && privileged < 2; i++)
      {
        Class clazz = classes[i];
        String method = methods[i];

        if (DEBUG)
          {
            debug(">>> checking " + clazz + "." + method);
            debug(">>> loader = " + clazz.getClassLoader());
          }

        // If the previous frame was a call to doPrivileged, then this is
        // the last frame we look at.
        if (privileged == 1)
          privileged = 2;

        if (clazz.equals (AccessController.class)
            && method.equals ("doPrivileged"))
          {
            // If there was a call to doPrivileged with a supplied context,
            // return that context.
            LinkedList l = (LinkedList) contexts.get();
            if (l != null)
              context = (AccessControlContext) l.getFirst();
            privileged = 1;
          }

        ProtectionDomain domain = clazz.getProtectionDomain();

        if (domain == null)
          continue;
        if (seenDomains.contains(domain))
          continue;
        seenDomains.add(domain);

        // Create a static snapshot of this domain, which may change over time
        // if the current policy changes.
        domains.add(new ProtectionDomain(domain.getCodeSource(),
                                         domain.getPermissions()));
      }

    if (DEBUG)
      debug("created domains: " + domains);

    ProtectionDomain[] result = (ProtectionDomain[])
      domains.toArray(new ProtectionDomain[domains.size()]);

    // Intersect the derived protection domain with the context supplied
    // to doPrivileged.
    if (context != null)
      context = new AccessControlContext(result, context,
                                         IntersectingDomainCombiner.SINGLETON);
    // No context was supplied. Return the derived one.
    else
      context = new AccessControlContext(result);

    inGetContext.set(Boolean.FALSE);
    return context;
  }

  /**
   * Returns a snapshot of the current call stack as a pair of arrays:
   * the first an array of classes in the call stack, the second an array
   * of strings containing the method names in the call stack. The two
   * arrays match up, meaning that method <i>i</i> is declared in class
   * <i>i</i>. The arrays are clean; it will only contain Java methods,
   * and no element of the list should be null.
   *
   * <p>The default implementation returns an empty stack, which will be
   * interpreted as having no permissions whatsoever.
   *
   * @return A pair of arrays describing the current call stack. The first
   *    element is an array of Class objects, and the second is an array
   *    of Strings comprising the method names.
   */
  private static Object[][] getStack()
  {
    return new Object[][] { new Class[0], new String[0] };
  }
}
