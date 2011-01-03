/* VMAccessController.java -- VM-specific access controller methods.
   Copyright (C) 2004, 2005, 2006, 2010  Free Software Foundation, Inc.

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
      new ProtectionDomain(source, permissions, null, null)
    };
    DEFAULT_CONTEXT = new AccessControlContext(domain);
  }

  private static final boolean DEBUG = gnu.classpath.Configuration.DEBUG;
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
    // Can't really do anything while the VM is initializing.
    VMAccessControlState state = VMAccessControlState.getThreadState();
    if (state == null)
      return;

    if (DEBUG)
      debug("pushing " + acc);

    LinkedList stack = state.getContexts();
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
    // Can't really do anything while the VM is initializing.
    VMAccessControlState state = VMAccessControlState.getThreadState();
    if (state == null)
      return;

    if (DEBUG)
      debug("popping context");

    // Stack should never be null, nor should it be empty, if this method
    // and its counterpart has been called properly.
    LinkedList stack = state.getContexts();
    if (!stack.isEmpty())
      {
        stack.removeFirst();
      }
    else if (DEBUG)
      {
        debug("no stack during pop?????");
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
    // If the VM is initializing return the all-permissive context
    // so that any security checks succeed.
    VMAccessControlState state = VMAccessControlState.getThreadState();
    if (state == null)
      return DEFAULT_CONTEXT;

    // If we are already in getContext, but called a method that needs
    // a permission check, return the all-permissive context so methods
    // called from here succeed.
    //
    // XXX is this necessary? We should verify if there are any calls in
    // the stack below this method that require permission checks.
    if (state.isInGetContext())
      {
        if (DEBUG)
          debug("already in getContext");
        return DEFAULT_CONTEXT;
      }

    state.setInGetContext(true);

    Object[] stack = getStack();
    Class[] classes = (Class[]) stack[0];
    boolean privileged = ((Boolean) stack[1]).booleanValue();

    if (DEBUG)
      debug("got trace of length " + classes.length);

    HashSet domains = new HashSet();
    HashSet seenDomains = new HashSet();
    AccessControlContext context = null;

    // We walk down the stack, adding each ProtectionDomain for each
    // class in the call stack. If we reach a call to doPrivileged,
    // we don't add any more stack frames. We skip the first three stack
    // frames, since they comprise the calls to getStack, getContext,
    // and AccessController.getContext.
    for (int i = 3; i < classes.length; i++)
      {
        Class clazz = classes[i];
        ClassLoader loader = clazz.getClassLoader();

        if (DEBUG)
          {
            debug("checking " + clazz);
            // subject to getClassLoader RuntimePermission
            debug("loader = " + loader);
          }

        if (privileged && i == classes.length - 2)
          {
            // If there was a call to doPrivileged with a supplied context,
            // return that context. If using JAAS doAs*, it should be
            // a context with a SubjectDomainCombiner
            LinkedList l = state.getContexts();
            if (!l.isEmpty())
              context = (AccessControlContext) l.getFirst();
          }

        // subject to getProtectionDomain RuntimePermission
        ProtectionDomain domain = clazz.getProtectionDomain();

        if (domain == null)
          continue;
        if (seenDomains.contains(domain))
          continue;
        seenDomains.add(domain);

        // Create a static snapshot of this domain, which may change over time
        // if the current policy changes.
        domains.add(new ProtectionDomain(domain.getCodeSource(),
                                         domain.getPermissions(),
                                         loader, null));
      }

    if (DEBUG)
      debug("created domains: " + domains);

    ProtectionDomain[] result = (ProtectionDomain[])
      domains.toArray(new ProtectionDomain[domains.size()]);

    if (context != null)
      {
        DomainCombiner dc = context.getDomainCombiner ();
        // If the supplied context had no explicit DomainCombiner, use
        // our private version, which computes the intersection of the
        // context's domains with the derived set.
        if (dc == null)
          context = new AccessControlContext
            (IntersectingDomainCombiner.SINGLETON.combine
             (result, context.getProtectionDomains ()));
        // Use the supplied DomainCombiner. This should be secure,
        // because only trusted code may create an
        // AccessControlContext with a custom DomainCombiner.
        else
          context = new AccessControlContext (result, context, dc);
      }
    // No context was supplied. Return the derived one.
    else
      context = new AccessControlContext (result);

    state.setInGetContext(false);
    return context;
  }

  /**
   * Returns a snapshot of the current call stack as a two-element
   * array. The first element is an array of classes in the call
   * stack, and the second element is a boolean value indicating
   * whether the trace stopped early because a call to doPrivileged
   * was encountered.  If this boolean value is true then the call to
   * doPrivileged will be the second-last frame in the returned trace.
   *
   * @return A snapshot of the current call stack.
   */
  private static native Object[] getStack();
}
