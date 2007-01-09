/* Subject.java -- a single entity in the system.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.security.auth;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.DomainCombiner;
import java.security.Principal;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;

public final class Subject implements Serializable
{
  // Fields.
  // -------------------------------------------------------------------------

  private static final long serialVersionUID = -8308522755600156056L;

  /**
   * @serial The set of principals. The type of this field is SecureSet, a
   *  private inner class.
   */
  private final Set principals;

  /**
   * @serial The read-only flag.
   */
  private boolean readOnly;

  private final transient SecureSet pubCred;
  private final transient SecureSet privCred;

  // Constructors.
  // -------------------------------------------------------------------------

  public Subject()
  {
    principals = new SecureSet (this, SecureSet.PRINCIPALS);
    pubCred = new SecureSet (this, SecureSet.PUBLIC_CREDENTIALS);
    privCred = new SecureSet (this, SecureSet.PRIVATE_CREDENTIALS);
    readOnly = false;
  }

  public Subject (final boolean readOnly,
                  final Set<? extends Principal> principals,
                  final Set<?> pubCred, final Set<?> privCred)
  {
    if (principals == null || pubCred == null || privCred == null)
      {
        throw new NullPointerException();
      }
    this.principals = new SecureSet (this, SecureSet.PRINCIPALS, principals);
    this.pubCred = new SecureSet (this, SecureSet.PUBLIC_CREDENTIALS, pubCred);
    this.privCred = new SecureSet (this, SecureSet.PRIVATE_CREDENTIALS, privCred);
    this.readOnly = readOnly;
  }

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * <p>Returns the subject associated with the given {@link
   * AccessControlContext}.</p>
   *
   * <p>All this method does is retrieve the Subject object from the supplied
   * context's {@link DomainCombiner}, if any, and if it is an instance of
   * a {@link SubjectDomainCombiner}.
   *
   * @param context The context to retrieve the subject from.
   * @return The subject assoctiated with the context, or <code>null</code>
   *  if there is none.
   * @throws NullPointerException If <i>subject</i> is null.
   * @throws SecurityException If the caller does not have permission to get
   *  the subject (<code>"getSubject"</code> target of {@link AuthPermission}.
   */
  public static Subject getSubject (final AccessControlContext context)
  {
    final SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission (new AuthPermission ("getSubject"));
      }
    DomainCombiner dc = context.getDomainCombiner();
    if (!(dc instanceof SubjectDomainCombiner))
      {
        return null;
      }
    return ((SubjectDomainCombiner) dc).getSubject();
  }

  /**
   * <p>Run a method as another subject. This method will obtain the current
   * {@link AccessControlContext} for this thread, then creates another with
   * a {@link SubjectDomainCombiner} with the given subject. The supplied
   * action will then be run with the modified context.</p>
   *
   * @param subject The subject to run as.
   * @param action The action to run.
   * @return The value returned by the privileged action.
   * @throws SecurityException If the caller is not allowed to run under a
   *  different identity (<code>"doAs"</code> target of {@link AuthPermission}.
   */
  public static Object doAs (final Subject subject, final PrivilegedAction action)
  {
    final SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission (new AuthPermission ("doAs"));
      }
    AccessControlContext context =
      new AccessControlContext (AccessController.getContext(),
                                new SubjectDomainCombiner (subject));
    return AccessController.doPrivileged (action, context);
  }

  /**
   * <p>Run a method as another subject. This method will obtain the current
   * {@link AccessControlContext} for this thread, then creates another with
   * a {@link SubjectDomainCombiner} with the given subject. The supplied
   * action will then be run with the modified context.</p>
   *
   * @param subject The subject to run as.
   * @param action The action to run.
   * @return The value returned by the privileged action.
   * @throws SecurityException If the caller is not allowed to run under a
   *  different identity (<code>"doAs"</code> target of {@link AuthPermission}.
   * @throws PrivilegedActionException If the action throws an exception.
   */
  public static Object doAs (final Subject subject,
                             final PrivilegedExceptionAction action)
    throws PrivilegedActionException
  {
    final SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission (new AuthPermission ("doAs"));
      }
    AccessControlContext context =
      new AccessControlContext (AccessController.getContext(),
                                new SubjectDomainCombiner(subject));
    return AccessController.doPrivileged (action, context);
  }

  /**
   * <p>Run a method as another subject. This method will create a new
   * {@link AccessControlContext} derived from the given one, with a
   * {@link SubjectDomainCombiner} with the given subject. The supplied
   * action will then be run with the modified context.</p>
   *
   * @param subject The subject to run as.
   * @param action The action to run.
   * @param acc The context to use.
   * @return The value returned by the privileged action.
   * @throws SecurityException If the caller is not allowed to run under a
   *  different identity (<code>"doAsPrivileged"</code> target of {@link
   *  AuthPermission}.
   */
  public static Object doAsPrivileged (final Subject subject,
                                       final PrivilegedAction action,
                                       final AccessControlContext acc)
  {
    final SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission (new AuthPermission ("doAsPrivileged"));
      }
    AccessControlContext context =
      new AccessControlContext (acc, new SubjectDomainCombiner (subject));
    return AccessController.doPrivileged (action, context);
  }

  /**
   * <p>Run a method as another subject. This method will create a new
   * {@link AccessControlContext} derived from the given one, with a
   * {@link SubjectDomainCombiner} with the given subject. The supplied
   * action will then be run with the modified context.</p>
   *
   * @param subject The subject to run as.
   * @param action The action to run.
   * @param acc The context to use.
   * @return The value returned by the privileged action.
   * @throws SecurityException If the caller is not allowed to run under a
   *  different identity (<code>"doAsPrivileged"</code> target of
   *  {@link AuthPermission}.
   * @throws PrivilegedActionException If the action throws an exception.
   */
  public static Object doAsPrivileged (final Subject subject,
                                       final PrivilegedExceptionAction action,
				       AccessControlContext acc)
    throws PrivilegedActionException
  {
    final SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission (new AuthPermission ("doAsPrivileged"));
      }
    if (acc == null)
      acc = new AccessControlContext (new java.security.ProtectionDomain[0]);
    AccessControlContext context =
      new AccessControlContext (acc, new SubjectDomainCombiner (subject));
    return AccessController.doPrivileged (action, context);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public boolean equals (Object o)
  {
    if (!(o instanceof Subject))
      {
        return false;
      }
    Subject that = (Subject) o;
    return principals.containsAll (that.getPrincipals()) &&
      pubCred.containsAll (that.getPublicCredentials()) &&
      privCred.containsAll (that.getPrivateCredentials());
  }

  public Set<Principal> getPrincipals()
  {
    return principals;
  }

  public <T extends Principal> Set<T> getPrincipals(Class<T> clazz)
  {
    HashSet result = new HashSet (principals.size());
    for (Iterator it = principals.iterator(); it.hasNext(); )
      {
        Object o = it.next();
        if (o != null && clazz.isAssignableFrom (o.getClass()))
          {
            result.add(o);
          }
      }
    return Collections.unmodifiableSet (result);
  }

  public Set<Object> getPrivateCredentials()
  {
    return privCred;
  }

  public <T> Set<T> getPrivateCredentials (Class<T> clazz)
  {
    HashSet result = new HashSet (privCred.size());
    for (Iterator it = privCred.iterator(); it.hasNext(); )
      {
        Object o = it.next();
        if (o != null && clazz.isAssignableFrom (o.getClass()))
          {
            result.add(o);
          }
      }
    return Collections.unmodifiableSet (result);
  }

  public Set<Object> getPublicCredentials()
  {
    return pubCred;
  }

  public <T> Set<T> getPublicCredentials (Class<T> clazz)
  {
    HashSet result = new HashSet (pubCred.size());
    for (Iterator it = pubCred.iterator(); it.hasNext(); )
      {
        Object o = it.next();
        if (o != null && clazz.isAssignableFrom (o.getClass()))
          {
            result.add(o);
          }
      }
    return Collections.unmodifiableSet (result);
  }

  public int hashCode()
  {
    return principals.hashCode() + privCred.hashCode() + pubCred.hashCode();
  }

  /**
   * <p>Returns whether or not this subject is read-only.</p>
   *
   * @return True is this subject is read-only.
   */
  public boolean isReadOnly()
  {
    return readOnly;
  }

  /**
   * <p>Marks this subject as read-only.</p>
   *
   * @throws SecurityException If the caller does not have permission to
   *  set this subject as read-only (<code>"setReadOnly"</code> target of
   *  {@link AuthPermission}.
   */
  public void setReadOnly()
  {
    final SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission (new AuthPermission ("setReadOnly"));
      }
    readOnly = true;
  }

  public String toString()
  {
    return Subject.class.getName() + " [ principals=" + principals +
      ", private credentials=" + privCred + ", public credentials=" +
      pubCred + ", read-only=" + readOnly + " ]";
  }

// Inner class.
  // -------------------------------------------------------------------------

  /**
   * An undocumented inner class that is used for sets in the parent class.
   */
  private static class SecureSet extends AbstractSet implements Serializable
  {
    // Fields.
    // -----------------------------------------------------------------------

    private static final long serialVersionUID = 7911754171111800359L;

    static final int PRINCIPALS = 0;
    static final int PUBLIC_CREDENTIALS = 1;
    static final int PRIVATE_CREDENTIALS = 2;

    private final Subject subject;
    private final LinkedList elements;
    private final transient int type;

    // Constructors.
    // -----------------------------------------------------------------------

    SecureSet (final Subject subject, final int type, final Collection inElements)
    {
      this (subject, type);
      for (Iterator it = inElements.iterator(); it.hasNext(); )
        {
          Object o = it.next();
          if (type == PRINCIPALS && !(o instanceof Principal))
            {
              throw new IllegalArgumentException(o+" is not a Principal");
            }
          if (!this.elements.contains (o))
            {
              this.elements.add (o);
            }
        }
    }

    SecureSet (final Subject subject, final int type)
    {
      this.subject = subject;
      this.type = type;
      this.elements = new LinkedList();
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public synchronized int size()
    {
      return elements.size();
    }

    public Iterator iterator()
    {
      return elements.iterator();
    }

    public synchronized boolean add(Object element)
    {
      if (subject.isReadOnly())
        {
          throw new IllegalStateException ("subject is read-only");
        }
      final SecurityManager sm = System.getSecurityManager();
      switch (type)
        {
        case PRINCIPALS:
          if (sm != null)
            {
              sm.checkPermission (new AuthPermission ("modifyPrincipals"));
            }
          if (!(element instanceof Principal))
            {
              throw new IllegalArgumentException ("element is not a Principal");
            }
          break;

        case PUBLIC_CREDENTIALS:
          if (sm != null)
            {
              sm.checkPermission (new AuthPermission ("modifyPublicCredentials"));
            }
          break;

        case PRIVATE_CREDENTIALS:
          if (sm != null)
            {
              sm.checkPermission (new AuthPermission ("modifyPrivateCredentials"));
            }
          break;

        default:
          throw new Error ("this statement should be unreachable");
        }

      if (elements.contains (element))
        {
          return false;
        }

      return elements.add (element);
    }

    public synchronized boolean remove (final Object element)
    {
      if (subject.isReadOnly())
        {
          throw new IllegalStateException ("subject is read-only");
        }
      final SecurityManager sm = System.getSecurityManager();
      switch (type)
        {
        case PRINCIPALS:
          if (sm != null)
            {
              sm.checkPermission (new AuthPermission ("modifyPrincipals"));
            }
          if (!(element instanceof Principal))
            {
              throw new IllegalArgumentException ("element is not a Principal");
            }
          break;

        case PUBLIC_CREDENTIALS:
          if (sm != null)
            {
              sm.checkPermission (new AuthPermission ("modifyPublicCredentials"));
            }
          break;

        case PRIVATE_CREDENTIALS:
          if (sm != null)
            {
              sm.checkPermission (new AuthPermission ("modifyPrivateCredentials"));
            }
          break;

        default:
          throw new Error("this statement should be unreachable");
        }

      return elements.remove(element);
    }

    public synchronized boolean contains (final Object element)
    {
      return elements.contains (element);
    }

    public boolean removeAll (final Collection c)
    {
      if (subject.isReadOnly())
        {
          throw new IllegalStateException ("subject is read-only");
        }
      return super.removeAll (c);
    }

    public boolean retainAll (final Collection c)
    {
      if (subject.isReadOnly())
        {
          throw new IllegalStateException ("subject is read-only");
        }
      return super.retainAll (c);
    }

    public void clear()
    {
      if (subject.isReadOnly())
        {
          throw new IllegalStateException ("subject is read-only");
        }
      elements.clear();
    }

    private synchronized void writeObject (ObjectOutputStream out)
      throws IOException
    {
      throw new UnsupportedOperationException ("FIXME: determine serialization");
    }

    private void readObject (ObjectInputStream in)
      throws ClassNotFoundException, IOException
    {
      throw new UnsupportedOperationException ("FIXME: determine serialization");
    }
  }
}
