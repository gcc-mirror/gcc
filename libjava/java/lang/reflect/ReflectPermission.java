// ReflectPermission.java - Process modifier values.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date November 18, 2000
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status: Believed complete and correct to version 1.2.
 */

package java.lang.reflect;

import java.security.BasicPermission;

/**
 * This class implements permissions for reflection.  This is a named
 * permission, and the only defined name is suppressAccessChecks.
 */
public final class ReflectPermission extends BasicPermission
{
  /**
   * Construct a ReflectPermission with the given name.
   * @param name The permission name
   */
  public ReflectPermission (String name)
  {
    super (name);
  }

  /**
   * Construct a ReflectPermission with the given name.
   * @param name The permission name
   * @param actions The actions; this is ignored and should be null.
   */
  public ReflectPermission (String name, String actions)
  {
    super (name, actions);
  }
}
