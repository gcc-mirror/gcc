/* Copyright (C) 1999  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public  interface MenuContainer
{
  public Font getFont();

  /**
   * @deprected
   */
  public boolean postEvent(Event evt);

  public void remove(MenuComponent comp);
}

