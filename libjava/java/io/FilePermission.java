/* FilePermission.java --
   Copyright (C) 1998, 2000, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.io;

import java.security.Permission;

public final class FilePermission extends Permission implements Serializable
{
  private static final long serialVersionUID = 7930732926638008763L;

  private static final String CURRENT_DIRECTORY = 
    System.getProperty("user.dir");

  private static final String ALL_FILES = "<<ALL FILES>>";

  private boolean readPerm = false;
  private boolean writePerm = false;
  private boolean executePerm = false;
  private boolean deletePerm = false;
  private final String actionsString;

  // Checks and caches the actions
  private void checkPerms() throws IllegalArgumentException
  {
    String action;
    int i = actionsString.indexOf(',');
    int startI = 0;
    while (i != -1) 
      {
        action = actionsString.substring(startI, i).trim().toLowerCase();
        if (action.equals("read"))
          readPerm = true;
        else if (action.equals("write"))
          writePerm = true;
        else if (action.equals("execute"))
          executePerm = true;
        else if (action.equals("delete"))
          deletePerm = true;
	else
	  throw new IllegalArgumentException("Unknown action: " + action);
      
        startI = i + 1;
        i = actionsString.indexOf(',', startI);
      }

    action = actionsString.substring(startI).trim().toLowerCase();
    if (action.equals("read"))
      readPerm = true;
    else if (action.equals("write"))
      writePerm = true;
    else if (action.equals("execute"))
      executePerm = true;
    else if (action.equals("delete"))
      deletePerm = true;
    else
      throw new IllegalArgumentException("Unknown action: " + action);
  }

  /**
   * Create a new FilePermission.
   *
   * @param pathExpression an expression specifying the paths this
   *        permission represents.
   * @param actionsString a comma-separated list of the actions this
   *        permission represents. The actions must be "read", "write",
   *        "execute" and/or "delete".
   */
  public FilePermission(String pathExpression, String actionsString) 
  {
    // FIXME: what to do when the file string is malformed?
    super(pathExpression);
    if (pathExpression == null)
      throw new NullPointerException("pathExpression");
    if (actionsString == null)
      throw new IllegalArgumentException("actionsString");
    this.actionsString = actionsString;
    checkPerms();
  }
  
  /**
   * Get the actions this FilePermission supports.
   * @return the String representing the actions this FilePermission supports.
   */
  public String getActions() 
  {
    return actionsString;
  }

  /**
   * Get the hash code for this Object.<P>
   * FilePermission's hash code is calculated as the exclusive or of the 
   * target
   * String's hash code and the action String's hash code.
   * @specnote Sun did not specify how to calculate the hash code; 
   * I made this up.
   * @return the hash code for this Object.
   */
  public int hashCode() 
  {
    return getName().hashCode() ^ actionsString.hashCode();
  }

  /**
   * Check two FilePermissions for semantic equality.
   * Two FilePermissions are exactly equivalent if they have identical path
   * expressions and have exactly the same access permissions.
   * @param o the Object to compare to.
   * @return whether the Objects are semantically equivalent.
   */
  public boolean equals(Object o) 
  {
    if (! (o instanceof FilePermission))
      return false;
    FilePermission p = (FilePermission) o;

    String f1 = getName();
    String f2 = p.getName();

    // Compare names, taking into account if they refer to a directory
    // and one has a separator and the other does not.
    if (f1.length() > 0 && f1.charAt(f1.length() - 1) == File.separatorChar) 
      {
        if (f2.length() > 0
	    && f2.charAt(f2.length() - 1) == File.separatorChar) 
          {
	    if (! f2.equals(f1))
	      return false;
          }
        else
          {
	    if (! f2.equals(f1.substring(0, f1.length() - 1)))
	      return false;
          }
      }
    else
      {
        if (f2.length() > 0
	    && f2.charAt(f2.length() - 1) == File.separatorChar)
          {
	    if (! f1.equals(f2.substring(0, f2.length() - 1)))
	      return false;
          }
        else
          {
	    if (! f1.equals(f2))
	      return false;
          }
      }
    return (readPerm == p.readPerm
	    && writePerm == p.writePerm
	    && executePerm == p.executePerm
	    && deletePerm == p.deletePerm);
  }

  /**
   * Check to see if this permission implies another.
   * Permission A implies permission B if these things are all true:
   * <OL>
   * <LI>A and B are both FilePermissions.</LI>
   * <LI>All possible files in B are included in A 
   * (possibly more are in A).</LI>
   * <LI>All actions B supports, A also supports.</LI>
   * </OL>
   * @param p the Permission to compare against.
   * @return whether this Permission implies p
   */
  public boolean implies(Permission p) 
  {
    if (! (p instanceof FilePermission))
      return false;

    String f1 = getName();

    if (f1.equals(ALL_FILES))
      return true;
    
    FilePermission fp = (FilePermission) p;
    String f2 = fp.getName();

    if (f1.charAt(0) != File.separatorChar)
      f1 = CURRENT_DIRECTORY + f1;
    if (f2.charAt(0) != File.separatorChar)
      f2 = CURRENT_DIRECTORY + f2;

    String sub1;

    switch (f1.charAt(f1.length() - 1))
      {
      case '*':
	sub1 = f1.substring(0, f1.length() - 1); // chop off "*"
	if (f2.length() <= sub1.length())
	  {
	    // If it's smaller, there is no way it could be part of
	    // this directory.  If it's the same (or length - 1), it
	    // could be the same directory but specifies access to
	    // the directory rather than the files in it.
	    return false;
	  } 
	else if (f2.charAt(sub1.length() - 1) == File.separatorChar)
	  {
	    // Make sure the part before the "/" is the same.
	    if (! f2.substring(0, sub1.length()).equals(sub1))
	      return false;
	    // Make sure there are no subdirectories specified
	    // underneath this one.
	    if (f2.substring(sub1.length() + 1).indexOf(File.separatorChar)
		!= -1)
	      return false;
	  }
	else
	  {
	    // Obviously not equal: f2 is either not a directory or
	    // is not the same directory (its name continues further
	    // than we want).
	    return false;
	  }
	break;
      case '-':
	// Chop off "/-".
	sub1 = f1.substring(0, f1.length() - 2);
	if (f2.length() < sub1.length())
	  {
	    // If it's smaller, there is no way it could be part of
	    // this directory.
	    return false;
	  }
	else if (f2.length() > sub1.length()
		 && f2.charAt(sub1.length()) != File.separatorChar)
	  return false;
	else if (! f2.substring(0, sub1.length()).equals(sub1))
	  return false;
	break;

      default:
	if (f2.charAt(f2.length() - 1) == File.separatorChar)
	  {
	    if (! f1.equals(f2.substring(0, f2.length() - 1)))
	      return false;
	  }
	else if (!f1.equals(f2))
	  return false;
	break;
      }

    if (readPerm && ! fp.readPerm)
      return false;
    if (writePerm && ! fp.writePerm)
      return false;
    if (executePerm && ! fp.executePerm)
      return false;
    if (deletePerm && ! fp.deletePerm)
      return false;
    
    return true;
  }
}
