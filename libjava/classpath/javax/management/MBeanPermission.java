/* MBeanPermission.java -- Permissions controlling server access.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import gnu.java.lang.CPStringBuilder;

import java.security.Permission;

import java.io.IOException;
import java.io.ObjectInputStream;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * <p>
 * Represents the permissions required to perform
 * operations using the {@link MBeanServer}.  As with
 * all {@link java.security.Permission} objects, an
 * instance of this class either represents a permission
 * already held or one that is required to access a
 * particular service.  In the case of {@link MBeanPermission}s,
 * implication checks are made using an instance of this class
 * when a user requests an operation from the server, and a
 * {@link SecurityManager} is in place.
 * </p>
 * <p>
 * An {@link MBeanPermission} consists of four elements,
 * which all have to match for the permission to be implied.
 * These are as follows:
 * </p>
 * <ol>
 * <li><strong>The action</strong>.  For a required permission,
 * this is a single value.  For a permission held by the user,
 * this is a list of comma-separated actions (with spaces allowed),
 * or <code>*</code> (representing all actions).  {@link #getActions()}
 * returns this value.</li>
 * <li><strong>The class name</strong>.  For a required permission,
 * this is the class name of the bean being accessed, if any.  If
 * a bean isn't involved in this action, the value is <code>null</code>.
 * For a permission held by the user, it has one of three values:
 * <ol>
 * <li>The empty string, implying any class.</li>
 * <li><code>*</code>, also implying any class.</li>
 * <li>A class name pattern, which may specify a single class
 * (e.g. <code>java.lang.Object</code>) or a series of classes
 * using the wildcard character <code>*</code> (e.g.
 * <code>javax.swing.*</code>.)</li>
 * </ol></li>
 * <li><strong>The member</strong>.  For a required permission,
 * this is the member of the bean being accessed (an attribute
 * or operation), if any.  If a member of the bean isn't involved
 * in this action, the value is <code>null</code>.
 * For a permission held by the user, it has one of three values:
 * <ol>
 * <li>The empty string, implying any member.</li>
 * <li><code>*</code>, also implying any member.</li>
 * <li>The name of a member.</li>
 * </ol></li>
 * <li>The object name</strong>.  For a required permission,
 * this is the {@link ObjectName} of the bean being accessed, if
 * any.  If a bean isn't involved in this action, the value is
 * <code>null</code>.  The name may not be a pattern.
 * For a permission held by the user, it may be the empty
 * string (allowing everything) or an {@link ObjectName}
 * pattern.
 * </li></ol>
 * {@link #getName()} returns the latter three of these as a
 * single string:
 * </p>
 * <p><code>className#member[objectName]</code></p>
 * <p>
 * where <code>""</code> is disallowed, as, although any of
 * the elements may be omitted, not all of them should be
 * left out simultaneously.  <code>"-"</code> is used to
 * represent <code>null</code>.  When this occurs in a
 * required permission, anything may match it.  When this
 * forms part of a permission held by the user, it only
 * matches another <code>null</code> value. 
 * </p>
 * <p>The list of valid actions is as follows:</p>
 * <ul>
 * <li>addNotificationListener</li>
 * <li>getAttribute</li>
 * <li>getClassLoader</li>
 * <li>getClassLoaderFor</li>
 * <li>getClassLoaderRepository</li>
 * <li>getDomains</li>
 * <li>getMBeanInfo</li>
 * <li>getObjectInstance</li>
 * <li>instantiate</li>
 * <li>invoke</li>
 * <li>isInstanceOf</li>
 * <li>queryMBeans</li>
 * <li>queryNames</li>
 * <li>registerMBean</li>
 * <li>removeNotificationListener</li>
 * <li>setAttribute</li>
 * <li>unregisterMBean</li>
 * </ul>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanPermission
  extends Permission
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -2416928705275160661L;

  /**
   * The list of actions associated with this permission.
   */
  private String actions;
  
  /**
   * The list of actions as an ordered set.
   */
  private transient Set<String> actionSet;

  /**
   * The set of valid actions.
   */
  private static final Set<String> validSet;

  /**
   * Initialise the set of valid actions.
   */
  static 
  {
    validSet = new HashSet<String>();
    validSet.add("addNotificationListener");
    validSet.add("getAttribute");
    validSet.add("getClassLoader");
    validSet.add("getClassLoaderFor");
    validSet.add("getClassLoaderRepository");
    validSet.add("getDomains");
    validSet.add("getMBeanInfo");
    validSet.add("getObjectInstance");
    validSet.add("instantiate");
    validSet.add("invoke");
    validSet.add("isInstanceOf");
    validSet.add("queryMBeans");
    validSet.add("queryNames");
    validSet.add("registerMBean");
    validSet.add("removeNotificationListener");
    validSet.add("setAttribute");
    validSet.add("unregisterMBean");
  }

  /**
   * Constructs a new {@link MBeanPermission} with the specified name
   * and actions.  The name is of the form <code>className#member[objectName]</code>,
   * where each element is optional, but a completely empty or <code>null</code>
   * name is disallowed.  Actions are specified as a comma-separated list
   * and may also not be empty or <code>null</code>.
   *
   * @param name the name of the permission.
   * @param actions the actions associated with this permission.
   * @throws IllegalArgumentException if the name or actions are invalid.
   */
  public MBeanPermission(String name, String actions)
  {
    super(name);
    if (name == null || name.length() == 0)
      throw new IllegalArgumentException("The supplied name was null or empty.");
    if (actions == null || actions.length() == 0)
      throw new IllegalArgumentException("The supplied action list was null or empty.");
    this.actions = actions;
    updateActionSet();
  }

  /**
   * Constructs a new {@link MBeanPermission} with the specified class name,
   * member, object name and actions.  The name of the permission is created
   * using the form <code>className#member[objectName]</code>,
   * where each element is optional, but an empty or <code>null</code>
   * name is disallowed.  Actions are specified as a comma-separated list
   * and may also not be empty or <code>null</code>.
   *
   * @param className the name of the class to which this permission applies,
   *                  or either <code>null</code> or <code>"-"</code> for a
   *                  value which may be implied by any class name, but not
   *                  imply any class name itself.
   * @param member the member of the class to which this permission applies,
   *               or either <code>null</code> or <code>"-"</code> for a
   *               value which may be implied by any member, but not
   *               imply any member itself.
   * @param name the {@link ObjectName} to which this permission applies,
   *                  or <code>null</code> for a value which may be implied by
   *                  any object name, but not imply any object name itself.
   * @param actions the actions associated with this permission.
   */
  public MBeanPermission(String className, String member,
			 ObjectName name, String actions)
  {
    this((className == null ? "-" : className) + "#" 
	 + (member == null ? "-" : member) + "[" 
	 + (name == null ? "-" : name.toString()) + "]", actions);
  }

  /**
   * Returns true if the given object is also an {@link MBeanPermission}
   * with the same name and actions.
   *
   * @param obj the object to test.
   * @return true if the object is an {@link MBeanPermission} with
   *         the same name and actions.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof MBeanPermission)
      {
	MBeanPermission p = (MBeanPermission) obj;
	return (p.getName().equals(getName()) &&
		p.getActions().equals(actions));
      }
    return false;
  }

  /**
   * Returns the list of actions in alphabetical order.
   *
   * @return the list of actions.
   */
  public String getActions()
  {
    Iterator<String> it = actionSet.iterator();
    CPStringBuilder builder = new CPStringBuilder();
    while (it.hasNext())
      {
	builder.append(it.next());
	if (it.hasNext())
	  builder.append(",");
      }
    return builder.toString();
  }

  /**
   * Returns the hashcode of the permission as the sum
   * of the hashcodes of the name and actions.
   *
   * @return the hashcode of the permission.
   */
  public int hashCode()
  {
    return getName().hashCode() + actions.hashCode();
  }

  /**
   * <p>
   * Returns true if this permission implies the supplied permission.
   * This happens if the following holds:
   * </p>
   * <ul>
   * <li>The supplied permission is an {@link MBeanPermission}</li>
   * <li>The supplied permission has either a <code>null</code> classname
   * or its classname matches the classname of this permission.  A
   * classname of <code>"*"</code> for this permission always matches
   * the classname of the supplied permission.  Generally, <code>'*'</code>
   * acts as a wildcard, so <code>".*"</code> matches <code>'.'</code>
   * followed by anything.</li>
   * <li>The supplied permission has either a <code>null</code> member
   * or its member matches the member of this permission.  A member of
   * <code>"*"</code> for this permission always matches the member
   * of the supplied permission.</li>
   * <li>The supplied permission has either a <code>null</code> object name
   * or its object name matches the object name of this permission.  If the
   * object name of this permission is a pattern, {@link ObjectName#apply(ObjectName)}
   * may be used as well.</li>
   * <li>The supplied permission's actions are a subset of the actions
   * of this permission.  If the <code>queryMBeans</code> action is presented,
   * the <code>queryNames</code> action is implied.</li>
   * </ul>
   * 
   * @param p the permission to check that this permission implies.
   * @return true if this permission implies <code>p</code>.
   */
  public boolean implies(Permission p)
  {
    if (p instanceof MBeanPermission)
      {
	MBeanPermission mp = (MBeanPermission) p;
	NameHolder pName = new NameHolder(mp.getName());
	NameHolder name = new NameHolder(getName());
	if (!(name.equals(pName)))
	  return false;
	for (String nextAction : mp.actionSet)
	  {
	    boolean found = actions.contains(nextAction);
	    if (!found)
	      if (nextAction.equals("queryNames"))
		found = actions.contains("queryMBeans");
	    if (!found)
	      return false;
	  }
	return true;
      }
    return false;
  }

  /**
   * Small helper class to handle deconstruction of the name.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private class NameHolder
  {

    /**
     * The class name.
     */
    private String className;

    /**
     * The member.
     */
    private String member;

    /**
     * The object name.
     */
    private ObjectName objectName;

    /**
     * Constructs a broken-down name from a given name.
     *
     * @param name the name to break down.
     */
    public NameHolder(String name)
    {
      String objectName = null;
      int memberIndex = name.indexOf("#");
      int onIndex = name.indexOf("[");
      if (onIndex == -1)
	{
	  if (memberIndex == -1)
	    className = name;
	  else
	    {
	      className = name.substring(0, memberIndex);
	      member = name.substring(memberIndex + 1);
	    }
	}
      else
	{
	  if (memberIndex == -1)
	    {
	      className = name.substring(0, onIndex);
	      objectName = name.substring(onIndex + 1,
					  name.length() - 1);
	    }
	  else
	    {
	      className = name.substring(0, memberIndex);
	      member = name.substring(memberIndex + 1, onIndex);
	      objectName = name.substring(onIndex + 1,
					  name.length() - 1);
	    }
	}      
      if (className.equals("-"))
	className = null;
      if (member.equals("-"))
	member = null;
      if (objectName == null || objectName.equals("-"))
	this.objectName = null;
      else
	try
	  {
	    this.objectName = new ObjectName(objectName);
	  }
	catch (MalformedObjectNameException e)
	  {
	    throw (Error) 
	      (new InternalError("Invalid object name.").initCause(e));
	  }
    }

    /**
     * <p>
     * Returns true if the supplied object is also a
     * {@link NameHolder} and the following holds:
     * </p>
     * <ul>
     * <li>The supplied classname is <code>null</code> or the two match.  A
     * classname of <code>"*"</code> for this holder always matches
     * the classname of the supplied holder.  Generally, <code>'*'</code>
     * acts as a wildcard, so <code>".*"</code> matches <code>'.'</code>
     * followed by anything.</li>
     * <li>The supplied name holder has either a <code>null</code> member
     * or its member matches the member of this name holder.  A member of
     * <code>"*"</code> for this name holder always matches the member
     * of the supplied name holder.</li>
     * <li>The supplied name holder has either a <code>null</code> object name
     * or its object name matches the object name of this name holder.  If the
     * object name of this name holder is a pattern,
     * {@link ObjectName#apply(ObjectName)} may be used as well.</li>
     * </ul>
     * 
     * @param obj the object to compare with this.
     * @return true if the above holds.
     */
    public boolean equals(Object obj)
    {
      if (obj instanceof NameHolder)
	{
	  NameHolder nh = (NameHolder) obj;
	  boolean cn = false;
	  String ocn = nh.getClassName();
	  if (ocn == null || className.equals("*"))
	    cn = true;
	  else
	    {
	      int wcIndex = className.indexOf("*");
	      if (wcIndex != -1)
		cn = ocn.startsWith(className.substring(0, wcIndex));
	      else
		cn = ocn.equals(className);
	    }
	  boolean m = false;
	  String om = nh.getMember();
	  if (om == null || member.equals("*")) 
	    m = true;
	  else
	    m = om.equals(member);
	  boolean on = false;
	  ObjectName oon = nh.getObjectName();
	  if (oon == null)
	    on = true;
	  else if (objectName.isPattern())
	    on = objectName.apply(oon);
	  else
	    on = oon.equals(objectName);
	  return (cn && m && on);    
	}
      return false;
    }
    
    /**
     * Returns the class name.
     */
    public String getClassName()
    {
      return className;
    }

    /**
     * Returns the member.
     */
    public String getMember()
    {
      return member;
    }

    /**
     * Returns the object name.
     */
    public ObjectName getObjectName()
    {
      return objectName;
    }
  }

  /**
   * Updates the action set from the current value of
   * the actions string.
   */
  private void updateActionSet()
  {
    String[] actionsArray = actions.split(",");
    actionSet = new TreeSet<String>();
    for (int a = 0; a < actionsArray.length; ++a)
      actionSet.add(actionsArray[a].trim());
  }

  /**
   * Reads the object from a stream and ensures the incoming
   * data is valid.
   *
   * @param in the input stream.
   * @throws IOException if an I/O error occurs.
   * @throws ClassNotFoundException if a class used by the object
   *                                can not be found.
   */
  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    in.defaultReadObject();
    updateActionSet();
    checkActions();
  }

  /**
   * Checks that the actions used in this permission
   * are from the valid set.
   *
   * @throws IllegalArgumentException if the name or actions are invalid.
   */
  private void checkActions()
  {
    for (String action : actionSet)
      {
	if (!(validSet.contains(action)))
	  throw new IllegalArgumentException("Invalid action " 
					     + action + " found.");
      }
  }

}

