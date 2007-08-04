/* ActivationGroupDesc.java -- the RMI activation group descriptor
   Copyright (c) 1996, 1997, 1998, 1999, 2004, 2006
   Free Software Foundation, Inc.

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


package java.rmi.activation;

import gnu.java.rmi.activation.DefaultActivationGroup;

import java.io.Serializable;
import java.rmi.MarshalledObject;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Properties;
import java.util.TreeSet;
import java.util.zip.Adler32;

/**
 * Contains information, necessary to create of recreate the activation objects.
 * The group descriptor contains:
 * <ul>
 * <li>The name of the group's class. This class is derived from the
 * {@link ActivationGroup}.</li>
 * <li>The group class code location.</li>
 * <li>The marshalled object that contains the group specific initialization
 * information</li>
 * </ul>
 * The groups are created by the {@link ActivationGroup#createGroup} method that
 * expectes the group class to have the two parameter constructor, the first
 * parameter being the {@link ActivationGroupID} and the second the
 * {@link MarshalledObject}.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org) (from stub)
 */
public final class ActivationGroupDesc
    implements Serializable
{
  /**
   * Contains the startup options for the {@link ActivationGroup}
   * implementations. Allows to override system properties and specify other
   * options for the implementation groups.
   * 
   * @author Audrius Meskauskas (audriusa@bioinformatics.org) (from stub)
   */
  public static class CommandEnvironment
      implements Serializable
  {

    /**
     * Use the SVUID for interoperability.
     */
    static final long serialVersionUID = 6165754737887770191L;
    
    /**
     * The zero size string array used as argv value when null is passed.
     */
    private static final String[] NO_ARGS = new String[0];

    /**
     * The path to the java executable (or null for using default jre).
     */
    final String command;
    
    /**
     * The extra parameters (may be empty array but never null).
     */
    final String[] options;
    
    /**
     * Create the new command environment.
     * 
     * @param commandPatch the full path (and name) to the java executable of
     *          null for using the default executable.
     * @param args extra options that will be used when creating the activation
     *          group. Null has the same effect as the empty list.
     */
    public CommandEnvironment(String commandPatch, String[] args)
    {
      command = commandPatch;
      if (args != null)
        options = args;
      else
        options = NO_ARGS;
    }
    
    /**
     * Get the path to the java executable.
     * 
     * @return the path to the java executable or null for using the default
     * jre.
     */
    public String getCommandPath()
    {
      return command;
    }
     
    /**
     * Get the additional command options.
     * 
     * @return the command options array, may be empty string
     */
    public String[] getCommandOptions()
    {
      return options;
    }
    
    /**
     * Compare for content equality.
     */
    public boolean equals(Object obj)
    {
      if (obj instanceof CommandEnvironment)
        {
          CommandEnvironment that = (CommandEnvironment) obj;

          if (command == null || that.command == null)
            {
              // Use direct comparison if null is involved.
              if (command != that.command)
                return false;
            }
          else
            {
              // Use .equals if null is not involved.
              if (! this.command.equals(that.command))
                return false;
            }

          return Arrays.equals(options, that.options);
        }
      else
        return false;
    }

    /**
     * Get the hash code.
     */
    public int hashCode()
    {
      int h = command == null ? 0 : command.hashCode();
      for (int i = 0; i < options.length; i++)
        h ^= options[i].hashCode();

      return h;
    }
  }
  
  /**
   * Use the SVUID for interoperability.
   */
  static final long serialVersionUID = - 4936225423168276595L;
  
  /**
   * The group class name or null for the default group class implementation.
   */
  final String className;
  
  /**
   * The group class download location URL (codebase), ignored by the
   * default implementation. 
   */
  final String location;
  
  /**
   * The group initialization data.
   */
  final MarshalledObject<?> data;
  
  /**
   * The path to the group jre and the parameters of this jre, may be
   * null for the default jre.
   */
  final ActivationGroupDesc.CommandEnvironment env;
  
  /**
   * The properties that override the system properties.
   */
  final Properties props;
  
  /**
   * The cached hash code.
   */
  transient long hash;
  
  /**
   * Create the new activation group descriptor that will use the default
   * activation group implementation with the given properties and
   * environment.
   * 
   * @param aProperties the properties that override the system properties
   * @param environment the command line (and parameters), indicating, where to
   *          find the jre executable and with that parameters to call it. May
   *          be null if the default executable should be used. In this case,
   *          the activation group with the null name (the system default group)
   *          will be created.
   */
  public ActivationGroupDesc(Properties aProperties,
                             ActivationGroupDesc.CommandEnvironment environment)
  {
    this(DefaultActivationGroup.class.getName(), null, null, aProperties,
         environment);
  }
  
  /**
   * Create the new activation group descriptor.
   * 
   * @param aClassName the name of the group implementation class. The null
   *          value indicates the default implementation.
   * @param aLocation the location, from where the group implementation class
   *          should be loaded (ignored for the system default implementation).
   * @param aData the group intialization data
   * @param aProperties the properties that will override the system properties
   *          of the new group. These properties will be translated into -D
   *          options.
   * @param environment the record, containing path to the jre executable and
   *          start options for the jre or null for using the default jre and
   *          options.
   */
  public ActivationGroupDesc(String aClassName, String aLocation,
                             MarshalledObject<?> aData, Properties aProperties,
                             ActivationGroupDesc.CommandEnvironment environment)
  {
    className = aClassName;
    location = aLocation;
    data = aData;
    props = aProperties;
    env = environment;
  }
  
  /**
   * Get the activation group class name.
   * 
   * @return the activation group class name (null for default implementation)
   */
  public String getClassName()
  {
    return className;
  }
  
  /**
   * Get the location, from where the group class will be loaded
   * 
   * @return the location, from where the implementation should be loaded (null
   *         for the default implementation)
   */
  public String getLocation()
  {
    return location;
  }
  
  /**
   * Get the group intialization data.
   * 
   * @return the group intialization data in the marshalled form.
   */
  public MarshalledObject<?> getData()
  {
    return data;
  }

  /**
   * Get the overridded system properties.
   * 
   * @return the overridden group system properties.
   */
  public Properties getPropertyOverrides()
  {
    return props;
  }
  
  /**
   * Get the group command environment, containing path to the jre executable
   * and startup options.
   * 
   * @return the command environment or null if the default environment should
   *         be used.
   */
  public ActivationGroupDesc.CommandEnvironment getCommandEnvironment()
  {
    return env;
  }
  
  /**
   * Compare for the content equality.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ActivationGroupDesc)
      {
        ActivationGroupDesc that = (ActivationGroupDesc) obj;

        // Ensure the hashcodes are computed.
        if (hash == 0)
          hashCode();
        if (that.hash == 0)
          that.hashCode();

        // We compare the hash fields as they are type long rather than int.
        if (hash != that.hash)
          return false;

        if (! eq(className, that.className))
          return false;
        if (! eq(data, that.data))
          return false;
        if (! eq(env, that.env))
          return false;
        if (! eq(location, that.location))
          return false;

        // Compare the properties.
        if (eq(props, that.props))
          return true;

        if (props.size() != that.props.size())
          return false;

        Enumeration en = props.propertyNames();
        Object key, value;

        while (en.hasMoreElements())
          {
            key = en.nextElement();
            if (! that.props.containsKey(key))
              return false;
            if (! eq(props.get(key), that.props.get(key)))
              return false;
          }
        return true;
      }
    else
      return false;
  }
  
  /**
   * Compare for direct equality if one or both parameters are null, otherwise
   * call .equals.
   */
  static boolean eq(Object a, Object b)
  {
    if (a == null || b == null)
      return a == b;
    else
      return a.equals(b);
  }
  
  /**
   * Return the hashcode.
   */
  public int hashCode()
  {
    if (hash==0)
      {
        // Using Adler32 - the hashcode is cached, will be computed only
        // once and due need to scan properties is the expensive operation
        // anyway. Reliability is more important.
        Adler32 adler = new Adler32();
        if (className!=null)
          adler.update(className.getBytes());
        if (data!=null)
          adler.update(data.hashCode());
        if (env!=null)
          adler.update(env.hashCode());
        if (location!=null)
          adler.update(location.getBytes());
        if (props!=null)
          {
            Enumeration en = props.propertyNames();
            
            // Using the intermediate sorted set to ensure that the
            // properties are sorted.
            TreeSet pr = new TreeSet();
            
            Object key;
            Object value;
            while (en.hasMoreElements())
              {
                key = en.nextElement();
                if (key!=null)
                  pr.add(key);
              }
            
            Iterator it = pr.iterator();
            while (it.hasNext())
              {
                key = it.next();
                value = props.get(key);
                adler.update(key.hashCode());
                if (value!=null)
                  adler.update(value.hashCode());
              }
          }
          hash = adler.getValue();
        }
    return (int) hash;
  }

}
