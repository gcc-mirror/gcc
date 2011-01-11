/* Level.java -- a class for indicating logging levels
   Copyright (C) 2002, 2005, 2006  Free Software Foundation, Inc.

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


package java.util.logging;

import java.io.Serializable;
import java.util.ResourceBundle;

/**
 * A class for indicating logging levels.  A number of commonly used
 * levels is pre-defined (such as <code>java.util.logging.Level.INFO</code>),
 * and applications should utilize those whenever possible.  For specialized
 * purposes, however, applications can sub-class Level in order to define
 * custom logging levels.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class Level implements Serializable
{
  /* The integer values are the same as in the Sun J2SE 1.4.
   * They have been obtained with a test program. In J2SE 1.4.1,
   * Sun has amended the API documentation; these values are now
   * publicly documented.
   */

  /**
   * The <code>OFF</code> level is used as a threshold for filtering
   * log records, meaning that no message should be logged.
   *
   * @see Logger#setLevel(java.util.logging.Level)
   */
  public static final Level OFF = new Level ("OFF", Integer.MAX_VALUE);

  /**
   * Log records whose level is <code>SEVERE</code> indicate a serious
   * failure that prevents normal program execution.  Messages at this
   * level should be understandable to an inexperienced, non-technical
   * end user.  Ideally, they explain in simple words what actions the
   * user can take in order to resolve the problem.
   */
  public static final Level SEVERE = new Level ("SEVERE", 1000);


  /**
   * Log records whose level is <code>WARNING</code> indicate a
   * potential problem that does not prevent normal program execution.
   * Messages at this level should be understandable to an
   * inexperienced, non-technical end user.  Ideally, they explain in
   * simple words what actions the user can take in order to resolve
   * the problem.
   */
  public static final Level WARNING = new Level ("WARNING", 900);


  /**
   * Log records whose level is <code>INFO</code> are used in purely
   * informational situations that do not constitute serious errors or
   * potential problems. In the default logging configuration, INFO
   * messages will be written to the system console.  For this reason,
   * the INFO level should be used only for messages that are
   * important to end users and system administrators.  Messages at
   * this level should be understandable to an inexperienced,
   * non-technical user.
   */
  public static final Level INFO = new Level ("INFO", 800);


  /**
   * Log records whose level is <code>CONFIG</code> are used for
   * describing the static configuration, for example the windowing
   * environment, the operating system version, etc.
   */
  public static final Level CONFIG = new Level ("CONFIG", 700);


  /**
   * Log records whose level is <code>FINE</code> are typically used
   * for messages that are relevant for developers using
   * the component generating log messages.  Examples include minor,
   * recoverable failures, or possible inefficiencies.
   */
  public static final Level FINE = new Level ("FINE", 500);


  /**
   * Log records whose level is <code>FINER</code> are intended for
   * rather detailed tracing, for example entering a method, returning
   * from a method, or throwing an exception.
   */
  public static final Level FINER = new Level ("FINER", 400);


  /**
   * Log records whose level is <code>FINEST</code> are used for
   * highly detailed tracing, for example to indicate that a certain
   * point inside the body of a method has been reached.
   */
  public static final Level FINEST = new Level ("FINEST", 300);


  /**
   * The <code>ALL</code> level is used as a threshold for filtering
   * log records, meaning that every message should be logged.
   *
   * @see Logger#setLevel(java.util.logging.Level)
   */
  public static final Level ALL = new Level ("ALL", Integer.MIN_VALUE);


  private static final Level[] knownLevels = {
    ALL, FINEST, FINER, FINE, CONFIG, INFO, WARNING, SEVERE, OFF
  };


  /**
   * The name of the Level without localizing it, for example
   * "WARNING".
   */
  private String name;


  /**
   * The integer value of this <code>Level</code>.
   */
  private int value;


  /**
   * The name of the resource bundle used for localizing the level
   * name, or <code>null</code> if the name does not undergo
   * localization.
   */
  private String resourceBundleName;


  /**
   * Creates a logging level given a name and an integer value.
   * It rarely is necessary to create custom levels,
   * as most applications should be well served with one of the
   * standard levels such as <code>Level.CONFIG</code>,
   * <code>Level.INFO</code>, or <code>Level.FINE</code>.
   *
   * @param name the name of the level.
   *
   * @param value the integer value of the level.  Please note
   *     that the Java<small><sup>TM</sup></small>
   *     Logging API does not specify integer
   *     values for standard levels (such as
   *     Level.FINE).  Therefore, a custom
   *     level should pass an integer value that
   *     is calculated at run-time, e.g.
   *     <code>(Level.FINE.intValue() + Level.CONFIG.intValue())
   *     / 2</code> for a level between FINE and CONFIG.
   */
  protected Level(String name, int value)
  {
    this(name, value, null);
  }


  /**
   * Create a logging level given a name, an integer value and a name
   * of a resource bundle for localizing the level name.  It rarely
   * is necessary to create custom levels, as most applications
   * should be well served with one of the standard levels such as
   * <code>Level.CONFIG</code>, <code>Level.INFO</code>, or
   * <code>Level.FINE</code>.
   *
   * @param name the name of the level.
   *
   * @param value the integer value of the level.  Please note
   *        that the Java<small><sup>TM</sup></small>
   *        Logging API does not specify integer
   *        values for standard levels (such as
   *        Level.FINE).  Therefore, a custom
   *        level should pass an integer value that
   *        is calculated at run-time, e.g.
   *        <code>(Level.FINE.intValue() + Level.CONFIG.intValue())
   *        / 2</code> for a level between FINE and CONFIG.
   *
   * @param resourceBundleName the name of a resource bundle
   *       for localizing the level name, or <code>null</code>
   *       if the name does not need to be localized.
   */
  protected Level(String name, int value, String resourceBundleName)
  {
    this.name = name;
    this.value = value;
    this.resourceBundleName = resourceBundleName;
  }


  static final long serialVersionUID = -8176160795706313070L;


  /**
   * Checks whether the Level has the same intValue as one of the
   * pre-defined levels.  If so, the pre-defined level object is
   * returned.
   *
   * <br/>Since the resource bundle name is not taken into
   * consideration, it is possible to resolve Level objects that have
   * been de-serialized by another implementation, even if the other
   * implementation uses a different resource bundle for localizing
   * the names of pre-defined levels.
   */
  private Object readResolve()
  {
    for (int i = 0; i < knownLevels.length; i++)
      if (value == knownLevels[i].intValue())
        return knownLevels[i];

    return this;
  }


  /**
   * Returns the name of the resource bundle used for localizing the
   * level name.
   *
   * @return the name of the resource bundle used for localizing the
   * level name, or <code>null</code> if the name does not undergo
   * localization.
   */
  public String getResourceBundleName()
  {
    return resourceBundleName;
  }


  /**
   * Returns the name of the Level without localizing it, for example
   * "WARNING".
   */
  public String getName()
  {
    return name;
  }


  /**
   * Returns the name of the Level after localizing it, for example
   * "WARNUNG".
   */
  public String getLocalizedName()
  {
    String localizedName = null;

    if (resourceBundleName != null)
    {
      try
      {
        ResourceBundle b = ResourceBundle.getBundle(resourceBundleName);
        localizedName = b.getString(name);
      }
      catch (Exception _)
      {
      }
    }

    if (localizedName != null)
      return localizedName;
    else
      return name;
  }


  /**
   * Returns the name of the Level without localizing it, for example
   * "WARNING".
   */
  public final String toString()
  {
    return getName();
  }


  /**
   * Returns the integer value of the Level.
   */
  public final int intValue()
  {
    return value;
  }


  /**
   * Returns one of the standard Levels given either its name or its
   * integer value.  Custom subclasses of Level will not be returned
   * by this method.
   *
   * @throws IllegalArgumentException if <code>name</code> is neither
   * the name nor the integer value of one of the pre-defined standard
   * logging levels.
   *
   * @throws NullPointerException if <code>name</code> is null.
   *
   */
  public static Level parse(String name)
    throws IllegalArgumentException
  {
    /* This will throw a NullPointerException if name is null,
     * as required by the API specification.
     */
    name = name.intern();

    for (int i = 0; i < knownLevels.length; i++)
    {
      // It's safe to use == instead of .equals here because only the
      // standard logging levels will be returned by this method, and
      // they are all created using string literals.
      if (name == knownLevels[i].name)
        return knownLevels[i];
    }

    try
    {
      int num = Integer.parseInt(name);
      for (int i = 0; i < knownLevels.length; i++)
        if (num == knownLevels[i].value)
          return knownLevels[i];
    }
    catch (NumberFormatException _)
    {
    }

    String msg = "Not the name of a standard logging level: \"" + name + "\"";
    throw new IllegalArgumentException(msg);
  }


  /**
   * Checks whether this Level's integer value is equal to that of
   * another object.
   *
   * @return <code>true</code> if <code>other</code> is an instance of
   *     <code>java.util.logging.Level</code> and has the same integer
   * value, <code>false</code> otherwise.
   */
  public boolean equals(Object other)
  {
    if (!(other instanceof Level))
      return false;

    return value == ((Level) other).value;
  }


  /**
   * Returns a hash code for this Level which is based on its numeric
   * value.
   */
  public int hashCode()
  {
    return value;
  }


  /**
   * Determines whether or not this Level is one of the standard
   * levels specified in the Logging API.
   *
   * <p>This method is package-private because it is not part
   * of the logging API specification.  However, an XMLFormatter
   * is supposed to emit the numeric value for a custom log
   * level, but the name for a pre-defined level. It seems
   * cleaner to put this method to Level than to write some
   * procedural code for XMLFormatter.
   *
   * @return <code>true</code> if this Level is a standard level,
   *         <code>false</code> otherwise.
   */
  final boolean isStandardLevel()
  {
    for (int i = 0; i < knownLevels.length; i++)
      if (knownLevels[i] == this)
        return true;

    return false;
  }
}
