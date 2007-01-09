/* DriverManager.java -- Manage JDBC drivers
   Copyright (C) 1999, 2000, 2001, 2003, 2004, 2006
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

package java.sql;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.Vector;

/**
  * This class manages the JDBC drivers in the system. It maintains a
  * registry of drivers and locates the appropriate driver to handle a
  * JDBC database URL.
  * <p>
  * On startup, <code>DriverManager</code> loads all the managers specified
  * by the system property <code>jdbc.drivers</code>.  The value of this
  * property should be a colon separated list of fully qualified driver
  * class names.  Additional drivers can be loaded at any time by
  * simply loading the driver class with <code>class.forName(String)</code>.
  * The driver should automatically register itself in a static 
  * initializer.
  * <p>
  * The methods in this class are all <code>static</code>. This class
  * cannot be instantiated.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class DriverManager 
{
  /**
   * This is the log stream for JDBC drivers.
   */
  private static PrintStream log_stream;

  /**
   * This is the log writer for JDBC drivers.
   */
  private static PrintWriter log_writer;

  /**
   * This is the login timeout used by JDBC drivers.
   */
  private static int login_timeout;

  /**
   * This is the list of JDBC drivers that are loaded.
   */
  private static Vector drivers;
   // Hmm, seems like we might want to do a Hashtable and lookup by something,
   // but what would it be?

  // Load all drivers on startup
  static
  {
    drivers = new Vector();

    String driver_string = System.getProperty("jdbc.drivers");
    if (driver_string != null)
      {
	StringTokenizer st = new StringTokenizer(driver_string);
	while (st.hasMoreTokens())
          {
            String driver_classname = st.nextToken();

            try
              {
        	Class.forName(driver_classname); // The driver registers itself
              }
            catch (Exception e)
	      {
		// Ignore not founds
	      }
          }
      }

  }

  /** Can't be instantiated. */
  private DriverManager()
  {
  }

  /**
   * This method returns the log writer being used by all JDBC drivers.
   * This method should be used in place of the deprecated
   * <code>getLogStream</code> method.
   *
   * @return The log writer in use by JDBC drivers.
   */
  public static PrintWriter getLogWriter()
  {
    return log_writer;
  }
  
  /**
   * This method sets the log writer being used by JDBC drivers.  This is a
   * system-wide parameter that affects all drivers.  Note that since there
   * is no way to retrieve a <code>PrintStream</code> from a 
   * <code>PrintWriter</code>, this method cannot set the log stream in
   * use by JDBC.  Thus any older drivers may not see this setting.
   *
   * @param out The new log writer for JDBC.
   */
  public static void setLogWriter(PrintWriter out)
  {
    DriverManager.log_writer = out;
  }

/**
  * This method attempts to return a connection to the specified
  * JDBC URL string using the specified connection properties.
  *
  * @param url The JDBC URL string to connect to.
  * @param properties The connection properties.
  *
  * @return A <code>Connection</code> to that URL.
  *
  * @exception SQLException If an error occurs.
  */
  public static Connection getConnection(String url, Properties properties)
    throws SQLException
  {
    Driver d = getDriver(url);
    if (d == null)
      throw new SQLException("Driver not found for URL: " + url);

    return d.connect(url, properties);
  }


  /**
   * This method attempts to return a connection to the specified
   * JDBC URL string using the specified username and password.
   *
   * @param url The JDBC URL string to connect to.
   * @param user The username to connect with.
   * @param password The password to connect with.
   * @return A <code>Connection</code> to that URL.
   * @exception SQLException If an error occurs.
   */
  public static Connection getConnection(String url, String user,
    String password) throws SQLException
  {
    Properties p = new Properties();

    if (user != null)
      p.setProperty("user", user);
    if (password != null)
      p.setProperty("password", password);

    return getConnection(url, p);
  }

  /**
   * This method attempts to return a connection to the specified
   * JDBC URL string.
   *
   * @param url The JDBC URL string to connect to.
   *
   * @return A <code>Connection</code> to that URL.
   *
   * @exception SQLException If an error occurs.
   */
  public static Connection getConnection(String url) throws SQLException
  {
    return getConnection(url, new Properties());
  }

  /**
   * This method returns a driver that can connect to the specified
   * JDBC URL string.  This will be selected from among drivers loaded
   * at initialization time and those drivers manually loaded by the
   * same class loader as the caller.
   *
   * @param url The JDBC URL string to find a driver for.
   *
   * @return A <code>Driver</code> that can connect to the specified
   * URL.
   *
   * @exception SQLException If an error occurs, or no suitable driver can be found.
   */
  public static Driver getDriver(String url) throws SQLException
  {
    // FIXME: Limit driver search to the appropriate subset of loaded drivers.
    Enumeration e = drivers.elements();
    while(e.hasMoreElements())
      {
	Driver d = (Driver)e.nextElement();
	if (d.acceptsURL(url))
          return d;
      }

    throw new SQLException("No driver found for " + url);
  }

  /**
   * This method registers a new driver with the manager.  This is normally
   * called by the driver itself in a static initializer.
   *
   * @param driver The new <code>Driver</code> to add.
   *
   * @exception SQLException If an error occurs.
   */
  public static void registerDriver(Driver driver) throws SQLException
  {
    if (! drivers.contains(driver))
      drivers.addElement(driver);  
  }

/**
  * This method de-registers a driver from the manager.
  *
  * @param driver The <code>Driver</code> to unregister.
  *
  * @exception SQLException If an error occurs.
  */
  public static void deregisterDriver(Driver driver) throws SQLException
  {
    if (drivers.contains(driver))
      drivers.removeElement(driver);
  }

  /**
   * This method returns a list of all the currently registered JDBC drivers
   * that were loaded by the current <code>ClassLoader</code>.
   *
   * @return An <code>Enumeration</code> of all currently loaded JDBC drivers.
   */
  public static Enumeration<Driver> getDrivers()
  {
    Vector v = new Vector();
    Enumeration e = drivers.elements();

    // Is this right?
    ClassLoader cl = Thread.currentThread().getContextClassLoader();

    while(e.hasMoreElements())
      {
	Object obj = e.nextElement();

	ClassLoader loader = obj.getClass().getClassLoader();

	if (loader == null)
	  loader = ClassLoader.getSystemClassLoader();
	if (! loader.equals(cl))
	  continue;

	v.addElement(obj);
      } 

    return v.elements();
  }

  /**
   * This method set the login timeout used by JDBC drivers.  This is a
   * system-wide parameter that applies to all drivers.
   *
   * @param seconds The new login timeout value.
   */
  public static void setLoginTimeout(int seconds)
  {
    DriverManager.login_timeout = seconds;  
  }

  /**
   * This method returns the login timeout in use by JDBC drivers systemwide.
   *
   * @return The login timeout.
   */
  public static int getLoginTimeout()
  {
    return login_timeout;
  }

  /**
   * This method sets the log stream in use by JDBC.
   *
   * @param stream The log stream in use by JDBC.
   * @deprecated Use <code>setLogWriter</code> instead.
   */
  public static void setLogStream(PrintStream stream)
  {
    DriverManager.log_stream = stream;
  }

  /**
   * This method returns the log stream in use by JDBC.
   *
   * @return The log stream in use by JDBC.
   * @deprecated Use <code>getLogWriter()</code> instead.
   */
  public static PrintStream getLogStream()
  {
    return log_stream;
  }

  /**
   * This method prints the specified line to the log stream.
   *
   * @param message The string to write to the log stream.
   */
  public static void println(String message)
  {
    if (log_stream != null) // Watch for user not using logging
      log_stream.println(message);
  }
}
