/* DriverManager.java -- Manage JDBC drivers
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


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

/*
 * Class Variables
 */

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
          catch (Exception e) { ; } // Ignore not founds
        }
    }

}
  
/*************************************************************************/

/*
 * Class Methods
 */

/**
  * This method returns the login timeout in use by JDBC drivers systemwide.
  *
  * @return The login timeout.
  */
public static int
getLoginTimeout()
{
  return(login_timeout);
}

/*************************************************************************/

/**
  * This method set the login timeout used by JDBC drivers.  This is a
  * system-wide parameter that applies to all drivers.
  *
  * @param login_timeout The new login timeout value.
  */
public static void
setLoginTimeout(int login_timeout)
{
  DriverManager.login_timeout = login_timeout;
}

/*************************************************************************/

/**
  * This method returns the log stream in use by JDBC.
  *
  * @return The log stream in use by JDBC.
  *
  * @deprecated Use <code>getLogWriter()</code> instead.
  */
public static PrintStream
getLogStream()
{
  return(log_stream);
}

/*************************************************************************/

/**
  * This method sets the log stream in use by JDBC.
  *
  * @param log_stream The log stream in use by JDBC.
  *
  * @deprecated Use <code>setLogWriter</code> instead.
  */
public static void
setLogStream(PrintStream log_stream)
{
  DriverManager.log_stream = log_stream;
}

/*************************************************************************/

/**
  * This method prints the specified line to the log stream.
  *
  * @param str The string to write to the log stream.
  */
public static void
println(String str)
{
  if (log_stream != null) // Watch for user not using logging
    log_stream.println(str);
}

/*************************************************************************/

/**
  * This method registers a new driver with the manager.  This is normally
  * called by the driver itself in a static initializer.
  *
  * @param driver The new <code>Driver</code> to add.
  */
public static void
registerDriver(Driver driver)
{
  if (!drivers.contains(driver))
    drivers.addElement(driver);
}

/*************************************************************************/

/**
  * This method de-registers a driver from the manager.
  *
  * @param driver The <code>Driver</code> to unregister.
  */
public static void
deregisterDriver(Driver driver)
{
  if (drivers.contains(driver))
    drivers.removeElement(driver);
}

/*************************************************************************/

/**
  * This method returns a list of all the currently loaded JDBC drivers which
  * the current caller has access to.
  *
  * @return An <code>Enumeration</code> of all currently loaded JDBC drivers.
  */
public static Enumeration
getDrivers()
{
  return(drivers.elements());
}

/*************************************************************************/

/**
  * This method returns a driver that can connect to the specified
  * JDBC URL string.  This will be selected from among drivers loaded
  * at initialization time and those drivers manually loaded by the
  * same class loader as the caller.
  *
  * @param url The JDBC URL string to find a driver for.
  *
  * @return A <code>Driver</code> that can connect to the specified
  * URL, or <code>null</code> if a suitable driver cannot be found.
  *
  * @exception SQLException If an error occurs.
  */
public static Driver
getDriver(String url) throws SQLException
{
  // FIXME: Limit driver search to the appropriate subset of loaded drivers.

  Enumeration e = drivers.elements();
  while(e.hasMoreElements())
    {
      Driver d = (Driver)e.nextElement();
      if (d.acceptsURL(url))
        return(d);
    }

  return(null);
}

/*************************************************************************/

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
public static Connection
getConnection(String url) throws SQLException
{
  return(getConnection(url, new Properties()));
}

/*************************************************************************/

/**
  * This method attempts to return a connection to the specified
  * JDBC URL string using the specified username and password.
  *
  * @param url The JDBC URL string to connect to.
  * @param user The username to connect with.
  * @param password The password to connect with.
  *
  * @return A <code>Connection</code> to that URL.
  *
  * @exception SQLException If an error occurs.
  */
public static Connection
getConnection(String url, String user, String password) throws SQLException
{
  Properties p = new Properties();

  p.setProperty("user", user);
  p.setProperty("password", password);

  return(getConnection(url, p));
}

/*************************************************************************/

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
public static Connection
getConnection(String url, Properties properties) throws SQLException
{
  Driver d = getDriver(url);
  if (d == null)
    throw new SQLException("Driver not found for URL: " + url);

  return(d.connect(url, properties));
}

/*************************************************************************/

/*
 * Constructors
 */

// Keep bozos from trying to instantiate us.
private
DriverManager()
{
  ;
}

} // class DriverManager 

