/* Driver.java -- A JDBC driver
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

import java.util.Properties;

/**
  * This interface specifies a mechanism for accessing a JDBC database
  * driver.  When the class implementing this method is loaded, it should
  * register an instance of itself with the <code>DriverManager</code> in
  * a static initializer.  
  * <p>
  * Because the <code>DriverManager</code> might attempt to use several
  * drivers to find one that can connect to the requested database, 
  * this driver should not cause large numbers of classes and code to
  * be loaded.  If another driver is the one that ends up performing the
  * request, any loading done by this driver would be wasted.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Driver
{

/**
  * This method returns the major version number of the driver.
  *
  * @return The major version number of the driver.
  */
public abstract int
getMajorVersion();

/*************************************************************************/

/**
  * This method returns the minor version number of the driver.
  *
  * @return The minor version number of the driver.
  */
public abstract int
getMinorVersion();

/*************************************************************************/

/**
  * This method tests whether or not the driver is JDBC compliant.  This
  * method should only return <code>true</code> if the driver has been
  * certified as JDBC compliant.
  *
  * @return <code>true</code> if the driver has been certified JDBC compliant,
  * <code>false</code> otherwise.
  */
public abstract boolean
jdbcCompliant();

/*************************************************************************/

/**
  * This method returns an array of possible properties that could be
  * used to connect to the specified database.
  *
  * @param url The URL string of the database to connect to.
  * @param properties The list of properties the caller is planning to use
  * to connect to the database.
  *
  * @return A list of possible additional properties for a connection to this
  * database.  This list may be empty.
  *
  * @exception SQLException If an error occurs.
  */
public abstract DriverPropertyInfo[]
getPropertyInfo(String url, Properties properties) throws SQLException;

/*************************************************************************/

/**
  * This method tests whether or not the driver believes it can connect to
  * the specified database.  The driver should only test whether it 
  * understands and accepts the URL. It should not necessarily attempt to 
  * probe the database for a connection.
  *
  * @param The database URL string.
  *
  * @return <code>true</code> if the drivers can connect to the database, 
  * <code>false</code> otherwise.
  *
  * @exception SQLException If an error occurs.
  */
public abstract boolean
acceptsURL(String url) throws SQLException;

/*************************************************************************/

/**
  * This method connects to the specified database using the connection
  * properties supplied.  If the driver does not understand the database
  * URL, it should return <code>null</code> instead of throwing an
  * exception since the <code>DriverManager</code> will probe a driver
  * in this manner.
  * 
  * @param url The URL string for this connection.
  * @param properties The list of database connection properties.
  *
  * @return A <code>Connection</code> object for the newly established
  * connection, or <code>null</code> if the URL is not understood.
  *
  * @exception SQLException If an error occurs.
  */
public abstract Connection
connect(String url, Properties properties) throws SQLException;

} // interface Driver

