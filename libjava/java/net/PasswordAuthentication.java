/* PasswordAuthentication.java -- Container class for username/password pairs
   Copyright (C) 1998,2000 Free Software Foundation, Inc.

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

package java.net;

/**
  * This class serves a container for username/password pairs.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public final class PasswordAuthentication
{

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * The username 
  */
private String username;

/**
  * The password
  */
private char[] password;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Creates a new PasswordAuthentication object from the specified username
  * and password.
  *
  * @param username The username for this object
  * @param password The password for this object
  */
public
PasswordAuthentication(String username, char[] password)
{
  this.username = username;
  this.password = password;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the username associated with this object
  *
  * @return The username
  */
public String
getUserName()
{
  return(username);
}
 
/*************************************************************************/

/**
  * Returns the password associated with this object
  *
  * @return The password
  */
public char[]
getPassword()
{
  return(password);
}

} // class PasswordAuthentication

