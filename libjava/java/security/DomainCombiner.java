/* DomainCombiner.java - Combines ProtectionDomains
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

package java.security;

/**
   A public interface used to combine two ProtectionDomains in a new
   ProtectionDomain and update the current Protection Domains
   associated with the current AccessControllerContext.

   It can add, subtract, or update ProtectionDomains or possibly
   remove duplicates or any possible complex action but just not add
   ones that do not already exist in either array.

   @since JDK 1.3
   @author Mark Benvenuto 
 */
public interface DomainCombiner
{
  /**
     Combines the current ProtectionDomains of the Thread with new
     ProtectionDomains.

     @param currentDomains - the ProtectionDomains for the current thread.
     @param assignedDomains - ProtectionsDomains to add
     @returns a new array of all the ProtectionDomains 
   */
  public ProtectionDomain[] combine(ProtectionDomain[]currentDomains,
				    ProtectionDomain[]assignedDomains);
}
