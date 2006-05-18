/* SecurityProviderInfo.java -- Data Access Object for a security provider
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


package gnu.classpath.tools.common;

import java.security.Provider;

/**
 * A Data Access Object (DAO) referenceing a <i>Security Provider</i> and its
 * position in the list of installed <i>Security Providers</i> in the underlying
 * JVM runtime.
 */
public class SecurityProviderInfo
{
  final private Provider provider;
  final private int position;
  private transient String str;

  /**
   * Constructs an instance of <code>SecurityProviderInfo</code>.
   * <p>
   * Used by {@link ProviderUtil} to indicate the result of adding a provider,
   * given its class name.
   * 
   * @param provider the possibly <code>null</code> {@link Provider}.
   * @param position the position of <code>provider</code> in the list of
   * <i>Security Providers</i> in the underlying JVM runtime. <code>-1</code>
   * if that provider (a) is <code>null</code>, or (b) was not added because it
   * was already there.
   */
  SecurityProviderInfo(Provider provider, int position)
  {
    super();

    this.provider = provider;
    this.position = position;
  }

  /** @return the possibly <code>null</code> {@link Provider} instance. */
  public Provider getProvider()
  {
    return this.provider;
  }

  /**
   * @return the position of the {@link Provider}, or <code>-1</code> if it
   *         was not added.
   */
  public int getPosition()
  {
    return this.position;
  }

  public String toString()
  {
    if (str == null)
      if (provider == null)
        str = "SecurityProviderInfo{null, -1}";
      else
        str = "SecurityProviderInfo{" + provider.getName() + ", " + position + "}";

    return str;
  }
}
