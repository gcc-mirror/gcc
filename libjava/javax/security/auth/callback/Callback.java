/* Callback.java -- marker interface for callback classes
   Copyright (C) 2003, Free Software Foundation, Inc.

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


package javax.security.auth.callback;

/**
 * <p>Implementations of this interface are passed to a {@link CallbackHandler},
 * allowing underlying security services the ability to interact with a calling
 * application to retrieve specific authentication data such as usernames and
 * passwords, or to display certain information, such as error and warning
 * messages.</p>
 *
 * <p><code>Callback</code> implementations do not retrieve or display the
 * information requested by underlying security services. <code>Callback</code>
 * implementations simply provide the means to pass such requests to
 * applications, and for applications, if appropriate, to return requested
 * information back to the underlying security services.</p>
 *
 * @see CallbackHandler
 * @see ChoiceCallback
 * @see ConfirmationCallback
 * @see LanguageCallback
 * @see NameCallback
 * @see PasswordCallback
 * @see TextInputCallback
 * @see TextOutputCallback
 */
public interface Callback {
}
