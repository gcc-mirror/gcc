/* Formatter.java --
   A class for formatting log messages by localizing message texts
   and performing substitution of parameters
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package java.util.logging;

import java.text.MessageFormat;
import java.util.ResourceBundle;

/**
 * A <code>Formatter</code> supports handlers by localizing
 * message texts and by subsituting parameter values for their
 * placeholders.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public abstract class Formatter
{
  /**
   * Constructs a new Formatter.
   */
  protected Formatter()
  {
  }


  /**
   * Formats a LogRecord into a string.  Usually called by handlers
   * which need a string for a log record, for example to append
   * a record to a log file or to transmit a record over the network.
   *
   * @param record the log record for which a string form is requested.
   */
  public abstract String format(LogRecord record);


  /**
   * Returns a string that handlers are supposed to emit before
   * the first log record.  The base implementation returns an
   * empty string, but subclasses such as {@link XMLFormatter}
   * override this method in order to provide a suitable header.
   *
   * @return a string for the header.
   *
   * @param handler the handler which will prepend the returned
   *     string in front of the first log record.  This method
   *     may inspect certain properties of the handler, for
   *     example its encoding, in order to construct the header.
   */
  public String getHead(Handler handler)
  {
    return "";
  }


  /**
   * Returns a string that handlers are supposed to emit after
   * the last log record.  The base implementation returns an
   * empty string, but subclasses such as {@link XMLFormatter}
   * override this method in order to provide a suitable tail.
   *
   * @return a string for the header.
   *
   * @param handler the handler which will append the returned
   *     string after the last log record.  This method
   *     may inspect certain properties of the handler
   *     in order to construct the tail.
   */
  public String getTail(Handler handler)
  {
    return "";
  }


  /**
   * Formats the message part of a log record.
   *
   * <p>First, the Formatter localizes the record message to the
   * default locale by looking up the message in the record's
   * localization resource bundle.  If this step fails because there
   * is no resource bundle associated with the record, or because the
   * record message is not a key in the bundle, the raw message is
   * used instead.
   *
   * <p>Second, the Formatter substitutes appropriate strings for
   * the message parameters. If the record returns a non-empty
   * array for <code>getParameters()</code> and the localized
   * message string contains the character sequence "{0", the
   * formatter uses <code>java.text.MessageFormat</code> to format
   * the message.  Otherwise, no parameter substitution is performed.
   *
   * @param record the log record to be localized and formatted.
   *
   * @return the localized message text where parameters have been
   *         substituted by suitable strings.
   *
   * @throws NullPointerException if <code>record</code>
   *         is <code>null</code>.
   */
  public String formatMessage(LogRecord record)
  {
    String          msg;
    ResourceBundle  bundle;
    Object[]        params;

    /* This will throw a NullPointerExceptionif record is null. */
    msg = record.getMessage();
    if (msg == null)
      msg = "";

    /* Try to localize the message. */
    bundle = record.getResourceBundle();
    if (bundle != null)
    {
      try
      {
	msg = bundle.getString(msg);
      }
      catch (java.util.MissingResourceException _)
      {
      }
    }

    /* Format the message if there are parameters. */
    params = record.getParameters();
    if ((params != null)
	&& (params.length > 0)
	&& (msg.indexOf("{0") >= 0))
    {
      msg = MessageFormat.format(msg, params);
    }

    return msg;
  }
}
