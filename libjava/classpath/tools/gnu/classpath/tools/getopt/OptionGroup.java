/* OptionGroup.java - a group of related command-line options
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


package gnu.classpath.tools.getopt;

import java.io.PrintStream;
import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Locale;

/**
 * An option group holds a collection of Options. It also has a name. Option
 * groups are primarily useful for grouping help output.
 */
public class OptionGroup
{
  /** An 80-character string of whitespaces to use as a source for padding. */
  private static final String FILLER = "                                        "
                                     + "                                        ";
  private String name;

  ArrayList options = new ArrayList();

  /**
   * Create a new nameless option group. This can only be used by Parser.
   */
  OptionGroup()
  {
  }

  /**
   * Create a new option group with the indicated name.
   * 
   * @param name the name
   */
  public OptionGroup(String name)
  {
    this.name = name;
  }

  /**
   * Print a designated text to a {@link PrintStream}, eventually wrapping the
   * lines of text so as to ensure that the width of each line does not overflow
   * {@link Parser#MAX_LINE_LENGTH} columns. The line-wrapping is done with a
   * {@link BreakIterator} using the default {@link Locale}.
   * <p>
   * The text to print may contain <code>\n</code> characters. This method will
   * force a line-break for each such character.
   * 
   * @param out the {@link PrintStream} destination of the formatted text.
   * @param text the text to print.
   * @param leftMargin a positive value indicating the column position of the
   *          start of the first line. Continuation lines, if they exist, are
   *          printed starting at <code>leftMargin + 2</code> as per GNU
   *          convention.
   * @see Parser#MAX_LINE_LENGTH
   */
  protected static void formatText(PrintStream out, String text, int leftMargin)
  {
    formatText(out, text, leftMargin, Locale.getDefault());
  }

  /**
   * Similar to the method with the same name and three arguments, except that
   * the caller MUST specify a non-null {@link Locale} instance.
   * <p>
   * Print a designated text to a {@link PrintStream}, eventually wrapping the
   * lines of text so as to ensure that the width of each line does not overflow
   * {@link Parser#MAX_LINE_LENGTH} columns. The line-wrapping is done with a
   * {@link BreakIterator} using the designated {@link Locale}.
   * <p>
   * The text to print may contain <code>\n</code> characters. This method will
   * force a line-break for each such character.
   * 
   * @param out the {@link PrintStream} destination of the formatted text.
   * @param text the text to print.
   * @param leftMargin a positive value indicating the column position of the
   *          start of the first line. Continuation lines, if they exist, are
   *          printed starting at <code>leftMargin + 2</code> as per GNU
   *          convention.
   * @param aLocale the {@link Locale} instance to use when constructing the
   *          {@link BreakIterator}.
   * @see Parser#MAX_LINE_LENGTH
   */
  protected static void formatText(PrintStream out, String text, int leftMargin,
                                   Locale aLocale)
  {
    BreakIterator bit = BreakIterator.getLineInstance(aLocale);
    String[] lines = text.split("\n");
    int length = leftMargin;
    String leftPadding = FILLER.substring(0, leftMargin + 2);
    for (int i = 0; i < lines.length; i++)
      {
        text = lines[i];
        bit.setText(text);
        int start = bit.first();
        int finish;
        while ((finish = bit.next()) != BreakIterator.DONE)
          {
            String word = text.substring(start, finish);
            length += word.length();
            if (length >= Parser.MAX_LINE_LENGTH)
              {
                out.println();
                out.print(leftPadding);
                length = word.length() + leftMargin + 2;
              }
            out.print(word);
            start = finish;
          }
        out.println();
        if (i != lines.length - 1)
          {
            length = leftMargin + 2;
            out.print(leftPadding);
          }
      }
  }

  /**
   * Add an option to this option group.
   * 
   * @param opt the option to add
   */
  public void add(Option opt)
  {
    options.add(opt);
  }

  /**
   * Print the help output for this option group.
   * 
   * @param out the stream to which to print
   */
  public void printHelp(PrintStream out, boolean longOnly)
  {
    // Compute maximum lengths.
    int maxArgLen = 0;
    boolean shortOptionSeen = false;
    Iterator it;

    // The first pass only looks to see if we have a short option.
    it = options.iterator();
    while (it.hasNext())
      {
        Option option = (Option) it.next();
        if (option.getShortName() != '\0')
          {
            shortOptionSeen = true;
            break;
          }
      }

    it = options.iterator();
    while (it.hasNext())
      {
        Option option = (Option) it.next();
        String argName = option.getArgumentName();
        // First compute the width required for the short
        // option. "2" is the initial indentation. In the
        // GNU style we don't print an argument name for
        // a short option if there is also a long name for
        // the option.
        int thisArgLen = 2;
        if (shortOptionSeen)
          thisArgLen += 4;
        if (option.getLongName() != null)
          {
            // Handle either '-' or '--'.
            thisArgLen += 1 + option.getLongName().length();
            if (! longOnly)
              ++thisArgLen;
          }
        // Add in the width of the argument name.
        if (argName != null)
          thisArgLen += 1 + argName.length();
        maxArgLen = Math.max(maxArgLen, thisArgLen);
      }

    // Print the help.
    if (name != null)
      out.println(name + ":");
    it = options.iterator();
    while (it.hasNext())
      {
        Option option = (Option) it.next();
        String argName = option.getArgumentName();
        int column = 0;
        if (option.getShortName() != '\0')
          {
            out.print("  -");
            out.print(option.getShortName());
            column += 4;
            if (option.getLongName() == null)
              {
                if (argName != null)
                  {
                    // This is a silly hack just for '-J'.  We don't
                    // support joined options in general, but this option
                    // is filtered out before argument processing can see it.
                    if (option.getShortName() != 'J')
                      {
                        out.print(' ');
                        ++column;
                      }
                    out.print(argName);
                    column += argName.length();
                  }
                out.print("  ");
              }
            else
              out.print(", ");
            column += 2;
          }
        // Indent the long option past the short options, if one
        // was seen.
        for (; column < (shortOptionSeen ? 6 : 2); ++column)
          out.print(' ');
        if (option.getLongName() != null)
          {
            out.print(longOnly ? "-" : "--");
            out.print(option.getLongName());
            column += (longOnly ? 1 : 2) + option.getLongName().length();
            if (argName != null)
              {
                out.print(" " + argName);
                column += 1 + argName.length();
              }
          }
        // FIXME: should have a better heuristic for padding.
        out.print(FILLER.substring(0, maxArgLen + 4 - column));
        formatText(out, option.getDescription(), maxArgLen + 4);
      }
  }
}
