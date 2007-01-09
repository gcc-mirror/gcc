/* Option.java - represent a command-line option
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

/**
 * This is the base class representing an option. An option can have a short
 * form. This is a single character, like '-x'. An option can have a long form,
 * like '--verbose'; if the parser is working in "long option only" mode, then a
 * long flag has a single dash, like '-verbose'. Both a long and a short form
 * may be specified; it is not valid to have neither. A description is mandatory
 * for options; this is used to automatically generate '--help' output.  An option
 * which takes an argument and which has a short form can also be "joined", in
 * this case the option's argument can either be separated, like "-I path" or
 * joined with the short option name, like "-Ipath".
 */
public abstract class Option
{
  private char shortName;

  private String longName;

  private String description;

  private String argumentName;

  private boolean joined;

  /**
   * Create a new option with the given short name and description.
   * 
   * @param shortName the short name
   * @param description the description
   */
  protected Option(char shortName, String description)
  {
    if (shortName == 0)
      throw new IllegalArgumentException("short name must not be \\0");
    this.shortName = shortName;
    this.description = description;
  }

  /**
   * Create a new option with the given short name and description.
   * 
   * @param shortName the short name
   * @param description the description
   * @param argumentName the descriptive name of the argument, if this option
   *          takes an argument; otherwise null
   */
  protected Option(char shortName, String description, String argumentName)
  {
    if (shortName == 0)
      throw new IllegalArgumentException("short name must not be \\0");
    this.shortName = shortName;
    this.description = description;
    this.argumentName = argumentName;
  }

  /**
   * Create a new option with the given short name and description.
   * 
   * @param shortName the short name
   * @param description the description
   * @param argumentName the descriptive name of the argument, if this option
   *          takes an argument; otherwise null
   * @param joined true if the short option is joined to its argument
   */
  protected Option(char shortName, String description, String argumentName,
                   boolean joined)
  {
    if (shortName == 0)
      throw new IllegalArgumentException("short name must not be \\0");
    this.shortName = shortName;
    this.description = description;
    this.argumentName = argumentName;
    this.joined = joined;
  }

  /**
   * Create a new option with the given long name and description. The long name
   * should be specified without any leading dashes.
   * 
   * @param longName the long name
   * @param description the description
   */
  protected Option(String longName, String description)
  {
    this.longName = longName;
    this.description = description;
  }

  /**
   * Create a new option with the given long name and description. The long name
   * should be specified without any leading dashes.
   * 
   * @param longName the long name
   * @param description the description
   * @param argumentName the descriptive name of the argument, if this option
   *          takes an argument; otherwise null
   */
  protected Option(String longName, String description, String argumentName)
  {
    this.longName = longName;
    this.description = description;
    this.argumentName = argumentName;
  }

  /**
   * Create a new option with the given short and long names and description.
   * The long name should be specified without any leading dashes.
   * 
   * @param longName the long name
   * @param shortName the short name
   * @param description the description
   */
  protected Option(String longName, char shortName, String description)
  {
    if (shortName == 0)
      throw new IllegalArgumentException("short name must not be \\0");
    this.shortName = shortName;
    this.longName = longName;
    this.description = description;
  }

  /**
   * Create a new option with the given short and long names and description.
   * The long name should be specified without any leading dashes.
   * 
   * @param longName the long name
   * @param shortName the short name
   * @param description the description
   * @param argumentName the descriptive name of the argument, if this option
   *          takes an argument; otherwise null
   */
  protected Option(String longName, char shortName, String description,
                   String argumentName)
  {
    if (shortName == 0)
      throw new IllegalArgumentException("short name must not be \\0");
    this.shortName = shortName;
    this.longName = longName;
    this.argumentName = argumentName;
    this.description = description;
  }

  /**
   * Create a new option with the given short and long names and description.
   * The long name should be specified without any leading dashes.
   * 
   * @param longName the long name
   * @param shortName the short name
   * @param description the description
   * @param argumentName the descriptive name of the argument, if this option
   *          takes an argument; otherwise null
   * @param joined true if the short option is joined to its argument
   */
  protected Option(String longName, char shortName, String description,
                   String argumentName, boolean joined)
  {
    if (shortName == 0)
      throw new IllegalArgumentException("short name must not be \\0");
    this.shortName = shortName;
    this.longName = longName;
    this.argumentName = argumentName;
    this.description = description;
    this.joined = joined;
  }

  /**
   * Return the short name of the option, or \0 if none.
   */
  public char getShortName()
  {
    return shortName;
  }

  /**
   * Return the long name of the option, or null if none.
   */
  public String getLongName()
  {
    return longName;
  }

  /**
   * Return true if the argument takes an option.
   */
  public boolean getTakesArgument()
  {
    return argumentName != null;
  }

  /**
   * Return the name of the argument. If the option does not take an argument,
   * returns null.
   */
  public String getArgumentName()
  {
    return argumentName;
  }

  /**
   * Return the description of the option.
   */
  public String getDescription()
  {
    return description;
  }

  /**
   * Return true if this is a "joined" option, false otherwise.
   * Only the short form of an option can be joined; this will always
   * return false for an option which does not have a short form.
   */
  public boolean isJoined()
  {
    return joined;
  }

  /**
   * This is called by the parser when this option is recognized. It may be
   * called multiple times during a single parse. If this option takes an
   * argument, the argument will be passed in. Otherwise the argument will be
   * null.
   * 
   * @param argument the argument
   * @throws OptionException if the option or its argument is somehow invalid
   */
  public abstract void parsed(String argument) throws OptionException;
}
