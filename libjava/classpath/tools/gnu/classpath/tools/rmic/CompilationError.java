/* CompilationError.java -- Thrown on compilation error.
   Copyright (C) 2006 Free Software Foundation

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
*/

package gnu.classpath.tools.rmic;

/**
 * This error is thrown when the target being compiled has illegal
 * strutures.
 * 
 * @author Audrius Meskauskas, Lithuania (audriusa@Bioinformatics.org)
 */
public class CompilationError extends Error
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;  

  /**
   * Create error with explaining message and cause.
   */
  public CompilationError(String message, Throwable cause)
  {
    super(message, cause);
  }

  /**
   * Create error with explaining message
   */
  public CompilationError(String message)
  {
    super(message);
  }
}
