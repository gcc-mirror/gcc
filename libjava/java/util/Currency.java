/* Currency.java -- Representation of a currency
   Copyright (C) 2003 Free Software Foundation, Inc.

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
package java.util;

import java.io.Serializable;
import java.util.ResourceBundle;
import java.util.Locale;
import java.text.NumberFormat;

public final class Currency implements Serializable
{
  static final long serialVersionUID = -158308464356906721L;

  private Locale locale;
  private ResourceBundle res;

  // For deserialization
  private Currency ()
  {
  }

  private Currency (Locale loc)
  {
    this.locale = loc;
    this.res = ResourceBundle.getBundle ("gnu.java.locale.LocaleInformation", locale);
  }

  /**
   * Returns the ISO4217 currency code of this currency.
   *
   * @return a <code>String</code> containing currency code.
   */
  public String getCurrencyCode ()
  {
    try
      {
	return res.getString ("intlCurrencySymbol");
      }
    catch (Exception _)
      {
	return null;
      }
  }

  /**
   * @return number of digits after decimal separator for this currency.
   */   
  public int getDefaultFractionDigits ()
  {
    NumberFormat currency = NumberFormat.getCurrencyInstance (locale);
    
    return currency.getMaximumFractionDigits();
  }
    
  /**
   * Builds a new currency instance for this locale.
   *
   * @param locale a <code>Locale</code> instance.
   * 
   * @return a new <code>Currency</code> instance.
   */ 
  public static Currency getInstance (Locale locale)
  {
    return new Currency (locale);
  }

  /**
   * Builds the currency corresponding to the specified currency code.
   *
   * @param currencyCode a string representing a currency code.
   *
   * @return a new <code>Currency</code> instance.
   */
  public static Currency getInstance (String currencyCode)
  {
    Locale[] all_locales = Locale.getAvailableLocales ();
    
    for (int i=0;i<all_locales.length;i++)
      {
	Currency test_currency = getInstance (all_locales[i]);
	
	if (test_currency.getCurrencyCode() != null &&
	    test_currency.getCurrencyCode().equals(currencyCode))
	  return test_currency;
      }
    
    return null;
  }

  /**
   * This method returns the currency symbol.
   *
   * @return the currency symbol.
   */
  public String getSymbol()
  {
    try
      {
	return res.getString ("currencySymbol");
      }
    catch (Exception _)
      {
	return null;
      }
  }

  /**
   * This methods returns the currency symbol expressed in the specified locale.
   *
   * @param locale the locale to express the symbol in.
   * @return the currency symbol.
   */
  public String getSymbol(Locale locale)
  {
    // TODO. The behaviour is unclear if locale != this.locale.
    // First we need to implement fully LocaleInformation*.java
    try
      {
	ResourceBundle res = ResourceBundle.getBundle ("gnu.java.locale.LocaleInformation", locale);

	if (res.equals(this.res))
	  return res.getString ("currencySymbol");
	else
	  return res.getString ("intlCurrencySymbol");
      }
    catch (Exception e1)
      {
	try
	  {
	    return res.getString ("intlCurrencySymbol");
	  }
	catch (Exception e2)
	  {
	    return null;
	  }
      }
  }

  /**
   * Returns the international ISO4217 currency code of this currency.
   *
   * @return a <code>String</code> containing the ISO4217 currency code.
   */
  public String toString()
  {
    try
      {
	return res.getString ("intlCurrencySymbol");
      }
    catch (Exception _)
      {
	return "(unknown currency)";
      }
  }
}
