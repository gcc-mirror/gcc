/* Currency.java -- Representation of a currency
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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

package java.util;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.text.NumberFormat;

/**
 * Representation of a currency for a particular locale.  Each currency
 * is identified by its ISO 4217 code, and only one instance of this
 * class exists per currency.  As a result, instances are created
 * via the <code>getInstance()</code> methods rather than by using
 * a constructor.
 *
 * @see java.util.Locale
 * @author Guilhem Lavaux  (guilhem.lavaux@free.fr)
 * @author Dalibor Topic (robilad@kaffe.org)
 * @author Bryce McKinlay (mckinlay@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.4
 */
public final class Currency 
  implements Serializable
{
  /**
   * For compatability with Sun's JDK
   */
  static final long serialVersionUID = -158308464356906721L;

  /**
   * The locale associated with this currency.
   *
   * @see #Currency(java.util.Locale)
   * @see #getInstance(java.util.Locale)
   * @see #getSymbol(java.util.Locale)
   * @serial ignored.
   */
  private transient Locale locale;

  /**
   * The resource bundle which maps the currency to
   * a ISO 4217 currency code.
   *
   * @see #getCurrencyCode()
   * @serial ignored.
   */
  private transient ResourceBundle res;

  /**
   * The ISO 4217 currency code associated with this
   * particular instance.
   *
   * @see #getCurrencyCode()
   * @serial the ISO 4217 currency code
   */
  private String currencyCode;

  /**
   * A cache of <code>Currency</code> instances to
   * ensure the singleton nature of this class.  The key
   * is the locale of the currency.
   *
   * @see #getInstance(java.util.Locale)
   * @see #readResolve()
   * @serial ignored.
   */
  private static transient Map cache;

  /**
   * Instantiates the cache.
   */
  static
  {
    cache = new HashMap();
  }

  /**
   * Default constructor for deserialization
   */
  private Currency ()
  {
  }

  /**
   * Constructor to create a <code>Currency</code> object
   * for a particular <code>Locale</code>.
   * All components of the given locale, other than the
   * country code, are ignored.  The results of calling this
   * method may vary over time, as the currency associated with
   * a particular country changes.  For countries without
   * a given currency (e.g. Antarctica), the result is null. 
   *
   * @param loc the locale for the new currency.
   */
  private Currency (Locale loc)
  {
    this.locale = loc;
    this.res = ResourceBundle.getBundle ("gnu.java.locale.LocaleInformation", 
      locale, ClassLoader.getSystemClassLoader());
    /* Retrieve the ISO4217 currency code */
    try
      {
	currencyCode = res.getString ("intlCurrencySymbol");
      }
    catch (Exception _)
      {
	currencyCode = null;
      }
  }

  /**
   * Returns the ISO4217 currency code of this currency.
   *
   * @return a <code>String</code> containing currency code.
   */
  public String getCurrencyCode ()
  {
    return currencyCode;
  }

  /**
   * Returns the number of digits which occur after the decimal point
   * for this particular currency.  For example, currencies such
   * as the U.S. dollar, the Euro and the Great British pound have two
   * digits following the decimal point to indicate the value which exists
   * in the associated lower-valued coinage (cents in the case of the first
   * two, pennies in the latter).  Some currencies such as the Japanese
   * Yen have no digits after the decimal point.  In the case of pseudo
   * currencies, such as IMF Special Drawing Rights, -1 is returned.
   *
   * @return the number of digits after the decimal separator for this currency.
   */   
  public int getDefaultFractionDigits ()
  {
    NumberFormat currency = NumberFormat.getCurrencyInstance (locale);
    
    return currency.getMaximumFractionDigits();
  }
    
  /**
   * Builds a new currency instance for this locale.
   * All components of the given locale, other than the
   * country code, are ignored.  The results of calling this
   * method may vary over time, as the currency associated with
   * a particular country changes.  For countries without
   * a given currency (e.g. Antarctica), the result is null. 
   *
   * @param locale a <code>Locale</code> instance.
   * @return a new <code>Currency</code> instance.
   * @throws NullPointerException if the locale or its
   *         country code is null.
   * @throws IllegalArgumentException if the country of
   *         the given locale is not a supported ISO3166 code.
   */ 
  public static Currency getInstance (Locale locale)
  {
    /**
     * The new instance must be the only available instance
     * for the currency it supports.  We ensure this happens,
     * while maintaining a suitable performance level, by
     * creating the appropriate object on the first call to
     * this method, and returning the cached instance on
     * later calls.
     */
    Currency newCurrency;

    /* Attempt to get the currency from the cache */
    newCurrency = (Currency) cache.get(locale);
    if (newCurrency == null)
      {
        /* Create the currency for this locale */
        newCurrency = new Currency (locale);
        /* Cache it */
        cache.put(locale, newCurrency);
      }
    /* Return the instance */
    return newCurrency;
  }

  /**
   * Builds the currency corresponding to the specified currency code.
   *
   * @param currencyCode a string representing a currency code.
   * @return a new <code>Currency</code> instance.
   * @throws NullPointerException if currencyCode is null.
   * @throws IllegalArgumentException if the supplied currency code
   *         is not a supported ISO 4217 code.
   */
  public static Currency getInstance (String currencyCode)
  {
    Locale[] allLocales = Locale.getAvailableLocales ();
    
    for (int i = 0;i < allLocales.length; i++)
      {
	Currency testCurrency = getInstance (allLocales[i]);
	
	if (testCurrency.getCurrencyCode() != null &&
	    testCurrency.getCurrencyCode().equals(currencyCode))
	  return testCurrency;
      }
    /* 
     * If we get this far, the code is not supported by any of
     * our locales.
     */
    throw new IllegalArgumentException("The currency code, " + currencyCode +
                                       ", is not supported.");
  }

  /**
   * This method returns the symbol which precedes or follows a
   * value in this particular currency.  In cases where there is no
   * such symbol for the currency, the ISO 4217 currency
   * code is returned.
   *
   * @return the currency symbol, or the ISO 4217 currency code if
   *         one doesn't exist.
   */
  public String getSymbol()
  {
    try
      {
        /* What does this return if there is no mapping? */
	return res.getString ("currencySymbol");
      }
    catch (Exception _)
      {
	return null;
      }
  }

  /**
   * <p>
   * This method returns the symbol which precedes or follows a
   * value in this particular currency.  The returned value is
   * the symbol used to denote the currency in the specified locale.
   * </p>
   * <p>
   * For example, a supplied locale may specify a different symbol
   * for the currency, due to conflicts with its own currency.
   * This would be the case with the American currency, the dollar.
   * Locales that also use a dollar-based currency (e.g. Canada, Australia)
   * need to differentiate the American dollar using 'US$' rather than '$'.
   * So, supplying one of these locales to <code>getSymbol()</code> would
   * return this value, rather than the standard '$'.
   * </p>
   * <p>
   * In cases where there is no such symbol for a particular currency,
   * the ISO 4217 currency code is returned.
   * </p>
   *
   * @param locale the locale to express the symbol in.
   * @return the currency symbol, or the ISO 4217 currency code if
   *         one doesn't exist.
   * @throws NullPointerException if the locale is null.
   */
  public String getSymbol(Locale locale)
  {
    // TODO. The behaviour is unclear if locale != this.locale.
    // First we need to implement fully LocaleInformation*.java

    /* 
     * FIXME: My reading of how this method works has this implementation
     * as wrong.  It should return a value relating to how the specified
     * locale handles the symbol for this currency.  This implementation
     * seems to just do a variation of getInstance(locale).
     */
    try
      {
	ResourceBundle localeResource = 
	  ResourceBundle.getBundle ("gnu.java.locale.LocaleInformation", 
				    locale, Currency.class.getClassLoader());

	if (localeResource.equals(res))
	  return localeResource.getString ("currencySymbol");
	else
	  return localeResource.getString ("intlCurrencySymbol");
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
    return getCurrencyCode();
  }

  /**
   * Resolves the deserialized object to the singleton instance for its
   * particular currency.  The currency code of the deserialized instance
   * is used to return the correct instance.
   *
   * @return the singleton instance for the currency specified by the
   *         currency code of the deserialized object.  This replaces
   *         the deserialized object as the returned object from
   *         deserialization.
   * @throws ObjectStreamException if a problem occurs with deserializing
   *         the object.
   */
  private Object readResolve()
    throws ObjectStreamException
  {
    return getInstance(currencyCode);
  }

}
