/* DatabaseMetaData.java -- Information about the database itself.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

package java.sql;

public interface DatabaseMetaData 
{
  /**
   * It is unknown whether or not the procedure returns a result.
   */
  int procedureResultUnknown = 0;

  /**
   * The procedure does not return a result.
   */
  int procedureNoResult = 1;

  /**
   * The procedure returns a result.
   */
  int procedureReturnsResult = 2;

  /**
   * The column type is unknown.
   */
  int procedureColumnUnknown = 0;

  /**
   * The column type is input.
   */
  int procedureColumnIn = 1;

  /**
   * The column type is input/output.
   */
  int procedureColumnInOut = 2;

  /**
   * The column type is output
   */
  int procedureColumnOut = 4;

  /**
   * The column is used for return values.
   */
  int procedureColumnReturn = 5;

  /**
   * The column is used for storing results
   */
  int procedureColumnResult = 3;

  /**
   * NULL values are not allowed.
   */
  int procedureNoNulls = 0;

  /**
   * NULL values are allowed.
   */
  int procedureNullable = 1;

  /**
   * It is unknown whether or not NULL values are allowed.
   */
  int procedureNullableUnknown = 2;

  /**
   * The column does not allow NULL
   */
  int columnNoNulls = 0;

  /**
   * The column does allow NULL
   */
  int columnNullable = 1;

  /**
   * It is unknown whether or not the column allows NULL
   */
  int columnNullableUnknown = 2;

  /**
   * The best row's scope is only guaranteed to be valid so long as the
   * row is actually being used.
   */
  int bestRowTemporary = 0;

  /**
   * The best row identifier is valid to the end of the transaction.
   */
  int bestRowTransaction = 1;

  /**
   * The best row identifier is valid to the end of the session.
   */
  int bestRowSession = 2;

  /**
   * The best row may or may not be a pseudo-column.
   */
  int bestRowUnknown = 0;

  /**
   * The best row identifier is not a pseudo-column.
   */
  int bestRowNotPseudo = 1;

  /**
   * The best row identifier is a pseudo-column.
   */
  int bestRowPseudo = 2;

  /**
   * It is unknown whether or not the version column is a pseudo-column.
   */
  int versionColumnUnknown = 0;

  /**
   * The version column is not a pseudo-column
   */
  int versionColumnNotPseudo = 1;

  /**
   * The version column is a pseudo-column
   */
  int versionColumnPseudo = 2;

  /**
   * Foreign key changes are cascaded in updates or deletes.
   */
  int importedKeyCascade = 0;

  /**
   * Column may not be updated or deleted in use as a foreign key.
   */
  int importedKeyRestrict = 1;

  /**
   * When primary key is updated or deleted, the foreign key is set to NULL.
   */
  int importedKeySetNull = 2;

  /**
   * If the primary key is a foreign key, it cannot be udpated or deleted.
   */
  int importedKeyNoAction = 3;

  /**
   * If the primary key is updated or deleted, the foreign key is set to
   * a default value.
   */
  int importedKeySetDefault = 4;

  /**
   * Wish I knew what this meant.
   */
  int importedKeyInitiallyDeferred = 5;

  /**
   * Wish I knew what this meant.
   */
  int importedKeyInitiallyImmediate = 6;

  /**
   * Wish I knew what this meant.
   */
  int importedKeyNotDeferrable = 7;

  /**
   * A NULL value is not allowed for this data type.
   */
  int typeNoNulls = 0;

  /**
   * A NULL value is allowed for this data type.
   */
  int typeNullable = 1;

  /**
   * It is unknown whether or not NULL values are allowed for this data type.
   */
  int typeNullableUnknown = 2;

  /**
   * Where clauses are not supported for this type.
   */
  int typePredNone = 0;

  /**
   * Only "WHERE..LIKE" style WHERE clauses are allowed on this data type.
   */
  int typePredChar = 1;

  /**
   * All WHERE clauses except "WHERE..LIKE" style are allowed on this data type.
   */
  int typePredBasic = 2;

  /**
   * Any type of WHERE clause is allowed for this data type.
   */
  int typeSearchable = 3;

  /**
   * This column contains table statistics.
   */
  short tableIndexStatistic = 0;

  /**
   * This table index is clustered.
   */
  short tableIndexClustered = 1;

  /**
   * This table index is hashed.
   */
  short tableIndexHashed = 2;

  /**
   * This table index is of another type.
   */
  short tableIndexOther = 3;

  short attributeNoNulls = 0;

  short attributeNullable = 1;

  short attributeNullableUnknown = 2;

  int sqlStateXOpen = 1;

  int sqlStateSQL99 = 2;

  /**
   * This method tests whether or not all the procedures returned by
   * the <code>getProcedures</code> method can be called by this user.
   *
   * @return <code>true</code> if all the procedures can be called,
   * <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean allProceduresAreCallable() throws SQLException;

  /**
   * This method tests whether or not all the table returned by the
   * <code>getTables</code> method can be selected by this user.
   *
   * @return <code>true</code> if all the procedures can be called,
   * <code>false</code> otherwise.
   *
   * @exception SQLException If an error occurs.
   */
  boolean allTablesAreSelectable() throws SQLException;

  /**
   * This method returns the URL for this database.
   *
   * @return The URL string for this database, or <code>null</code> if it
   *         is not known.
   * @exception SQLException If an error occurs.
   */
  String getURL() throws SQLException;

  /**
   * This method returns the database username for this connection.
   *
   * @return The database username.
   * @exception SQLException If an error occurs.
   */
  String getUserName() throws SQLException;

  /**
   * This method tests whether or not the database is in read only mode.
   *
   * @return <code>true</code> if the database is in read only mode,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean isReadOnly() throws SQLException;

  /**
   * This method tests whether or not NULL's sort as high values.
   *
   * @return <code>true</code> if NULL's sort as high values, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean nullsAreSortedHigh() throws SQLException;

  /**
   * This method tests whether or not NULL's sort as low values.
   *
   * @return <code>true</code> if NULL's sort as low values, <code>false</code>
   * otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean nullsAreSortedLow() throws SQLException;

  /**
   * This method tests whether or not NULL's sort as high values.
   *
   * @return <code>true</code> if NULL's sort as high values, <code>false</code>
   * otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean nullsAreSortedAtStart() throws SQLException;

  /**
   * This method test whether or not NULL's are sorted to the end
   * of the list regardless of ascending or descending sort order.
   *
   * @return <code>true</code> if NULL's always sort to the end,
   * <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean nullsAreSortedAtEnd() throws SQLException;

  /**
   * This method returns the name of the database product.
   *
   * @return The database product.
   * @exception SQLException If an error occurs.
   */
  String getDatabaseProductName() throws SQLException;

  /**
   * This method returns the version of the database product.
   *
   * @return The version of the database product.
   * @exception SQLException If an error occurs.
   */
  String getDatabaseProductVersion() throws SQLException;

  /**
   * This method returns the name of the JDBC driver.
   *
   * @return The name of the JDBC driver.
   * @exception SQLException If an error occurs.
   */
  String getDriverName() throws SQLException;

  /**
   * This method returns the version of the JDBC driver.
   *
   * @return The version of the JDBC driver.
   * @exception SQLException If an error occurs.
   */
  String getDriverVersion() throws SQLException;

  /**
   * This method returns the major version number of the JDBC driver.
   *
   * @return The major version number of the JDBC driver.
   */
  int getDriverMajorVersion();

  /**
   * This method returns the minor version number of the JDBC driver.
   *
   * @return The minor version number of the JDBC driver.
   */
  int getDriverMinorVersion();

  /**
   * This method tests whether or not the database uses local files to
   * store tables.
   *
   * @return <code>true</code> if the database uses local files, 
   * <code>false</code> otherwise.
   *
   * @exception SQLException If an error occurs.
   */
  boolean usesLocalFiles() throws SQLException;

  /**
   * This method tests whether or not the database uses a separate file for
   * each table.
   *
   * @return <code>true</code> if the database uses a separate file for each
   * table </code>false</code> otherwise.
   *
   * @exception SQLException If an error occurs.
   */
  boolean usesLocalFilePerTable() throws SQLException;

  /**
   * This method tests whether or not the database supports identifiers
   * with mixed case.
   *
   * @return <code>true</code> if the database supports mixed case identifiers,
   * <code>false</code> otherwise.
   *
   * @exception SQLException If an error occurs.
   */
  boolean supportsMixedCaseIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database treats mixed case
   * identifiers as all upper case.
   *
   * @exception <code>true</code> if the database treats all identifiers as
   *            upper case, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean storesUpperCaseIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database treats mixed case
   * identifiers as all lower case.
   *
   * @exception <code>true</code> if the database treats all identifiers as
   *            lower case, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean storesLowerCaseIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database stores mixed case 
   * identifers even if it treats them as case insensitive.
   *
   * @return <code>true</code> if the database stores mixed case identifiers,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean storesMixedCaseIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database supports quoted identifiers
   * with mixed case.
   *
   * @return <code>true</code> if the database supports mixed case quoted
   *         identifiers, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsMixedCaseQuotedIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database treats mixed case
   * quoted identifiers as all upper case.
   *
   * @exception <code>true</code> if the database treats all quoted identifiers 
   *            as upper case, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean storesUpperCaseQuotedIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database treats mixed case
   * quoted identifiers as all lower case.
   *
   * @exception <code>true</code> if the database treats all quoted identifiers 
   *            as lower case, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean storesLowerCaseQuotedIdentifiers() throws SQLException;

  /**
   * This method tests whether or not the database stores mixed case 
   * quoted identifers even if it treats them as case insensitive.
   *
   * @return <code>true</code> if the database stores mixed case quoted 
   *         identifiers, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean storesMixedCaseQuotedIdentifiers() throws SQLException;

  /**
   * This metohd returns the quote string for SQL identifiers.
   *
   * @return The quote string for SQL identifers, or a space if quoting
   *         is not supported.
   * @exception SQLException If an error occurs.
   */
  String getIdentifierQuoteString() throws SQLException;

  /**
   * This method returns a comma separated list of all the SQL keywords in
   * the database that are not in SQL92.
   *
   * @return The list of SQL keywords not in SQL92.
   * @exception SQLException If an error occurs.
   */
  String getSQLKeywords() throws SQLException;

  /**
   * This method returns a comma separated list of math functions.
   *
   * @return The list of math functions.
   * @exception SQLException If an error occurs.
   */
  String getNumericFunctions() throws SQLException;

  /**
   * This method returns a comma separated list of string functions.
   *
   * @return The list of string functions.
   * @exception SQLException If an error occurs.
   */
  String getStringFunctions() throws SQLException;

  /**
   * This method returns a comma separated list of of system functions.
   *
   * @return A comma separated list of system functions.
   * @exception SQLException If an error occurs.
   */
  String getSystemFunctions() throws SQLException;

  /**
   * This method returns comma separated list of time/date functions.
   * 
   * @return The list of time/date functions.
   * @exception SQLException If an error occurs.
   */
  String getTimeDateFunctions() throws SQLException;

  /**
   * This method returns the string used to escape wildcards in search strings.
   *
   * @return The string used to escape wildcards in search strings.
   * @exception SQLException If an error occurs.
   */
  String getSearchStringEscape() throws SQLException;

  /**
   * This methods returns non-standard characters that can appear in 
   * unquoted identifiers.
   *
   * @return Non-standard characters that can appear in unquoted identifiers.
   * @exception SQLException If an error occurs.
   */
  String getExtraNameCharacters() throws SQLException;

  /**
   * This method tests whether or not the database supports
   * "ALTER TABLE ADD COLUMN"
   *
   * @return <code>true</code> if column add supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsAlterTableWithAddColumn() throws SQLException;

  /**
   * This method tests whether or not the database supports
   * "ALTER TABLE DROP COLUMN"
   *
   * @return <code>true</code> if column drop supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsAlterTableWithDropColumn() throws SQLException;

  /**
   * This method tests whether or not column aliasing is supported.
   *
   * @return <code>true</code> if column aliasing is supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsColumnAliasing() throws SQLException;

  /**
   * This method tests whether the concatenation of a NULL and non-NULL
   * value results in a NULL.  This will always be true in fully JDBC compliant
   * drivers.
   *
   * @return <code>true</code> if concatenating NULL and a non-NULL value
   *         returns a NULL, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean nullPlusNonNullIsNull() throws SQLException;

  /**
   * Tests whether or not CONVERT is supported.
   *
   * @return <code>true</code> if CONVERT is supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsConvert() throws SQLException;

  /**
   * This method tests whether or not CONVERT can be performed between the
   * specified types.  The types are contants from <code>Types</code>.
   *
   * @param fromType The SQL type to convert from.
   * @param toType The SQL type to convert to.
   * @return <code>true</code> if the conversion can be performed,
   *         <code>false</code> otherwise.
   * @see Types
   */
  boolean supportsConvert(int fromType, int toType) throws
      SQLException;

  /**
   * This method tests whether or not table correlation names are 
   * supported.  This will be always be <code>true</code> in a fully JDBC
   * compliant driver.
   *
   * @return <code>true</code> if table correlation names are supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsTableCorrelationNames() throws SQLException;

  /**
   * This method tests whether correlation names must be different from the
   * name of the table.
   *
   * @return <code>true</code> if the correlation name must be different from
   *         the table name, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsDifferentTableCorrelationNames() throws SQLException;

  /**
   * This method tests whether or not expressions are allowed in an
   * ORDER BY lists.
   *
   * @return <code>true</code> if expressions are allowed in ORDER BY
   *         lists, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsExpressionsInOrderBy() throws SQLException;

  /**
   * This method tests whether or ORDER BY on a non-selected column is
   * allowed.
   *
   * @return <code>true</code> if a non-selected column can be used in an
   *         ORDER BY, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsOrderByUnrelated() throws SQLException;

  /**
   * This method tests whether or not GROUP BY is supported.
   *
   * @return <code>true</code> if GROUP BY is supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsGroupBy() throws SQLException;

  /**
   * This method tests whether GROUP BY on a non-selected column is
   * allowed.
   *
   * @return <code>true</code> if a non-selected column can be used in a
   *         GROUP BY, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsGroupByUnrelated() throws SQLException;

  /**
   * This method tests whether or not a GROUP BY can add columns not in the
   * select if it includes all the columns in the select.
   *
   * @return <code>true</code> if GROUP BY an add columns provided it includes
   *         all columns in the select, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsGroupByBeyondSelect() throws SQLException;

  /**
   * This method tests whether or not the escape character is supported in
   * LIKE expressions.  A fully JDBC compliant driver will always return
   * <code>true</code>.
   *
   * @return <code>true</code> if escapes are supported in LIKE expressions,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsLikeEscapeClause() throws SQLException;

  /**
   * This method tests whether multiple result sets for a single statement are
   * supported.
   *
   * @return <code>true</code> if multiple result sets are supported for a 
   *         single statement, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsMultipleResultSets() throws SQLException;

  /**
   * This method test whether or not multiple transactions may be open
   * at once, as long as they are on different connections.
   *
   * @return <code>true</code> if multiple transactions on different
   *         connections are supported, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsMultipleTransactions() throws SQLException;

  /**
   * This method tests whether or not columns can be defined as NOT NULL.  A
   * fully JDBC compliant driver always returns <code>true</code>.
   *
   * @return <code>true</code> if NOT NULL columns are supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsNonNullableColumns() throws SQLException;

  /**
   * This method tests whether or not the minimum grammer for ODBC is supported.
   * A fully JDBC compliant driver will always return <code>true</code>.
   *
   * @return <code>true</code> if the ODBC minimum grammar is supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsMinimumSQLGrammar() throws SQLException;

  /**
   * This method tests whether or not the core grammer for ODBC is supported.
   *
   * @return <code>true</code> if the ODBC core grammar is supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCoreSQLGrammar() throws SQLException;

  /**
   * This method tests whether or not the extended grammer for ODBC is supported.
   *
   * @return <code>true</code> if the ODBC extended grammar is supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsExtendedSQLGrammar() throws SQLException;

  /**
   * This method tests whether or not the ANSI92 entry level SQL
   * grammar is supported.  A fully JDBC compliant drivers must return
   * <code>true</code>.
   *
   * @return <code>true</code> if the ANSI92 entry level SQL grammar is
   *         supported, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsANSI92EntryLevelSQL() throws SQLException;

  /**
   * This method tests whether or not the ANSI92 intermediate SQL
   * grammar is supported.  
   *
   * @return <code>true</code> if the ANSI92 intermediate SQL grammar is
   *         supported, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsANSI92IntermediateSQL() throws SQLException;

  /**
   * This method tests whether or not the ANSI92 full SQL
   * grammar is supported.  
   *
   * @return <code>true</code> if the ANSI92 full SQL grammar is
   *         supported, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsANSI92FullSQL() throws SQLException;

  /**
   * This method tests whether or not the SQL integrity enhancement
   * facility is supported.
   *
   * @return <code>true</code> if the integrity enhancement facility is
   *         supported, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsIntegrityEnhancementFacility() throws SQLException;

  /**
   * This method tests whether or not the database supports outer joins.
   *
   * @return <code>true</code> if outer joins are supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsOuterJoins() throws SQLException;

  /**
   * This method tests whether or not the database supports full outer joins.
   *
   * @return <code>true</code> if full outer joins are supported, 
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsFullOuterJoins() throws SQLException;

  /**
   * This method tests whether or not the database supports limited outer joins.
   *
   * @return <code>true</code> if limited outer joins are supported, 
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsLimitedOuterJoins() throws SQLException;

  /**
   * This method returns the vendor's term for "schema".
   *
   * @return The vendor's term for schema.
   * @exception SQLException if an error occurs.
   */
  String getSchemaTerm() throws SQLException;

  /**
   * This method returns the vendor's term for "procedure".
   *
   * @return The vendor's term for procedure.
   * @exception SQLException if an error occurs.
   */
  String getProcedureTerm() throws SQLException;

  /**
   * This method returns the vendor's term for "catalog".
   *
   * @return The vendor's term for catalog.
   * @exception SQLException if an error occurs.
   */
  String getCatalogTerm() throws SQLException;

  /**
   * This method tests whether a catalog name appears at the beginning of
   * a fully qualified table name.
   *
   * @return <code>true</code> if the catalog name appears at the beginning,
   *         <code>false</code> if it appears at the end.
   * @exception SQLException If an error occurs.
   */
  boolean isCatalogAtStart() throws SQLException;

  /**
   * This method returns the separator between the catalog name and the
   * table name.
   *
   * @return The separator between the catalog name and the table name.
   * @exception SQLException If an error occurs.
   */
  String getCatalogSeparator() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in a data
   * manipulation statement.
   *
   * @return <code>true</code> if a catalog name can appear in a data
   *         manipulation statement, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSchemasInDataManipulation() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in a procedure
   * call
   *
   * @return <code>true</code> if a catalog name can appear in a procedure
   *         call, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSchemasInProcedureCalls() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in a table definition.
   *
   * @return <code>true</code> if a catalog name can appear in a table
   *         definition, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSchemasInTableDefinitions() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in an index definition.
   *
   * @return <code>true</code> if a catalog name can appear in an index
   *         definition, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSchemasInIndexDefinitions() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in privilege definitions.
   *
   * @return <code>true</code> if a catalog name can appear in privilege
   *         definition, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSchemasInPrivilegeDefinitions() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in a data
   * manipulation statement.
   *
   * @return <code>true</code> if a catalog name can appear in a data
   *         manipulation statement, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCatalogsInDataManipulation() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in a procedure
   * call
   *
   * @return <code>true</code> if a catalog name can appear in a procedure
   *         call, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCatalogsInProcedureCalls() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in a table definition.
   *
   * @return <code>true</code> if a catalog name can appear in a table
   *         definition, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCatalogsInTableDefinitions() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in an index definition.
   *
   * @return <code>true</code> if a catalog name can appear in an index
   *         definition, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCatalogsInIndexDefinitions() throws SQLException;

  /**
   * This method tests whether a catalog name can appear in privilege definitions.
   *
   * @return <code>true</code> if a catalog name can appear in privilege
   *         definition, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCatalogsInPrivilegeDefinitions() throws SQLException;

  /**
   * This method tests whether or not that database supports positioned
   * deletes.
   *
   * @return <code>true</code> if positioned deletes are supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsPositionedDelete() throws SQLException;

  /**
   * This method tests whether or not that database supports positioned
   * updates.
   *
   * @return <code>true</code> if positioned updates are supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsPositionedUpdate() throws SQLException;

  /**
   * This method tests whether or not SELECT FOR UPDATE is supported by the
   * database.
   *
   * @return <code>true</code> if SELECT FOR UPDATE is supported 
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSelectForUpdate() throws SQLException;

  /**
   * This method tests whether or not stored procedures are supported on
   * this database.
   *
   * @return <code>true</code> if stored procedures are supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsStoredProcedures() throws SQLException;

  /**
   * This method tests whether or not subqueries are allowed in comparisons.
   * A fully JDBC compliant driver will always return <code>true</code>.
   *
   * @return <code>true</code> if subqueries are allowed in comparisons,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSubqueriesInComparisons() throws SQLException;

  /**
   * This method tests whether or not subqueries are allowed in exists
   * expressions.  A fully JDBC compliant driver will always return
   * <code>true</code>.
   *
   * @return <code>true</code> if subqueries are allowed in exists 
   *         expressions, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSubqueriesInExists() throws SQLException;

  /**
   * This method tests whether subqueries are allowed in IN statements.
   * A fully JDBC compliant driver will always return <code>true</code>.
   *
   * @return <code>true</code> if the driver supports subqueries in IN
   *         statements, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSubqueriesInIns() throws SQLException;

  /**
   * This method tests whether or not subqueries are allowed in quantified
   * expressions.  A fully JDBC compliant driver will always return
   * <code>true</code>.
   *
   * @return <code>true</code> if subqueries are allowed in quantified 
   *         expressions, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsSubqueriesInQuantifieds() throws SQLException;

  /**
   * This method test whether or not correlated subqueries are allowed. A
   * fully JDBC compliant driver will always return <code>true</code>.
   *
   * @return <code>true</code> if correlated subqueries are allowed,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsCorrelatedSubqueries() throws SQLException;

  /**
   * This method tests whether or not the UNION statement is supported.
   *
   * @return <code>true</code> if UNION is supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsUnion() throws SQLException;

  /**
   * This method tests whether or not the UNION ALL statement is supported.
   *
   * @return <code>true</code> if UNION ALL is supported, <code>false</code>
   *         otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsUnionAll() throws SQLException;

  /**
   * This method tests whether or not the database supports cursors
   * remaining open across commits.
   *
   * @return <code>true</code> if cursors can remain open across commits,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsOpenCursorsAcrossCommit() throws SQLException;

  /**
   * This method tests whether or not the database supports cursors
   * remaining open across rollbacks.
   *
   * @return <code>true</code> if cursors can remain open across rollbacks,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsOpenCursorsAcrossRollback() throws SQLException;

  /**
   * This method tests whether or not the database supports statements
   * remaining open across commits.
   *
   * @return <code>true</code> if statements can remain open across commits,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsOpenStatementsAcrossCommit() throws SQLException;

  /**
   * This method tests whether or not the database supports statements
   * remaining open across rollbacks.
   *
   * @return <code>true</code> if statements can remain open across rollbacks,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsOpenStatementsAcrossRollback() throws SQLException;

  /**
   * This method returns the number of hex characters allowed in an inline
   * binary literal.
   *
   * @return The number of hex characters allowed in a binary literal, 0 meaning
   *         either an unknown or unlimited number.
   * @exception SQLException If an error occurs.
   */
  int getMaxBinaryLiteralLength() throws SQLException;

  /**
   * This method returns the maximum length of a character literal.
   * 
   * @return The maximum length of a character literal.
   * @exception SQLException If an error occurs.
   */
  int getMaxCharLiteralLength() throws SQLException;

  /**
   * This method returns the maximum length of a column name.
   *
   * @return The maximum length of a column name.
   * @exception SQLException If an error occurs.
   */
  int getMaxColumnNameLength() throws SQLException;

  /**
   * This method returns the maximum number of columns in a GROUP BY statement.
   *
   * @return The maximum number of columns in a GROUP BY statement.
   * @exception SQLException If an error occurs.
   */
  int getMaxColumnsInGroupBy() throws SQLException;

  /**
   * This method returns the maximum number of columns in an index.
   *
   * @return The maximum number of columns in an index.
   * @exception SQLException If an error occurs.
   */
  int getMaxColumnsInIndex() throws SQLException;

  /**
   * This method returns the maximum number of columns in an ORDER BY statement.
   *
   * @return The maximum number of columns in an ORDER BY statement.
   * @exception SQLException If an error occurs.
   */
  int getMaxColumnsInOrderBy() throws SQLException;

  /**
   * This method returns the maximum number of columns in a SELECT statement.
   *
   * @return The maximum number of columns in a SELECT statement.
   * @exception SQLException If an error occurs.
   */
  int getMaxColumnsInSelect() throws SQLException;

  /**
   * This method returns the maximum number of columns in a table.
   *
   * @return The maximum number of columns in a table.
   * @exception SQLException If an error occurs.
   */
  int getMaxColumnsInTable() throws SQLException;

  /**
   * This method returns the maximum number of connections this client
   * can have to the database.
   *
   * @return The maximum number of database connections.
   * @SQLException If an error occurs.
   */
  int getMaxConnections() throws SQLException;

  /**
   * This method returns the maximum length of a cursor name.
   *
   * @return The maximum length of a cursor name.
   * @exception SQLException If an error occurs.
   */
  int getMaxCursorNameLength() throws SQLException;

  /**
   * This method returns the maximum length of an index.
   *
   * @return The maximum length of an index.
   * @exception SQLException If an error occurs.
   */
  int getMaxIndexLength() throws SQLException;

  /**
   * This method returns the maximum length of a schema name.
   *
   * @return The maximum length of a schema name.
   * @exception SQLException If an error occurs.
   */
  int getMaxSchemaNameLength() throws SQLException;

  /**
   * This method returns the maximum length of a procedure name.
   *
   * @return The maximum length of a procedure name.
   * @exception SQLException If an error occurs.
   */
  int getMaxProcedureNameLength() throws SQLException;

  /**
   * This method returns the maximum length of a catalog name.
   *
   * @return The maximum length of a catalog name.
   * @exception SQLException If an error occurs.
   */
  int getMaxCatalogNameLength() throws SQLException;

  /**
   * This method returns the maximum size of a row in bytes.
   *
   * @return The maximum size of a row.
   * @exception SQLException If an error occurs.
   */
  int getMaxRowSize() throws SQLException;

  /**
   * This method tests whether or not the maximum row size includes BLOB's
   *
   * @return <code>true</code> if the maximum row size includes BLOB's,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean doesMaxRowSizeIncludeBlobs() throws SQLException;

  /**
   * This method includes the maximum length of a SQL statement.
   *
   * @return The maximum length of a SQL statement.
   * @exception SQLException If an error occurs.
   */
  int getMaxStatementLength() throws SQLException;

  /**
   * This method returns the maximum number of statements that can be
   * active at any time.
   *
   * @return The maximum number of statements that can be active at any time.
   * @exception SQLException If an error occurs.
   */
  int getMaxStatements() throws SQLException;

  /**
   * This method returns the maximum length of a table name.
   *
   * @return The maximum length of a table name.
   * @exception SQLException If an error occurs.
   */
  int getMaxTableNameLength() throws SQLException;

  /**
   * This method returns the maximum number of tables that may be referenced
   * in a SELECT statement.
   *
   * @return The maximum number of tables allowed in a SELECT statement.
   * @exception SQLException If an error occurs.
   */
  int getMaxTablesInSelect() throws SQLException;

  /**
   * This method returns the maximum length of a user name.
   *
   * @return The maximum length of a user name.
   * @exception SQLException If an error occurs.
   */
  int getMaxUserNameLength() throws SQLException;

  /**
   * This method returns the default transaction isolation level of the
   * database.
   *
   * @return The default transaction isolation level of the database.
   * @exception SQLException If an error occurs.
   * @see Connection
   */
  int getDefaultTransactionIsolation() throws SQLException;

  /**
   * This method tests whether or not the database supports transactions.
   *
   * @return <code>true</code> if the database supports transactions,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsTransactions() throws SQLException;

  /**
   * This method tests whether or not the database supports the specified
   * transaction isolation level.
   *
   * @param level The transaction isolation level.
   *
   * @return <code>true</code> if the specified transaction isolation level
   *         is supported, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsTransactionIsolationLevel(int level) throws
      SQLException;

  /**
   * This method tests whether or not DDL and DML statements allowed within 
   * the same transaction.
   *
   * @return <code>true</code> if DDL and DML statements are allowed in the
   *         same transaction, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsDataDefinitionAndDataManipulationTransactions()
      throws SQLException;

  /**
   * This method tests whether or not only DML statement are allowed
   * inside a transaction.
   *
   * @return <code>true</code> if only DML statements are allowed in
   *         transactions, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsDataManipulationTransactionsOnly() throws
      SQLException;

  /**
   * This method tests whether or not a DDL statement will cause the
   * current transaction to be automatically committed.
   *
   * @return <code>true</code> if DDL causes an immediate transaction commit,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean dataDefinitionCausesTransactionCommit() throws SQLException;

  /**
   * This method tests whether or not DDL statements are ignored in
   * transactions.
   *
   * @return <code>true</code> if DDL statements are ignored in transactions,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean dataDefinitionIgnoredInTransactions() throws SQLException;

  /**
   * This method returns a list of all the stored procedures matching the
   * specified pattern in the given schema and catalog.  This is returned
   * a <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>PROCEDURE_CAT - The catalog the procedure is in, which may be 
   * <code>null</code>.
   * <li>PROCEDURE_SCHEM - The schema the procedures is in, which may be
   * <code>null</code>.
   * <li>PROCEDURE_NAME - The name of the procedure.
   * <li>Unused
   * <li>Unused
   * <li>Unused
   * <li>REMARKS - A description of the procedure
   * <li>PROCEDURE_TYPE - Indicates the return type of the procedure, which 
   * is one of the contstants defined in this class 
   * (<code>procedureResultUnknown</code>, <code>procedureNoResult</code>, or
   * <code>procedureReturnsResult</code>).
   * </ol>
   *
   * @param catalog The name of the catalog to return stored procedured from,
   *        or "" to return procedures from all catalogs.
   * @param schemaPattern A schema pattern for the schemas to return stored
   *        procedures from, or "" to return procedures from all schemas.
   * @param namePattern The pattern of procedures names to return.
   * @returns A <code>ResultSet</code> with all the requested procedures.
   * @exception SQLException If an error occurs.
   */
  ResultSet getProcedures(String catalog, String schemaPattern, String
      procedureNamePattern) throws SQLException;

  /**
   * This method returns a list of the parameter and result columns for
   * the requested stored procedures.  This is returned in the form of a
   * <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>PROCEDURE_CAT - The catalog the procedure is in, which may be 
   * <code>null</code>.
   * <li>PROCEDURE_SCHEM - The schema the procedures is in, which may be
   * <code>null</code>.
   * <li>PROCEDURE_NAME - The name of the procedure.
   * <li>COLUMN_NAME - The name of the column
   * <li>COLUMN_TYPE - The type of the column, which will be one of the
   * contants defined in this class (<code>procedureColumnUnknown</code>,
   * <code>procedureColumnIn</code>, <code>procedureColumnInOut</code>,
   * <code>procedureColumnOut</code>, <code>procedureColumnReturn</code>,
   * or <code>procedureColumnResult</code>).
   * <li>DATA_TYPE - The SQL type of the column. This is one of the constants
   * defined in <code>Types</code>.
   * <li>TYPE_NAME - The string name of the data type for this column.
   * <li>PRECISION - The precision of the column.
   * <li>LENGTH - The length of the column in bytes
   * <li>SCALE - The scale of the column.
   * <li>RADIX - The radix of the column.
   * <li>NULLABLE - Whether or not the column is NULLABLE.  This is one of
   * the constants defined in this class (<code>procedureNoNulls</code>,
   * <code>procedureNullable</code>, or <code>procedureNullableUnknown</code>)
   * <li>REMARKS - A description of the column.
   * </ol>
   *
   * @param catalog The name of the catalog to return stored procedured from,
   *        or "" to return procedures from all catalogs.
   * @param schemaPattern A schema pattern for the schemas to return stored
   *        procedures from, or "" to return procedures from all schemas.
   * @param namePattern The pattern of procedures names to return.
   * @param columnPattern The pattern of column names to return.
   * @returns A <code>ResultSet</code> with all the requested procedures.
   * @exception SQLException If an error occurs.
   */
  ResultSet getProcedureColumns(String catalog, String schemaPattern,
      String procedureNamePattern, String columnNamePattern) throws
      SQLException;

  /**
   * This method returns a list of the requested table as a   
   * <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>TABLE_CAT - The catalog the table is in, which may be <code>null</code>.
   * <li>TABLE_SCHEM - The schema the table is in, which may be <code>null</code>.
   * <li>TABLE_NAME - The name of the table.
   * <li>TABLE_TYPE - A string describing the table type.  This will be one
   * of the values returned by the <code>getTableTypes()</code> method.
   * <li>REMARKS - Comments about the table.
   * </ol>
   * 
   * @param catalog The name of the catalog to return tables from,
   *        or "" to return tables from all catalogs.
   * @param schemaPattern A schema pattern for the schemas to return tables
   *        from, or "" to return tables from all schemas.
   * @param namePattern The pattern of table names to return.
   * @param types The list of table types to include; null returns all types.
   * @returns A <code>ResultSet</code> with all the requested tables.
   * @exception SQLException If an error occurs.
   */
  ResultSet getTables(String catalog, String schemaPattern, String
      tableNamePattern, String[] types) throws SQLException;

  /**
   * This method returns the list of database schemas as a 
   * <code>ResultSet</code>, with one column - TABLE_SCHEM - that is the
   * name of the schema.
   *
   * @return A <code>ResultSet</code> with all the requested schemas.
   * @exception SQLException If an error occurs.
   */
  ResultSet getSchemas() throws SQLException;

  /**
   * This method returns the list of database catalogs as a
   * <code>ResultSet</code> with one column - TABLE_CAT - that is the
   * name of the catalog.
   *
   * @return A <code>ResultSet</code> with all the requested catalogs.
   * @exception SQLException If an error occurs.
   */
  ResultSet getCatalogs() throws SQLException;

  /**
   * This method returns the list of database table types as a
   * <code>ResultSet</code> with one column - TABLE_TYPE - that is the
   * name of the table type.
   *
   * @return A <code>ResultSet</code> with all the requested table types.
   * @exception SQLException If an error occurs.
   */
  ResultSet getTableTypes() throws SQLException;

  /**
   * This method returns a list of the tables columns for
   * the requested tables.  This is returned in the form of a
   * <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>TABLE_CAT - The catalog the table is in, which may be 
   * <code>null</code>.
   * <li>TABLE_SCHEM - The schema the tables is in, which may be
   * <code>null</code>.
   * <li>TABLE_NAME - The name of the table.
   * <li>COLUMN_NAME - The name of the column
   * <li>DATA_TYPE - The SQL type of the column. This is one of the constants
   * defined in <code>Types</code>.
   * <li>TYPE_NAME - The string name of the data type for this column.
   * <li>COLUMN_SIZE - The size of the column.
   * <li>Unused
   * <li>NUM_PREC_RADIX - The radix of the column.
   * <li>NULLABLE - Whether or not the column is NULLABLE.  This is one of
   * the constants defined in this class (<code>tableNoNulls</code>,
   * <code>tableNullable</code>, or <code>tableNullableUnknown</code>)
   * <li>REMARKS - A description of the column.
   * <li>COLUMN_DEF - The default value for the column, may be <code>null</code>.
   * <li>SQL_DATA_TYPE - Unused
   * <li>SQL_DATETIME_SUB - Unused
   * <li>CHAR_OCTET_LENGTH - For character columns, the maximum number of bytes
   * in the column.
   * <li>ORDINAL_POSITION - The index of the column in the table.
   * <li>IS_NULLABLE - "NO" means no, "YES" means maybe, and an empty string
   * means unknown.
   * </ol>
   *
   * @param catalog The name of the catalog to return table from,
   * or "" to return tables from all catalogs.
   * @param schemaPattern A schema pattern for the schemas to return 
   * tables from, or "" to return tables from all schemas.
   * @param namePattern The pattern of tables names to return.
   * @param columnPattern The pattern of column names to return.
   * @returns A <code>ResultSet</code> with all the requested tables.
   * @exception SQLException If an error occurs.
   */
  ResultSet getColumns(String catalog, String schemaPattern, String
      tableNamePattern, String columnNamePattern) throws SQLException;

  /**
   * This method returns the access rights that have been granted to the
   * requested columns.  This information is returned as a <code>ResultSet</code>
   * with the following columns:
   * <p>
   * <ol>
   * <li>TABLE_CAT - The catalog the table is in, which may be 
   * <code>null</code>.
   * <li>TABLE_SCHEM - The schema the tables is in, which may be
   * <code>null</code>.
   * <li>TABLE_NAME - The name of the table.
   * <li>COLUMN_NAME - The name of the column.
   * <li>GRANTOR - The entity that granted the access.
   * <li>GRANTEE - The entity granted the access.
   * <li>PRIVILEGE - The name of the privilege granted.
   * <li>IS_GRANTABLE - "YES" if the grantee can grant the privilege to
   * others, "NO" if not, and <code>null</code> if unknown.
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param table The table name to return information for.
   * @param columnPattern A pattern of column names to return information for.
   * @return A <code>ResultSet</code> with all the requested privileges.
   * @exception SQLException If an error occurs.
   */
  ResultSet getColumnPrivileges(String catalog, String schema, String
      table, String columnNamePattern) throws SQLException;

  /**
   * This method returns the access rights that have been granted to the
   * requested tables.  This information is returned as a <code>ResultSet</code>
   * with the following columns:
   * <p>
   * <ol>
   * <li>TABLE_CAT - The catalog the table is in, which may be 
   * <code>null</code>.
   * <li>TABLE_SCHEM - The schema the tables is in, which may be
   * <code>null</code>.
   * <li>TABLE_NAME - The name of the table.
   * <li>GRANTOR - The entity that granted the access.
   * <li>GRANTEE - The entity granted the access.
   * <li>PRIVILEGE - The name of the privilege granted.
   * <li>IS_GRANTABLE - "YES" if the grantee can grant the privilege to
   * others, "NO" if not, and <code>null</code> if unknown.
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param tablePattern The table name pattern of tables to return 
   *        information for.
   * @return A <code>ResultSet</code> with all the requested privileges.
   * @exception SQLException If an error occurs.
   */
  ResultSet getTablePrivileges(String catalog, String schemaPattern,
      String tableNamePattern) throws SQLException;

  /**
   * This method returns the best set of columns for uniquely identifying
   * a row.  It returns this information as a <code>ResultSet</code> with
   * the following columns:
   * <p>
   * <ol>
   * <li>SCOPE - The scope of the results returned.  This is one of the 
   * constants defined in this class (<code>bestRowTemporary</code>,
   * <code>bestRowTransaction</code>, or <code>bestRowSession</code>).
   * <li>COLUMN_NAME - The name of the column.
   * <li>DATA_TYPE - The SQL type of the column. This is one of the constants
   * defined in <code>Types</code>.
   * <li>TYPE_NAME - The string name of the data type for this column.
   * <li>COLUMN_SIZE - The precision of the columns
   * <li>BUFFER_LENGTH - Unused
   * <li>DECIMAL_DIGITS - The scale of the column.
   * <li>PSEUDO_COLUMN - Whether or not the best row identifier is a
   * pseudo_column.  This is one of the constants defined in this class 
   * (<code>bestRowUnknown</code>, <code>bestRowNotPseudo</code>, or
   * <code>bestRowPseudo</code>).
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   * to return entities not associated with a catalog, or <code>null</code>
   * to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   * to return entities not associated with a schema.
   * @param table The table name to return information for.
   * @param columnPattern A pattern of column names to return information for.
   * @param scope One of the best row id scope constants from this class.
   * @param nullable <code>true</code> to include columns that are nullable,
   * <code>false</code> otherwise.
   * @return A <code>ResultSet</code> with the best row identifier.
   * @exception SQLException If an error occurs.
   */
  ResultSet getBestRowIdentifier(String catalog, String schema,
    String table, int scope, boolean nullable) throws SQLException;

  /**
   * This method returns the set of columns that are automatically updated
   * when the row is update. It returns this information as a 
   * <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>SCOPE - Unused
   * <li>COLUMN_NAME - The name of the column.
   * <li>DATA_TYPE - The SQL type of the column. This is one of the constants
   * defined in <code>Types</code>.
   * <li>TYPE_NAME - The string name of the data type for this column.
   * <li>COLUMN_SIZE - The precision of the columns
   * <li>BUFFER_LENGTH - Unused
   * <li>DECIMAL_DIGITS - The scale of the column.
   * <li>PSEUDO_COLUMN - Whether or not the best row identifier is a
   * pseudo_column.  This is one of the constants defined in this class 
   * (<code>versionRowUnknown</code>, <code>versionRowNotPseudo</code>, or
   * <code>versionRowPseudo</code>).
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param table The table name to return information for.
   * @param columnPattern A pattern of column names to return information for.
   * @return A <code>ResultSet</code> with the version columns.
   * @exception SQLException If an error occurs.
   */
  ResultSet getVersionColumns(String catalog, String schema,
    String table) throws SQLException;

  /**
   * This method returns a list of a table's primary key columns.  These
   * are returned as a <code>ResultSet</code> with the following columns.
   * <p>
   * <ol>
   * <li>TABLE_CAT - The catalog of the table, which may be <code>null</code>.
   * <li>TABLE_SCHEM - The schema of the table, which may be <code>null</code>.
   * <li>TABLE_NAME - The name of the table.
   * <li>COLUMN_NAME - The name of the column.
   * <li>KEY_SEQ - The sequence number of the column within the primary key.
   * <li>PK_NAME - The name of the primary key, which may be <code>null</code>.
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param table The table name to return information for.
   * @param columnPattern A pattern of column names to return information for.
   * @return A <code>ResultSet</code> with the primary key columns.
   * @exception SQLException If an error occurs.
   */
  ResultSet getPrimaryKeys(String catalog, String schema, String table)
      throws SQLException;

  /**
   * This method returns a list of the table's foreign keys.  These are
   * returned as a <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>PKTABLE_CAT - The catalog of the table the key was imported from.
   * <li>PKTABLE_SCHEM - The schema of the table the key was imported from.
   * <li>PKTABLE_NAME - The name of the table the key was imported from.
   * <li>PKCOLUMN_NAME - The name of the column that was imported.
   * <li>FKTABLE_CAT - The foreign key catalog name.
   * <li>FKTABLE_SCHEM - The foreign key schema name.
   * <li>FKTABLE_NAME - The foreign key table name.
   * <li>FKCOLUMN_NAME - The foreign key column name.
   * <li>KEY_SEQ - The sequence number of the column within the foreign key.
   * <li>UPDATE_RULE - How the foreign key behaves when the primary key is
   * updated.  This is one of the constants defined in this class 
   * (<code>importedNoAction</code>, <code>importedKeyCascade</code>,
   * <code>importedKeySetNull</code>, <code>importedKeySetDefault</code>, or
   * <code>importedKeyRestrict</code>).
   * <li>DELETE_RULE - How the foreign key behaves when the primary key is
   * deleted.  This is one of the constants defined in this class 
   * (<code>importedNoAction</code>, <code>importedKeyCascade</code>,
   * <code>importedKeySetNull</code>, or <code>importedKeySetDefault</code>)
   * <li>FK_NAME - The name of the foreign key.
   * <li>PK_NAME - The name of the primary key.
   * <li>DEFERRABILITY - The deferrability value.  This is one of the
   * constants defined in this table (<code>importedKeyInitiallyDeferred</code>,
   * <code>importedKeyInitiallyImmediate</code>, or
   * <code>importedKeyNotDeferrable</code>).
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param table The table name to return information for.
   *
   * @return A <code>ResultSet</code> with the foreign key columns.
   *
   * @exception SQLException If an error occurs.
   */
  ResultSet getImportedKeys(String catalog, String schema,
    String table) throws SQLException;

  /**
   * This method returns a list of the table's which use this table's
   * primary key as a foreign key.  The information is
   * returned as a <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>PKTABLE_CAT - The catalog of the table the key was imported from.
   * <li>PKTABLE_SCHEM - The schema of the table the key was imported from.
   * <li>PKTABLE_NAME - The name of the table the key was imported from.
   * <li>PKCOLUMN_NAME - The name of the column that was imported.
   * <li>FKTABLE_CAT - The foreign key catalog name.
   * <li>FKTABLE_SCHEM - The foreign key schema name.
   * <li>FKTABLE_NAME - The foreign key table name.
   * <li>FKCOLUMN_NAME - The foreign key column name.
   * <li>KEY_SEQ - The sequence number of the column within the foreign key.
   * <li>UPDATE_RULE - How the foreign key behaves when the primary key is
   * updated.  This is one of the constants defined in this class 
   * (<code>importedNoAction</code>, <code>importedKeyCascade</code>,
   * <code>importedKeySetNull</code>, <code>importedKeySetDefault</code>, or
   * <code>importedKeyRestrict</code>).
   * <li>DELETE_RULE - How the foreign key behaves when the primary key is
   * deleted.  This is one of the constants defined in this class 
   * (<code>importedNoAction</code>, <code>importedKeyCascade</code>,
   * <code>importedKeySetNull</code>, or <code>importedKeySetDefault</code>)
   * <li>FK_NAME - The name of the foreign key.
   * <li>PK_NAME - The name of the primary key.
   * <li>DEFERRABILITY - The deferrability value.  This is one of the
   * constants defined in this table (<code>importedKeyInitiallyDeferred</code>,
   * <code>importedKeyInitiallyImmediate</code>, or
   * <code>importedKeyNotDeferrable</code>).
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param table The table name to return information for.
   * @return A <code>ResultSet</code> with the requested information
   * @exception SQLException If an error occurs.
   */
  ResultSet getExportedKeys(String catalog, String schema,
    String table) throws SQLException;

  /**
   * This method returns a description of how one table imports another
   * table's primary key as a foreign key.  The information is
   * returned as a <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>PKTABLE_CAT - The catalog of the table the key was imported from.
   * <li>PKTABLE_SCHEM - The schema of the table the key was imported from.
   * <li>PKTABLE_NAME - The name of the table the key was imported from.
   * <li>PKCOLUMN_NAME - The name of the column that was imported.
   * <li>FKTABLE_CAT - The foreign key catalog name.
   * <li>FKTABLE_SCHEM - The foreign key schema name.
   * <li>FKTABLE_NAME - The foreign key table name.
   * <li>FKCOLUMN_NAME - The foreign key column name.
   * <li>KEY_SEQ - The sequence number of the column within the foreign key.
   * <li>UPDATE_RULE - How the foreign key behaves when the primary key is
   * updated.  This is one of the constants defined in this class 
   * (<code>importedNoAction</code>, <code>importedKeyCascade</code>,
   * <code>importedKeySetNull</code>, <code>importedKeySetDefault</code>, or
   * <code>importedKeyRestrict</code>).
   * <li>DELETE_RULE - How the foreign key behaves when the primary key is
   * deleted.  This is one of the constants defined in this class 
   * (<code>importedNoAction</code>, <code>importedKeyCascade</code>,
   * <code>importedKeySetNull</code>, or <code>importedKeySetDefault</code>)
   * <li>FK_NAME - The name of the foreign key.
   * <li>PK_NAME - The name of the primary key.
   * <li>DEFERRABILITY - The deferrability value.  This is one of the
   * constants defined in this table (<code>importedKeyInitiallyDeferred</code>,
   * <code>importedKeyInitiallyImmediate</code>, or
   * <code>importedKeyNotDeferrable</code>).
   * </ol>
   *
   * @param primCatalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs, on the exporting side.
   * @param primSchema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema, on the exporting side.
   * @param primTable The table name to return information for, on the exporting
   *        side.
   * @param forCatalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs, on the importing side.
   * @param forSchema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema on the importing side.
   * @param forTable The table name to return information for on the importing
   *        side.
   * @return A <code>ResultSet</code> with the requested information
   * @exception SQLException If an error occurs.
   */
  ResultSet getCrossReference(String primaryCatalog, String
    primarySchema, String primaryTable, String foreignCatalog, String
    foreignSchema, String foreignTable) throws SQLException;

  /**
   * This method returns a list of the SQL types supported by this
   * database.  The information is returned as a <code>ResultSet</code>
   * with the following columns:
   * <p>
   * <ol>
   * <li>TYPE_NAME - The name of the data type.
   * <li>DATA_TYPE - A data type constant from <code>Types</code> for this
   * type.
   * <li>PRECISION - The maximum precision of this type.
   * <li>LITERAL_PREFIX - Prefix value used to quote a literal, which may be
   * <code>null</code>.
   * <li>LITERAL_SUFFIX - Suffix value used to quote a literal, which may be
   * <code>null</code>.
   * <li>CREATE_PARAMS - The parameters used to create the type, which may be
   * <code>null</code>.
   * <li>NULLABLE - Whether or not this type supports NULL values.  This will
   * be one of the constants defined in this interface 
   * (<code>typeNoNulls</code>, <code>typeNullable</code>, or
   * <code>typeNullableUnknown</code>).
   * <li>CASE_SENSITIVE - Whether or not the value is case sensitive.
   * <li>SEARCHABLE - Whether or not "LIKE" expressions are supported in
   * WHERE clauses for this type.  This will be one of the constants defined
   * in this interface (<code>typePredNone</code>, <code>typePredChar</code>,
   * <code>typePredBasic</code>, or <code>typeSearchable</code>).
   * <li>UNSIGNED_ATTRIBUTE - Is the value of this type unsigned.
   * <li>FIXED_PREC_SCALE - Whether or not this type can be used for money.
   * <li>AUTO_INCREMENT - Whether or not this type supports auto-incrementing.
   * <li>LOCAL_TYPE_NAME - A localized name for this data type.
   * <li>MINIMUM_SCALE - The minimum scale supported by this type.
   * <li>MAXIMUM_SCALE - The maximum scale supported by this type.
   * <li>SQL_DATA_TYPE - Unused.
   * <li>SQL_DATETIME_SUB - Unused.
   * <li>NUM_PREC_RADIX - The radix of this data type.
   * </ol>
   * 
   * @return A <code>ResultSet</code> with the list of available data types.
   * @exception SQLException If an error occurs.
   */
  ResultSet getTypeInfo() throws SQLException;

  /**
   * This method returns information about a tables indices and statistics.
   * It is returned as a <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>TABLE_CAT - The catalog of the table, which may be <code>null</code>.
   * <li>TABLE_SCHEM - The schema of the table, which may be <code>null</code>.
   * <li>TABLE_NAME - The name of the table.
   * <li>NON_UNIQUE - Are index values non-unique?
   * <li>INDEX_QUALIFIER The index catalog, which may be <code>null</code>
   * <li>INDEX_NAME - The name of the index.
   * <li>TYPE - The type of index, which will be one of the constants defined
   * in this interface (<code>tableIndexStatistic</code>,
   * <code>tableIndexClustered</code>, <code>tableIndexHashed</code>, or
   * <code>tableIndexOther</code>).
   * <li>ORDINAL_POSITION - The sequence number of this column in the index.
   * This will be 0 when the index type is <code>tableIndexStatistic</code>.
   * <li>COLUMN_NAME - The name of this column in the index.
   * <li>ASC_OR_DESC - "A" for an ascending sort sequence, "D" for a
   * descending sort sequence or <code>null</code> if a sort sequence is not
   * supported.
   * <li>CARDINALITY - The number of unique rows in the index, or the number
   * of rows in the table if the index type is <code>tableIndexStatistic</code>.
   * <li>PAGES - The number of pages used for the index, or the number of pages
   * in the table if the index type is <code>tableIndexStatistic</code>.
   * <li>FILTER_CONDITION - The filter condition for this index, which may be
   * <code>null</code>.
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or 
   *        <code>null</code> to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param table The table name to return information for.
   * @param unique <code>true</code> to return only unique indexes, 
   *        <code>false</code> otherwise.
   * @param approx <code>true</code> if data values can be approximations,
   *        <code>false</code> otherwise.
   * @return A <code>ResultSet</code> with the requested index information
   * @exception SQLException If an error occurs.
   */
  ResultSet getIndexInfo(String catalog, String schema, String table,
    boolean unique, boolean approximate) throws SQLException;

  /**
   * This method tests whether or not the datbase supports the specified
   * result type.
   *
   * @param type The desired result type, which is one of the constants
   * defined in <code>ResultSet</code>.
   *
   * @return <code>true</code> if the result set type is supported,
   * <code>false</code> otherwise.
   *
   * @exception SQLException If an error occurs.
   *
   * @see ResultSet
   */
  boolean supportsResultSetType(int type) throws SQLException;

  /**
   * This method tests whether the specified result set type and result set
   * concurrency type are supported by the database.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @param concur The desired concurrency type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type is supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean supportsResultSetConcurrency(int type, int concurrency)
      throws SQLException;

  /**
   * This method tests whether or not the specified result set type sees its
   * own updates.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type sees its own updates,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean ownUpdatesAreVisible(int type) throws SQLException;

 /**
   * This method tests whether or not the specified result set type sees its
   * own deletes.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type sees its own deletes,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean ownDeletesAreVisible(int type) throws SQLException;

  /**
   * This method tests whether or not the specified result set type sees its
   * own inserts.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type sees its own inserts,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean ownInsertsAreVisible(int type) throws SQLException;

  /**
   * This method tests whether or not the specified result set type sees 
   * updates committed by others.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type sees other updates,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean othersUpdatesAreVisible(int type) throws SQLException;

  /**
   * This method tests whether or not the specified result set type sees 
   * deletes committed by others.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type sees other deletes,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean othersDeletesAreVisible(int type) throws SQLException;

  /**
   * This method tests whether or not the specified result set type sees 
   * inserts committed by others.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type sees other inserts,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean othersInsertsAreVisible(int type) throws SQLException;

  /**
   * This method tests whether or not the specified result set type can detect
   * a visible update by calling the <code>rowUpdated</code> method.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type can detect visible updates
   *         using <code>rowUpdated</code>, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean updatesAreDetected(int type) throws SQLException;

  /**
   * This method tests whether or not the specified result set type can detect
   * a visible delete by calling the <code>rowUpdated</code> method.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type can detect visible deletes
   *         using <code>rowUpdated</code>, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean deletesAreDetected(int type) throws SQLException;
  
  /**
   * This method tests whether or not the specified result set type can detect
   * a visible insert by calling the <code>rowUpdated</code> method.
   *
   * @param type The desired result type, which is one of the constants
   *        defined in <code>ResultSet</code>.
   * @return <code>true</code> if the result set type can detect visible inserts
   *         using <code>rowUpdated</code>, <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   * @see ResultSet
   */
  boolean insertsAreDetected(int type) throws SQLException;

  /**
   * This method tests whether or not the database supports batch updates.
   *
   * @return <code>true</code> if batch updates are supported,
   *         <code>false</code> otherwise.
   * @exception SQLException If an error occurs.
   */
  boolean supportsBatchUpdates() throws SQLException;

  /**
   * This method returns the list of user defined data types in use.  These
   * are returned as a <code>ResultSet</code> with the following columns:
   * <p>
   * <ol>
   * <li>TYPE_CAT - The catalog name, which may be <code>null</code>.
   * <li>TYPE_SCEHM - The schema name, which may be <code>null</code>.
   * <li>TYPE_NAME - The user defined data type name.
   * <li>CLASS_NAME - The Java class name this type maps to.
   * <li>DATA_TYPE - A type identifier from <code>Types</code> for this type.
   * This will be one of <code>JAVA_OBJECT</code>, <code>STRUCT</code>, or
   * <code>DISTINCT</code>.
   * <li>REMARKS - Comments about this data type.
   * </ol>
   *
   * @param catalog The catalog to retrieve information from, or the empty string
   *        to return entities not associated with a catalog, or <code>null</code>
   *        to return information from all catalogs.
   * @param schema The schema to retrieve information from, or the empty string
   *        to return entities not associated with a schema.
   * @param typePattern The type name pattern to match.
   * @param types The type identifier patterns (from <code>Types</code>) to
   *        match.
   * @return A <code>ResultSet</code> with the requested type information
   * @exception SQLException If an error occurs.
   */
  ResultSet getUDTs(String catalog, String schemaPattern, String
      typeNamePattern, int[] types) throws SQLException;

  /**
   * This method returns the <code>Connection</code> object that was used
   * to generate the metadata in this object.
   *
   * @return The connection for this object.
   * @exception SQLException If an error occurs.
   */
  Connection getConnection() throws SQLException;

  /**
   * @since 1.4
   */
  boolean supportsSavepoints() throws SQLException;

  /**
   * @since 1.4
   */
  boolean supportsNamedParameters() throws SQLException;

  /**
   * @since 1.4
   */
  boolean supportsMultipleOpenResults() throws SQLException;

  /**
   * @since 1.4
   */
  boolean supportsGetGeneratedKeys() throws SQLException;

  /**
   * @since 1.4
   */
  ResultSet getSuperTypes(String catalog, String schemaPattern,
    String typeNamePattern) throws SQLException;

  /**
   * @since 1.4
   */
  ResultSet getSuperTables(String catalog, String schemaPattern,
    String tableNamePattern) throws SQLException;

  /**
   * @since 1.4
   */
  ResultSet getAttributes(String catalog, String schemaPattern, String
    typeNamePattern, String attributeNamePattern) throws SQLException;

  /**
   * @since 1.4
   */
  boolean supportsResultSetHoldability(int holdability)
    throws SQLException;

  /**
   * @since 1.4
   */
  int getResultSetHoldability() throws SQLException;

  /**
   * @since 1.4
   */
  int getDatabaseMajorVersion() throws SQLException;

  /**
   * @since 1.4
   */
  int getDatabaseMinorVersion() throws SQLException;

  /**
   * @since 1.4
   */
  int getJDBCMajorVersion() throws SQLException;

  /**
   * @since 1.4
   */
  int getJDBCMinorVersion() throws SQLException;

  /**
   * @since 1.4
   */
  int getSQLStateType() throws SQLException;

  /**
   * @since 1.4
   */
  boolean locatorsUpdateCopy() throws SQLException;

  /**
   * @since 1.4
   */
  boolean supportsStatementPooling() throws SQLException;
}
