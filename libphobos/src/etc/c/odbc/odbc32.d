module etc.c.odbc.odbc32;
public:

extern (C)
{
	alias SQLCHAR = ubyte;
	alias SQLSCHAR = byte;
	alias SQLDATE = ubyte;
	alias SQLDECIMAL = ubyte;
	alias SQLDOUBLE = double;
	alias SQLFLOAT = double;
	alias SQLINTEGER = int;
	alias SQLUINTEGER = uint;
	alias SQLNUMERIC = ubyte;
	alias SQLPOINTER = void*;
	alias SQLREAL = float;
	alias SQLSMALLINT = short;
	alias SQLUSMALLINT = ushort;
	alias SQLTIME = ubyte;
	alias SQLTIMESTAMP = ubyte;
	alias SQLVARCHAR = ubyte;
	alias SQLTIMEWITHTIMEZONE = ubyte;
	alias SQLTIMESTAMPWITHTIMEZONE = ubyte;
	alias SQLRETURN = short;
	alias SQLHANDLE = int;
	alias SQLHENV = int;
	alias SQLHDBC = int;
	alias SQLHSTMT = int;
	alias SQLHDESC = int;
	alias RETCODE = short;
	alias SQLHWND = void*;
	struct tagDATE_STRUCT
	{
		SQLSMALLINT year = void;
		SQLUSMALLINT month = void;
		SQLUSMALLINT day = void;
	}
	alias DATE_STRUCT = tagDATE_STRUCT;
	alias SQL_DATE_STRUCT = tagDATE_STRUCT;
	struct tagTIME_STRUCT
	{
		SQLUSMALLINT hour = void;
		SQLUSMALLINT minute = void;
		SQLUSMALLINT second = void;
	}
	alias TIME_STRUCT = tagTIME_STRUCT;
	alias SQL_TIME_STRUCT = tagTIME_STRUCT;
	struct tagTIMESTAMP_STRUCT
	{
		SQLSMALLINT year = void;
		SQLUSMALLINT month = void;
		SQLUSMALLINT day = void;
		SQLUSMALLINT hour = void;
		SQLUSMALLINT minute = void;
		SQLUSMALLINT second = void;
		SQLUINTEGER fraction = void;
	}
	alias TIMESTAMP_STRUCT = tagTIMESTAMP_STRUCT;
	alias SQL_TIMESTAMP_STRUCT = tagTIMESTAMP_STRUCT;
	struct tagTIME_WITH_TIMEZONE_STRUCT
	{
		SQLUSMALLINT hour = void;
		SQLUSMALLINT minute = void;
		SQLUSMALLINT second = void;
		SQLSMALLINT timezone_hours = void;
		SQLUSMALLINT timezone_minutes = void;
	}
	alias TIME_WITH_TIMEZONE_STRUCT = tagTIME_WITH_TIMEZONE_STRUCT;
	alias SQL_TIME_WITH_TIMEZONE_STRUCT = tagTIME_WITH_TIMEZONE_STRUCT;
	struct tagTIMESTAMP_WITH_TIMEZONE_STRUCT
	{
		SQLSMALLINT year = void;
		SQLUSMALLINT month = void;
		SQLUSMALLINT day = void;
		SQLUSMALLINT hour = void;
		SQLUSMALLINT minute = void;
		SQLUSMALLINT second = void;
		SQLUINTEGER fraction = void;
		SQLSMALLINT timezone_hours = void;
		SQLUSMALLINT timezone_minutes = void;
	}
	alias TIMESTAMP_WITH_TIMEZONE_STRUCT = tagTIMESTAMP_WITH_TIMEZONE_STRUCT;
	alias SQL_TIMESTAMP_WITH_TIMEZONE_STRUCT = tagTIMESTAMP_WITH_TIMEZONE_STRUCT;
	enum SQLINTERVAL
	{
		SQL_IS_YEAR = 1,
		SQL_IS_MONTH = 2,
		SQL_IS_DAY = 3,
		SQL_IS_HOUR = 4,
		SQL_IS_MINUTE = 5,
		SQL_IS_SECOND = 6,
		SQL_IS_YEAR_TO_MONTH = 7,
		SQL_IS_DAY_TO_HOUR = 8,
		SQL_IS_DAY_TO_MINUTE = 9,
		SQL_IS_DAY_TO_SECOND = 10,
		SQL_IS_HOUR_TO_MINUTE = 11,
		SQL_IS_HOUR_TO_SECOND = 12,
		SQL_IS_MINUTE_TO_SECOND = 13,
	}
	alias SQL_IS_YEAR = SQLINTERVAL.SQL_IS_YEAR;
	alias SQL_IS_MONTH = SQLINTERVAL.SQL_IS_MONTH;
	alias SQL_IS_DAY = SQLINTERVAL.SQL_IS_DAY;
	alias SQL_IS_HOUR = SQLINTERVAL.SQL_IS_HOUR;
	alias SQL_IS_MINUTE = SQLINTERVAL.SQL_IS_MINUTE;
	alias SQL_IS_SECOND = SQLINTERVAL.SQL_IS_SECOND;
	alias SQL_IS_YEAR_TO_MONTH = SQLINTERVAL.SQL_IS_YEAR_TO_MONTH;
	alias SQL_IS_DAY_TO_HOUR = SQLINTERVAL.SQL_IS_DAY_TO_HOUR;
	alias SQL_IS_DAY_TO_MINUTE = SQLINTERVAL.SQL_IS_DAY_TO_MINUTE;
	alias SQL_IS_DAY_TO_SECOND = SQLINTERVAL.SQL_IS_DAY_TO_SECOND;
	alias SQL_IS_HOUR_TO_MINUTE = SQLINTERVAL.SQL_IS_HOUR_TO_MINUTE;
	alias SQL_IS_HOUR_TO_SECOND = SQLINTERVAL.SQL_IS_HOUR_TO_SECOND;
	alias SQL_IS_MINUTE_TO_SECOND = SQLINTERVAL.SQL_IS_MINUTE_TO_SECOND;
	struct tagSQL_YEAR_MONTH
	{
		SQLUINTEGER year = void;
		SQLUINTEGER month = void;
	}
	alias SQL_YEAR_MONTH_STRUCT = tagSQL_YEAR_MONTH;
	struct tagSQL_DAY_SECOND
	{
		SQLUINTEGER day = void;
		SQLUINTEGER hour = void;
		SQLUINTEGER minute = void;
		SQLUINTEGER second = void;
		SQLUINTEGER fraction = void;
	}
	alias SQL_DAY_SECOND_STRUCT = tagSQL_DAY_SECOND;
	struct tagSQL_INTERVAL_STRUCT
	{
		SQLINTERVAL interval_type = void;
		SQLSMALLINT interval_sign = void;
		union intval { SQL_YEAR_MONTH_STRUCT year_month = void; SQL_DAY_SECOND_STRUCT day_second = void; }
	}
	alias SQL_INTERVAL_STRUCT = tagSQL_INTERVAL_STRUCT;
	alias SQLBIGINT = long;
	alias SQLUBIGINT = ulong;
	struct tagSQL_NUMERIC_STRUCT
	{
		SQLCHAR precision = void;
		SQLSCHAR scale = void;
		SQLCHAR sign = void;
		SQLCHAR[SQL_MAX_NUMERIC_LEN] val = void;
	}
	alias SQL_NUMERIC_STRUCT = tagSQL_NUMERIC_STRUCT;
	struct tagSQLGUID
	{
		SQLUINTEGER Data1 = void;
		SQLUSMALLINT Data2 = void;
		SQLUSMALLINT Data3 = void;
		SQLCHAR[8] Data4 = void;
	}
	alias SQLGUID = tagSQLGUID;
	alias BOOKMARK = uint;
	alias SQLWCHAR = ushort;
	alias SQLTCHAR = ubyte;
	nothrow @nogc SQLRETURN SQLAllocConnect(SQLHENV EnvironmentHandle, SQLHDBC* ConnectionHandle);
	nothrow @nogc SQLRETURN SQLAllocEnv(SQLHENV* EnvironmentHandle);
	nothrow @nogc SQLRETURN SQLAllocHandle(SQLSMALLINT HandleType, SQLHANDLE InputHandle, SQLHANDLE* OutputHandle);
	nothrow @nogc SQLRETURN SQLAllocStmt(SQLHDBC ConnectionHandle, SQLHSTMT* StatementHandle);
	nothrow @nogc SQLRETURN SQLBindCol(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType, SQLPOINTER TargetValue, SQLINTEGER BufferLength, SQLINTEGER* StrLen_or_Ind);
	deprecated("ODBC API: SQLBindParam is deprecated. Please use SQLBindParameter instead.") deprecated nothrow @nogc SQLRETURN SQLBindParam(SQLHSTMT StatementHandle, SQLUSMALLINT ParameterNumber, SQLSMALLINT ValueType, SQLSMALLINT ParameterType, SQLUINTEGER LengthPrecision, SQLSMALLINT ParameterScale, SQLPOINTER ParameterValue, SQLINTEGER* StrLen_or_Ind);
	nothrow @nogc SQLRETURN SQLCancel(SQLHSTMT StatementHandle);
	nothrow @nogc SQLRETURN SQLCancelHandle(SQLSMALLINT HandleType, SQLHANDLE InputHandle);
	nothrow @nogc SQLRETURN SQLCloseCursor(SQLHSTMT StatementHandle);
	nothrow @nogc SQLRETURN SQLColAttribute(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber, SQLUSMALLINT FieldIdentifier, SQLPOINTER CharacterAttribute, SQLSMALLINT BufferLength, SQLSMALLINT* StringLength, SQLPOINTER NumericAttribute);
	nothrow @nogc SQLRETURN SQLColumns(SQLHSTMT StatementHandle, SQLCHAR* CatalogName, SQLSMALLINT NameLength1, SQLCHAR* SchemaName, SQLSMALLINT NameLength2, SQLCHAR* TableName, SQLSMALLINT NameLength3, SQLCHAR* ColumnName, SQLSMALLINT NameLength4);
	nothrow @nogc SQLRETURN SQLCompleteAsync(SQLSMALLINT HandleType, SQLHANDLE Handle, RETCODE* AsyncRetCodePtr);
	nothrow @nogc SQLRETURN SQLConnect(SQLHDBC ConnectionHandle, SQLCHAR* ServerName, SQLSMALLINT NameLength1, SQLCHAR* UserName, SQLSMALLINT NameLength2, SQLCHAR* Authentication, SQLSMALLINT NameLength3);
	nothrow @nogc SQLRETURN SQLCopyDesc(SQLHDESC SourceDescHandle, SQLHDESC TargetDescHandle);
	nothrow @nogc SQLRETURN SQLDataSources(SQLHENV EnvironmentHandle, SQLUSMALLINT Direction, SQLCHAR* ServerName, SQLSMALLINT BufferLength1, SQLSMALLINT* NameLength1Ptr, SQLCHAR* Description, SQLSMALLINT BufferLength2, SQLSMALLINT* NameLength2Ptr);
	nothrow @nogc SQLRETURN SQLDescribeCol(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber, SQLCHAR* ColumnName, SQLSMALLINT BufferLength, SQLSMALLINT* NameLength, SQLSMALLINT* DataType, SQLUINTEGER* ColumnSize, SQLSMALLINT* DecimalDigits, SQLSMALLINT* Nullable);
	nothrow @nogc SQLRETURN SQLDisconnect(SQLHDBC ConnectionHandle);
	nothrow @nogc SQLRETURN SQLEndTran(SQLSMALLINT HandleType, SQLHANDLE Handle, SQLSMALLINT CompletionType);
	nothrow @nogc SQLRETURN SQLError(SQLHENV EnvironmentHandle, SQLHDBC ConnectionHandle, SQLHSTMT StatementHandle, SQLCHAR* Sqlstate, SQLINTEGER* NativeError, SQLCHAR* MessageText, SQLSMALLINT BufferLength, SQLSMALLINT* TextLength);
	nothrow @nogc SQLRETURN SQLExecDirect(SQLHSTMT StatementHandle, SQLCHAR* StatementText, SQLINTEGER TextLength);
	nothrow @nogc SQLRETURN SQLExecute(SQLHSTMT StatementHandle);
	nothrow @nogc SQLRETURN SQLFetch(SQLHSTMT StatementHandle);
	nothrow @nogc SQLRETURN SQLFetchScroll(SQLHSTMT StatementHandle, SQLSMALLINT FetchOrientation, SQLINTEGER FetchOffset);
	nothrow @nogc SQLRETURN SQLFreeConnect(SQLHDBC ConnectionHandle);
	nothrow @nogc SQLRETURN SQLFreeEnv(SQLHENV EnvironmentHandle);
	nothrow @nogc SQLRETURN SQLFreeHandle(SQLSMALLINT HandleType, SQLHANDLE Handle);
	nothrow @nogc SQLRETURN SQLFreeStmt(SQLHSTMT StatementHandle, SQLUSMALLINT Option);
	nothrow @nogc SQLRETURN SQLGetConnectAttr(SQLHDBC ConnectionHandle, SQLINTEGER Attribute, SQLPOINTER Value, SQLINTEGER BufferLength, SQLINTEGER* StringLengthPtr);
	deprecated("ODBC API: SQLGetConnectOption is deprecated. Please use SQLGetConnectAttr instead.") deprecated nothrow @nogc SQLRETURN SQLGetConnectOption(SQLHDBC ConnectionHandle, SQLUSMALLINT Option, SQLPOINTER Value);
	nothrow @nogc SQLRETURN SQLGetCursorName(SQLHSTMT StatementHandle, SQLCHAR* CursorName, SQLSMALLINT BufferLength, SQLSMALLINT* NameLengthPtr);
	nothrow @nogc SQLRETURN SQLGetData(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType, SQLPOINTER TargetValue, SQLINTEGER BufferLength, SQLINTEGER* StrLen_or_IndPtr);
	nothrow @nogc SQLRETURN SQLGetDescField(SQLHDESC DescriptorHandle, SQLSMALLINT RecNumber, SQLSMALLINT FieldIdentifier, SQLPOINTER Value, SQLINTEGER BufferLength, SQLINTEGER* StringLength);
	nothrow @nogc SQLRETURN SQLGetDescRec(SQLHDESC DescriptorHandle, SQLSMALLINT RecNumber, SQLCHAR* Name, SQLSMALLINT BufferLength, SQLSMALLINT* StringLengthPtr, SQLSMALLINT* TypePtr, SQLSMALLINT* SubTypePtr, SQLINTEGER* LengthPtr, SQLSMALLINT* PrecisionPtr, SQLSMALLINT* ScalePtr, SQLSMALLINT* NullablePtr);
	nothrow @nogc SQLRETURN SQLGetDiagField(SQLSMALLINT HandleType, SQLHANDLE Handle, SQLSMALLINT RecNumber, SQLSMALLINT DiagIdentifier, SQLPOINTER DiagInfo, SQLSMALLINT BufferLength, SQLSMALLINT* StringLength);
	nothrow @nogc SQLRETURN SQLGetDiagRec(SQLSMALLINT HandleType, SQLHANDLE Handle, SQLSMALLINT RecNumber, SQLCHAR* Sqlstate, SQLINTEGER* NativeError, SQLCHAR* MessageText, SQLSMALLINT BufferLength, SQLSMALLINT* TextLength);
	nothrow @nogc SQLRETURN SQLGetEnvAttr(SQLHENV EnvironmentHandle, SQLINTEGER Attribute, SQLPOINTER Value, SQLINTEGER BufferLength, SQLINTEGER* StringLength);
	nothrow @nogc SQLRETURN SQLGetFunctions(SQLHDBC ConnectionHandle, SQLUSMALLINT FunctionId, SQLUSMALLINT* Supported);
	nothrow @nogc SQLRETURN SQLGetInfo(SQLHDBC ConnectionHandle, SQLUSMALLINT InfoType, SQLPOINTER InfoValue, SQLSMALLINT BufferLength, SQLSMALLINT* StringLengthPtr);
	nothrow @nogc SQLRETURN SQLGetStmtAttr(SQLHSTMT StatementHandle, SQLINTEGER Attribute, SQLPOINTER Value, SQLINTEGER BufferLength, SQLINTEGER* StringLength);
	deprecated("ODBC API: SQLGetStmtOption is deprecated. Please use SQLGetStmtAttr instead.") deprecated nothrow @nogc SQLRETURN SQLGetStmtOption(SQLHSTMT StatementHandle, SQLUSMALLINT Option, SQLPOINTER Value);
	nothrow @nogc SQLRETURN SQLGetTypeInfo(SQLHSTMT StatementHandle, SQLSMALLINT DataType);
	nothrow @nogc SQLRETURN SQLNumResultCols(SQLHSTMT StatementHandle, SQLSMALLINT* ColumnCount);
	nothrow @nogc SQLRETURN SQLParamData(SQLHSTMT StatementHandle, SQLPOINTER* Value);
	nothrow @nogc SQLRETURN SQLPrepare(SQLHSTMT StatementHandle, SQLCHAR* StatementText, SQLINTEGER TextLength);
	nothrow @nogc SQLRETURN SQLPutData(SQLHSTMT StatementHandle, SQLPOINTER Data, SQLINTEGER StrLen_or_Ind);
	nothrow @nogc SQLRETURN SQLRowCount(SQLHSTMT StatementHandle, SQLINTEGER* RowCount);
	nothrow @nogc SQLRETURN SQLSetConnectAttr(SQLHDBC ConnectionHandle, SQLINTEGER Attribute, SQLPOINTER Value, SQLINTEGER StringLength);
	deprecated("ODBC API: SQLSetConnectOption is deprecated. Please use SQLSetConnectAttr instead.") deprecated nothrow @nogc SQLRETURN SQLSetConnectOption(SQLHDBC ConnectionHandle, SQLUSMALLINT Option, SQLUINTEGER Value);
	nothrow @nogc SQLRETURN SQLSetCursorName(SQLHSTMT StatementHandle, SQLCHAR* CursorName, SQLSMALLINT NameLength);
	nothrow @nogc SQLRETURN SQLSetDescField(SQLHDESC DescriptorHandle, SQLSMALLINT RecNumber, SQLSMALLINT FieldIdentifier, SQLPOINTER Value, SQLINTEGER BufferLength);
	nothrow @nogc SQLRETURN SQLSetDescRec(SQLHDESC DescriptorHandle, SQLSMALLINT RecNumber, SQLSMALLINT Type, SQLSMALLINT SubType, SQLINTEGER Length, SQLSMALLINT Precision, SQLSMALLINT Scale, SQLPOINTER Data, SQLINTEGER* StringLength, SQLINTEGER* Indicator);
	nothrow @nogc SQLRETURN SQLSetEnvAttr(SQLHENV EnvironmentHandle, SQLINTEGER Attribute, SQLPOINTER Value, SQLINTEGER StringLength);
	deprecated("ODBC API: SQLSetParam is deprecated. Please use SQLBindParameter instead.") deprecated nothrow @nogc SQLRETURN SQLSetParam(SQLHSTMT StatementHandle, SQLUSMALLINT ParameterNumber, SQLSMALLINT ValueType, SQLSMALLINT ParameterType, SQLUINTEGER LengthPrecision, SQLSMALLINT ParameterScale, SQLPOINTER ParameterValue, SQLINTEGER* StrLen_or_Ind);
	nothrow @nogc SQLRETURN SQLSetStmtAttr(SQLHSTMT StatementHandle, SQLINTEGER Attribute, SQLPOINTER Value, SQLINTEGER StringLength);
	deprecated("ODBC API: SQLSetStmtOption is deprecated. Please use SQLSetStmtAttr instead.") deprecated nothrow @nogc SQLRETURN SQLSetStmtOption(SQLHSTMT StatementHandle, SQLUSMALLINT Option, SQLUINTEGER Value);
	nothrow @nogc SQLRETURN SQLSpecialColumns(SQLHSTMT StatementHandle, SQLUSMALLINT IdentifierType, SQLCHAR* CatalogName, SQLSMALLINT NameLength1, SQLCHAR* SchemaName, SQLSMALLINT NameLength2, SQLCHAR* TableName, SQLSMALLINT NameLength3, SQLUSMALLINT Scope, SQLUSMALLINT Nullable);
	nothrow @nogc SQLRETURN SQLStatistics(SQLHSTMT StatementHandle, SQLCHAR* CatalogName, SQLSMALLINT NameLength1, SQLCHAR* SchemaName, SQLSMALLINT NameLength2, SQLCHAR* TableName, SQLSMALLINT NameLength3, SQLUSMALLINT Unique, SQLUSMALLINT Reserved);
	nothrow @nogc SQLRETURN SQLTables(SQLHSTMT StatementHandle, SQLCHAR* CatalogName, SQLSMALLINT NameLength1, SQLCHAR* SchemaName, SQLSMALLINT NameLength2, SQLCHAR* TableName, SQLSMALLINT NameLength3, SQLCHAR* TableType, SQLSMALLINT NameLength4);
	nothrow @nogc SQLRETURN SQLTransact(SQLHENV EnvironmentHandle, SQLHDBC ConnectionHandle, SQLUSMALLINT CompletionType);
	alias SQLSTATE = ubyte[SQL_SQLSTATE_SIZE + 1];
	nothrow @nogc SQLRETURN SQLDriverConnect(SQLHDBC hdbc, SQLHWND hwnd, SQLCHAR* szConnStrIn, SQLSMALLINT cchConnStrIn, SQLCHAR* szConnStrOut, SQLSMALLINT cchConnStrOutMax, SQLSMALLINT* pcchConnStrOut, SQLUSMALLINT fDriverCompletion);
	nothrow @nogc SQLRETURN SQLBrowseConnect(SQLHDBC hdbc, SQLCHAR* szConnStrIn, SQLSMALLINT cchConnStrIn, SQLCHAR* szConnStrOut, SQLSMALLINT cchConnStrOutMax, SQLSMALLINT* pcchConnStrOut);
	nothrow @nogc SQLRETURN SQLBulkOperations(SQLHSTMT StatementHandle, SQLSMALLINT Operation);
	nothrow @nogc SQLRETURN SQLColAttributes(SQLHSTMT hstmt, SQLUSMALLINT icol, SQLUSMALLINT fDescType, SQLPOINTER rgbDesc, SQLSMALLINT cbDescMax, SQLSMALLINT* pcbDesc, SQLINTEGER* pfDesc);
	nothrow @nogc SQLRETURN SQLColumnPrivileges(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLCHAR* szTableName, SQLSMALLINT cchTableName, SQLCHAR* szColumnName, SQLSMALLINT cchColumnName);
	nothrow @nogc SQLRETURN SQLDescribeParam(SQLHSTMT hstmt, SQLUSMALLINT ipar, SQLSMALLINT* pfSqlType, SQLUINTEGER* pcbParamDef, SQLSMALLINT* pibScale, SQLSMALLINT* pfNullable);
	nothrow @nogc SQLRETURN SQLExtendedFetch(SQLHSTMT hstmt, SQLUSMALLINT fFetchType, SQLINTEGER irow, SQLUINTEGER* pcrow, SQLUSMALLINT* rgfRowStatus);
	nothrow @nogc SQLRETURN SQLForeignKeys(SQLHSTMT hstmt, SQLCHAR* szPkCatalogName, SQLSMALLINT cchPkCatalogName, SQLCHAR* szPkSchemaName, SQLSMALLINT cchPkSchemaName, SQLCHAR* szPkTableName, SQLSMALLINT cchPkTableName, SQLCHAR* szFkCatalogName, SQLSMALLINT cchFkCatalogName, SQLCHAR* szFkSchemaName, SQLSMALLINT cchFkSchemaName, SQLCHAR* szFkTableName, SQLSMALLINT cchFkTableName);
	nothrow @nogc SQLRETURN SQLMoreResults(SQLHSTMT hstmt);
	nothrow @nogc SQLRETURN SQLNativeSql(SQLHDBC hdbc, SQLCHAR* szSqlStrIn, SQLINTEGER cchSqlStrIn, SQLCHAR* szSqlStr, SQLINTEGER cchSqlStrMax, SQLINTEGER* pcbSqlStr);
	nothrow @nogc SQLRETURN SQLNumParams(SQLHSTMT hstmt, SQLSMALLINT* pcpar);
	nothrow @nogc SQLRETURN SQLParamOptions(SQLHSTMT hstmt, SQLUINTEGER crow, SQLUINTEGER* pirow);
	nothrow @nogc SQLRETURN SQLPrimaryKeys(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLCHAR* szTableName, SQLSMALLINT cchTableName);
	nothrow @nogc SQLRETURN SQLProcedureColumns(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLCHAR* szProcName, SQLSMALLINT cchProcName, SQLCHAR* szColumnName, SQLSMALLINT cchColumnName);
	nothrow @nogc SQLRETURN SQLProcedures(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLCHAR* szProcName, SQLSMALLINT cchProcName);
	nothrow @nogc SQLRETURN SQLSetPos(SQLHSTMT hstmt, SQLUSMALLINT irow, SQLUSMALLINT fOption, SQLUSMALLINT fLock);
	nothrow @nogc SQLRETURN SQLTablePrivileges(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLCHAR* szTableName, SQLSMALLINT cchTableName);
	nothrow @nogc SQLRETURN SQLDrivers(SQLHENV henv, SQLUSMALLINT fDirection, SQLCHAR* szDriverDesc, SQLSMALLINT cchDriverDescMax, SQLSMALLINT* pcchDriverDesc, SQLCHAR* szDriverAttributes, SQLSMALLINT cchDrvrAttrMax, SQLSMALLINT* pcchDrvrAttr);
	nothrow @nogc SQLRETURN SQLBindParameter(SQLHSTMT hstmt, SQLUSMALLINT ipar, SQLSMALLINT fParamType, SQLSMALLINT fCType, SQLSMALLINT fSqlType, SQLUINTEGER cbColDef, SQLSMALLINT ibScale, SQLPOINTER rgbValue, SQLINTEGER cbValueMax, SQLINTEGER* pcbValue);
	nothrow @nogc SQLRETURN SQLAllocHandle(SQLSMALLINT HandleType, SQLHANDLE InputHandle, SQLHANDLE* OutputHandle);
	nothrow @nogc SQLRETURN SQLGetNestedHandle(SQLHSTMT ParentStatementHandle, SQLUSMALLINT Col_or_Param_Num, SQLHSTMT* OutputChildStatementHandle);
	nothrow @nogc SQLRETURN SQLStructuredTypes(SQLHSTMT StatementHandle, SQLCHAR* CatalogName, SQLSMALLINT NameLength1, SQLCHAR* SchemaName, SQLSMALLINT NameLength2, SQLCHAR* TypeName, SQLSMALLINT NameLength3);
	nothrow @nogc SQLRETURN SQLStructuredTypeColumns(SQLHSTMT StatementHandle, SQLCHAR* CatalogName, SQLSMALLINT NameLength1, SQLCHAR* SchemaName, SQLSMALLINT NameLength2, SQLCHAR* TypeName, SQLSMALLINT NameLength3, SQLCHAR* ColumnName, SQLSMALLINT NameLength4);
	nothrow @nogc SQLRETURN SQLNextColumn(SQLHSTMT StatementHandle, SQLUSMALLINT* ColumnCount);
	nothrow @nogc SQLRETURN SQLAllocHandleStd(SQLSMALLINT fHandleType, SQLHANDLE hInput, SQLHANDLE* phOutput);
	nothrow @nogc SQLRETURN SQLColAttributeW(SQLHSTMT hstmt, SQLUSMALLINT iCol, SQLUSMALLINT iField, SQLPOINTER pCharAttr, SQLSMALLINT cbDescMax, SQLSMALLINT* pcbCharAttr, SQLPOINTER pNumAttr);
	nothrow @nogc SQLRETURN SQLColAttributesW(SQLHSTMT hstmt, SQLUSMALLINT icol, SQLUSMALLINT fDescType, SQLPOINTER rgbDesc, SQLSMALLINT cbDescMax, SQLSMALLINT* pcbDesc, SQLINTEGER* pfDesc);
	nothrow @nogc SQLRETURN SQLConnectW(SQLHDBC hdbc, SQLWCHAR* szDSN, SQLSMALLINT cchDSN, SQLWCHAR* szUID, SQLSMALLINT cchUID, SQLWCHAR* szAuthStr, SQLSMALLINT cchAuthStr);
	nothrow @nogc SQLRETURN SQLDescribeColW(SQLHSTMT hstmt, SQLUSMALLINT icol, SQLWCHAR* szColName, SQLSMALLINT cchColNameMax, SQLSMALLINT* pcchColName, SQLSMALLINT* pfSqlType, SQLUINTEGER* pcbColDef, SQLSMALLINT* pibScale, SQLSMALLINT* pfNullable);
	nothrow @nogc SQLRETURN SQLErrorW(SQLHENV henv, SQLHDBC hdbc, SQLHSTMT hstmt, SQLWCHAR* wszSqlState, SQLINTEGER* pfNativeError, SQLWCHAR* wszErrorMsg, SQLSMALLINT cchErrorMsgMax, SQLSMALLINT* pcchErrorMsg);
	nothrow @nogc SQLRETURN SQLExecDirectW(SQLHSTMT hstmt, SQLWCHAR* szSqlStr, SQLINTEGER TextLength);
	nothrow @nogc SQLRETURN SQLGetConnectAttrW(SQLHDBC hdbc, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValueMax, SQLINTEGER* pcbValue);
	nothrow @nogc SQLRETURN SQLGetCursorNameW(SQLHSTMT hstmt, SQLWCHAR* szCursor, SQLSMALLINT cchCursorMax, SQLSMALLINT* pcchCursor);
	nothrow @nogc SQLRETURN SQLSetDescFieldW(SQLHDESC DescriptorHandle, SQLSMALLINT RecNumber, SQLSMALLINT FieldIdentifier, SQLPOINTER Value, SQLINTEGER BufferLength);
	nothrow @nogc SQLRETURN SQLGetDescFieldW(SQLHDESC hdesc, SQLSMALLINT iRecord, SQLSMALLINT iField, SQLPOINTER rgbValue, SQLINTEGER cbBufferLength, SQLINTEGER* StringLength);
	nothrow @nogc SQLRETURN SQLGetDescRecW(SQLHDESC hdesc, SQLSMALLINT iRecord, SQLWCHAR* szName, SQLSMALLINT cchNameMax, SQLSMALLINT* pcchName, SQLSMALLINT* pfType, SQLSMALLINT* pfSubType, SQLINTEGER* pLength, SQLSMALLINT* pPrecision, SQLSMALLINT* pScale, SQLSMALLINT* pNullable);
	nothrow @nogc SQLRETURN SQLGetDiagFieldW(SQLSMALLINT fHandleType, SQLHANDLE handle, SQLSMALLINT iRecord, SQLSMALLINT fDiagField, SQLPOINTER rgbDiagInfo, SQLSMALLINT cbBufferLength, SQLSMALLINT* pcbStringLength);
	nothrow @nogc SQLRETURN SQLGetDiagRecW(SQLSMALLINT fHandleType, SQLHANDLE handle, SQLSMALLINT iRecord, SQLWCHAR* szSqlState, SQLINTEGER* pfNativeError, SQLWCHAR* szErrorMsg, SQLSMALLINT cchErrorMsgMax, SQLSMALLINT* pcchErrorMsg);
	nothrow @nogc SQLRETURN SQLPrepareW(SQLHSTMT hstmt, SQLWCHAR* szSqlStr, SQLINTEGER cchSqlStr);
	nothrow @nogc SQLRETURN SQLSetConnectAttrW(SQLHDBC hdbc, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValue);
	nothrow @nogc SQLRETURN SQLSetCursorNameW(SQLHSTMT hstmt, SQLWCHAR* szCursor, SQLSMALLINT cchCursor);
	nothrow @nogc SQLRETURN SQLColumnsW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName, SQLWCHAR* szColumnName, SQLSMALLINT cchColumnName);
	nothrow @nogc SQLRETURN SQLGetConnectOptionW(SQLHDBC hdbc, SQLUSMALLINT fOption, SQLPOINTER pvParam);
	nothrow @nogc SQLRETURN SQLGetInfoW(SQLHDBC hdbc, SQLUSMALLINT fInfoType, SQLPOINTER rgbInfoValue, SQLSMALLINT cbInfoValueMax, SQLSMALLINT* pcbInfoValue);
	nothrow @nogc SQLRETURN SQLGetTypeInfoW(SQLHSTMT StatementHandle, SQLSMALLINT DataType);
	nothrow @nogc SQLRETURN SQLSetConnectOptionW(SQLHDBC hdbc, SQLUSMALLINT fOption, SQLUINTEGER vParam);
	nothrow @nogc SQLRETURN SQLSpecialColumnsW(SQLHSTMT hstmt, SQLUSMALLINT fColType, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName, SQLUSMALLINT fScope, SQLUSMALLINT fNullable);
	nothrow @nogc SQLRETURN SQLStatisticsW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName, SQLUSMALLINT fUnique, SQLUSMALLINT fAccuracy);
	nothrow @nogc SQLRETURN SQLTablesW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName, SQLWCHAR* szTableType, SQLSMALLINT cchTableType);
	nothrow @nogc SQLRETURN SQLDataSourcesW(SQLHENV henv, SQLUSMALLINT fDirection, SQLWCHAR* szDSN, SQLSMALLINT cchDSNMax, SQLSMALLINT* pcchDSN, SQLWCHAR* wszDescription, SQLSMALLINT cchDescriptionMax, SQLSMALLINT* pcchDescription);
	nothrow @nogc SQLRETURN SQLDriverConnectW(SQLHDBC hdbc, SQLHWND hwnd, SQLWCHAR* szConnStrIn, SQLSMALLINT cchConnStrIn, SQLWCHAR* szConnStrOut, SQLSMALLINT cchConnStrOutMax, SQLSMALLINT* pcchConnStrOut, SQLUSMALLINT fDriverCompletion);
	nothrow @nogc SQLRETURN SQLBrowseConnectW(SQLHDBC hdbc, SQLWCHAR* szConnStrIn, SQLSMALLINT cchConnStrIn, SQLWCHAR* szConnStrOut, SQLSMALLINT cchConnStrOutMax, SQLSMALLINT* pcchConnStrOut);
	nothrow @nogc SQLRETURN SQLColumnPrivilegesW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName, SQLWCHAR* szColumnName, SQLSMALLINT cchColumnName);
	nothrow @nogc SQLRETURN SQLGetStmtAttrW(SQLHSTMT hstmt, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValueMax, SQLINTEGER* pcbValue);
	nothrow @nogc SQLRETURN SQLSetStmtAttrW(SQLHSTMT hstmt, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValueMax);
	nothrow @nogc SQLRETURN SQLForeignKeysW(SQLHSTMT hstmt, SQLWCHAR* szPkCatalogName, SQLSMALLINT cchPkCatalogName, SQLWCHAR* szPkSchemaName, SQLSMALLINT cchPkSchemaName, SQLWCHAR* szPkTableName, SQLSMALLINT cchPkTableName, SQLWCHAR* szFkCatalogName, SQLSMALLINT cchFkCatalogName, SQLWCHAR* szFkSchemaName, SQLSMALLINT cchFkSchemaName, SQLWCHAR* szFkTableName, SQLSMALLINT cchFkTableName);
	nothrow @nogc SQLRETURN SQLNativeSqlW(SQLHDBC hdbc, SQLWCHAR* szSqlStrIn, SQLINTEGER cchSqlStrIn, SQLWCHAR* szSqlStr, SQLINTEGER cchSqlStrMax, SQLINTEGER* pcchSqlStr);
	nothrow @nogc SQLRETURN SQLPrimaryKeysW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName);
	nothrow @nogc SQLRETURN SQLProcedureColumnsW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szProcName, SQLSMALLINT cchProcName, SQLWCHAR* szColumnName, SQLSMALLINT cchColumnName);
	nothrow @nogc SQLRETURN SQLProceduresW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szProcName, SQLSMALLINT cchProcName);
	nothrow @nogc SQLRETURN SQLTablePrivilegesW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTableName, SQLSMALLINT cchTableName);
	nothrow @nogc SQLRETURN SQLDriversW(SQLHENV henv, SQLUSMALLINT fDirection, SQLWCHAR* szDriverDesc, SQLSMALLINT cchDriverDescMax, SQLSMALLINT* pcchDriverDesc, SQLWCHAR* szDriverAttributes, SQLSMALLINT cchDrvrAttrMax, SQLSMALLINT* pcchDrvrAttr);
	nothrow @nogc SQLRETURN SQLStructuredTypesW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTypeName, SQLSMALLINT cchTypeName);
	nothrow @nogc SQLRETURN SQLStructuredTypeColumnsW(SQLHSTMT hstmt, SQLWCHAR* szCatalogName, SQLSMALLINT cchCatalogName, SQLWCHAR* szSchemaName, SQLSMALLINT cchSchemaName, SQLWCHAR* szTypeName, SQLSMALLINT cchTypeName, SQLWCHAR* szColumnName, SQLSMALLINT cchColumnName);
	nothrow @nogc SQLRETURN SQLColAttributeA(SQLHSTMT hstmt, SQLSMALLINT iCol, SQLSMALLINT iField, SQLPOINTER pCharAttr, SQLSMALLINT cbCharAttrMax, SQLSMALLINT* pcbCharAttr, SQLPOINTER pNumAttr);
	nothrow @nogc SQLRETURN SQLColAttributesA(SQLHSTMT hstmt, SQLUSMALLINT icol, SQLUSMALLINT fDescType, SQLPOINTER rgbDesc, SQLSMALLINT cbDescMax, SQLSMALLINT* pcbDesc, SQLINTEGER* pfDesc);
	nothrow @nogc SQLRETURN SQLConnectA(SQLHDBC hdbc, SQLCHAR* szDSN, SQLSMALLINT cbDSN, SQLCHAR* szUID, SQLSMALLINT cbUID, SQLCHAR* szAuthStr, SQLSMALLINT cbAuthStr);
	nothrow @nogc SQLRETURN SQLDescribeColA(SQLHSTMT hstmt, SQLUSMALLINT icol, SQLCHAR* szColName, SQLSMALLINT cbColNameMax, SQLSMALLINT* pcbColName, SQLSMALLINT* pfSqlType, SQLUINTEGER* pcbColDef, SQLSMALLINT* pibScale, SQLSMALLINT* pfNullable);
	nothrow @nogc SQLRETURN SQLErrorA(SQLHENV henv, SQLHDBC hdbc, SQLHSTMT hstmt, SQLCHAR* szSqlState, SQLINTEGER* pfNativeError, SQLCHAR* szErrorMsg, SQLSMALLINT cbErrorMsgMax, SQLSMALLINT* pcbErrorMsg);
	nothrow @nogc SQLRETURN SQLExecDirectA(SQLHSTMT hstmt, SQLCHAR* szSqlStr, SQLINTEGER cbSqlStr);
	nothrow @nogc SQLRETURN SQLGetConnectAttrA(SQLHDBC hdbc, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValueMax, SQLINTEGER* pcbValue);
	nothrow @nogc SQLRETURN SQLGetCursorNameA(SQLHSTMT hstmt, SQLCHAR* szCursor, SQLSMALLINT cbCursorMax, SQLSMALLINT* pcbCursor);
	nothrow @nogc SQLRETURN SQLGetDescFieldA(SQLHDESC hdesc, SQLSMALLINT iRecord, SQLSMALLINT iField, SQLPOINTER rgbValue, SQLINTEGER cbBufferLength, SQLINTEGER* StringLength);
	nothrow @nogc SQLRETURN SQLGetDescRecA(SQLHDESC hdesc, SQLSMALLINT iRecord, SQLCHAR* szName, SQLSMALLINT cbNameMax, SQLSMALLINT* pcbName, SQLSMALLINT* pfType, SQLSMALLINT* pfSubType, SQLINTEGER* pLength, SQLSMALLINT* pPrecision, SQLSMALLINT* pScale, SQLSMALLINT* pNullable);
	nothrow @nogc SQLRETURN SQLGetDiagFieldA(SQLSMALLINT fHandleType, SQLHANDLE handle, SQLSMALLINT iRecord, SQLSMALLINT fDiagField, SQLPOINTER rgbDiagInfo, SQLSMALLINT cbDiagInfoMax, SQLSMALLINT* pcbDiagInfo);
	nothrow @nogc SQLRETURN SQLGetDiagRecA(SQLSMALLINT fHandleType, SQLHANDLE handle, SQLSMALLINT iRecord, SQLCHAR* szSqlState, SQLINTEGER* pfNativeError, SQLCHAR* szErrorMsg, SQLSMALLINT cbErrorMsgMax, SQLSMALLINT* pcbErrorMsg);
	nothrow @nogc SQLRETURN SQLGetStmtAttrA(SQLHSTMT hstmt, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValueMax, SQLINTEGER* pcbValue);
	nothrow @nogc SQLRETURN SQLGetTypeInfoA(SQLHSTMT StatementHandle, SQLSMALLINT DataType);
	nothrow @nogc SQLRETURN SQLPrepareA(SQLHSTMT hstmt, SQLCHAR* szSqlStr, SQLINTEGER cbSqlStr);
	nothrow @nogc SQLRETURN SQLSetConnectAttrA(SQLHDBC hdbc, SQLINTEGER fAttribute, SQLPOINTER rgbValue, SQLINTEGER cbValue);
	nothrow @nogc SQLRETURN SQLSetCursorNameA(SQLHSTMT hstmt, SQLCHAR* szCursor, SQLSMALLINT cbCursor);
	nothrow @nogc SQLRETURN SQLColumnsA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName, SQLCHAR* szColumnName, SQLSMALLINT cbColumnName);
	nothrow @nogc SQLRETURN SQLGetConnectOptionA(SQLHDBC hdbc, SQLUSMALLINT fOption, SQLPOINTER pvParam);
	nothrow @nogc SQLRETURN SQLGetInfoA(SQLHDBC hdbc, SQLUSMALLINT fInfoType, SQLPOINTER rgbInfoValue, SQLSMALLINT cbInfoValueMax, SQLSMALLINT* pcbInfoValue);
	nothrow @nogc SQLRETURN SQLGetStmtOptionA(SQLHSTMT hstmt, SQLUSMALLINT fOption, SQLPOINTER pvParam);
	nothrow @nogc SQLRETURN SQLSetConnectOptionA(SQLHDBC hdbc, SQLUSMALLINT fOption, SQLUINTEGER vParam);
	nothrow @nogc SQLRETURN SQLSetStmtOptionA(SQLHSTMT hstmt, SQLUSMALLINT fOption, SQLUINTEGER vParam);
	nothrow @nogc SQLRETURN SQLSpecialColumnsA(SQLHSTMT hstmt, SQLUSMALLINT fColType, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName, SQLUSMALLINT fScope, SQLUSMALLINT fNullable);
	nothrow @nogc SQLRETURN SQLStatisticsA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName, SQLUSMALLINT fUnique, SQLUSMALLINT fAccuracy);
	nothrow @nogc SQLRETURN SQLTablesA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName, SQLCHAR* szTableType, SQLSMALLINT cbTableType);
	nothrow @nogc SQLRETURN SQLDataSourcesA(SQLHENV henv, SQLUSMALLINT fDirection, SQLCHAR* szDSN, SQLSMALLINT cbDSNMax, SQLSMALLINT* pcbDSN, SQLCHAR* szDescription, SQLSMALLINT cbDescriptionMax, SQLSMALLINT* pcbDescription);
	nothrow @nogc SQLRETURN SQLDriverConnectA(SQLHDBC hdbc, SQLHWND hwnd, SQLCHAR* szConnStrIn, SQLSMALLINT cbConnStrIn, SQLCHAR* szConnStrOut, SQLSMALLINT cbConnStrOutMax, SQLSMALLINT* pcbConnStrOut, SQLUSMALLINT fDriverCompletion);
	nothrow @nogc SQLRETURN SQLBrowseConnectA(SQLHDBC hdbc, SQLCHAR* szConnStrIn, SQLSMALLINT cbConnStrIn, SQLCHAR* szConnStrOut, SQLSMALLINT cbConnStrOutMax, SQLSMALLINT* pcbConnStrOut);
	nothrow @nogc SQLRETURN SQLColumnPrivilegesA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName, SQLCHAR* szColumnName, SQLSMALLINT cbColumnName);
	nothrow @nogc SQLRETURN SQLDescribeParamA(SQLHSTMT hstmt, SQLUSMALLINT ipar, SQLSMALLINT* pfSqlType, SQLUINTEGER* pcbParamDef, SQLSMALLINT* pibScale, SQLSMALLINT* pfNullable);
	nothrow @nogc SQLRETURN SQLForeignKeysA(SQLHSTMT hstmt, SQLCHAR* szPkCatalogName, SQLSMALLINT cbPkCatalogName, SQLCHAR* szPkSchemaName, SQLSMALLINT cbPkSchemaName, SQLCHAR* szPkTableName, SQLSMALLINT cbPkTableName, SQLCHAR* szFkCatalogName, SQLSMALLINT cbFkCatalogName, SQLCHAR* szFkSchemaName, SQLSMALLINT cbFkSchemaName, SQLCHAR* szFkTableName, SQLSMALLINT cbFkTableName);
	nothrow @nogc SQLRETURN SQLNativeSqlA(SQLHDBC hdbc, SQLCHAR* szSqlStrIn, SQLINTEGER cbSqlStrIn, SQLCHAR* szSqlStr, SQLINTEGER cbSqlStrMax, SQLINTEGER* pcbSqlStr);
	nothrow @nogc SQLRETURN SQLPrimaryKeysA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName);
	nothrow @nogc SQLRETURN SQLProcedureColumnsA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szProcName, SQLSMALLINT cbProcName, SQLCHAR* szColumnName, SQLSMALLINT cbColumnName);
	nothrow @nogc SQLRETURN SQLProceduresA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szProcName, SQLSMALLINT cbProcName);
	nothrow @nogc SQLRETURN SQLTablePrivilegesA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTableName, SQLSMALLINT cbTableName);
	nothrow @nogc SQLRETURN SQLDriversA(SQLHENV henv, SQLUSMALLINT fDirection, SQLCHAR* szDriverDesc, SQLSMALLINT cbDriverDescMax, SQLSMALLINT* pcbDriverDesc, SQLCHAR* szDriverAttributes, SQLSMALLINT cbDrvrAttrMax, SQLSMALLINT* pcbDrvrAttr);
	nothrow @nogc SQLRETURN SQLStructuredTypesA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTypeName, SQLSMALLINT cbTypeName);
	nothrow @nogc SQLRETURN SQLStructuredTypeColumnsA(SQLHSTMT hstmt, SQLCHAR* szCatalogName, SQLSMALLINT cbCatalogName, SQLCHAR* szSchemaName, SQLSMALLINT cbSchemaName, SQLCHAR* szTypeName, SQLSMALLINT cbTypeName, SQLCHAR* szColumnName, SQLSMALLINT cbColumnName);
	enum SQLINTEGER ODBCVER = 1024;
	enum SQLINTEGER SQL_MAX_NUMERIC_LEN = 16;
	auto SQL_NULL_DATA()()
	{
		return -1;
	}
	auto SQL_DATA_AT_EXEC()()
	{
		return -2;
	}
	enum SQLINTEGER SQL_SUCCESS = 0;
	enum SQLINTEGER SQL_SUCCESS_WITH_INFO = 1;
	enum SQLINTEGER SQL_NO_DATA = 100;
	enum SQLINTEGER SQL_PARAM_DATA_AVAILABLE = 101;
	auto SQL_ERROR()()
	{
		return -1;
	}
	auto SQL_INVALID_HANDLE()()
	{
		return -2;
	}
	enum SQLINTEGER SQL_STILL_EXECUTING = 2;
	enum SQLINTEGER SQL_NEED_DATA = 99;
	auto SQL_SUCCEEDED(__MP1)(__MP1 rc)
	{
		return (rc & ~1) == 0;
	}
	auto SQL_NTS()()
	{
		return -3;
	}
	auto SQL_NTSL()()
	{
		return -3;
	}
	enum SQLINTEGER SQL_MAX_MESSAGE_LENGTH = 512;
	enum SQLINTEGER SQL_DATE_LEN = 10;
	enum SQLINTEGER SQL_TIME_LEN = 8;
	enum SQLINTEGER SQL_TIMESTAMP_LEN = 19;
	enum SQLINTEGER SQL_HANDLE_ENV = 1;
	enum SQLINTEGER SQL_HANDLE_DBC = 2;
	enum SQLINTEGER SQL_HANDLE_STMT = 3;
	enum SQLINTEGER SQL_HANDLE_DESC = 4;
	enum SQLINTEGER SQL_ATTR_OUTPUT_NTS = 10001;
	enum SQLINTEGER SQL_ATTR_AUTO_IPD = 10001;
	enum SQLINTEGER SQL_ATTR_METADATA_ID = 10014;
	enum SQLINTEGER SQL_ATTR_APP_ROW_DESC = 10010;
	enum SQLINTEGER SQL_ATTR_APP_PARAM_DESC = 10011;
	enum SQLINTEGER SQL_ATTR_IMP_ROW_DESC = 10012;
	enum SQLINTEGER SQL_ATTR_IMP_PARAM_DESC = 10013;
	auto SQL_ATTR_CURSOR_SCROLLABLE()()
	{
		return -1;
	}
	auto SQL_ATTR_CURSOR_SENSITIVITY()()
	{
		return -2;
	}
	enum SQLINTEGER SQL_NONSCROLLABLE = 0;
	enum SQLINTEGER SQL_SCROLLABLE = 1;
	enum SQLINTEGER SQL_DESC_COUNT = 1001;
	enum SQLINTEGER SQL_DESC_TYPE = 1002;
	enum SQLINTEGER SQL_DESC_LENGTH = 1003;
	enum SQLINTEGER SQL_DESC_OCTET_LENGTH_PTR = 1004;
	enum SQLINTEGER SQL_DESC_PRECISION = 1005;
	enum SQLINTEGER SQL_DESC_SCALE = 1006;
	enum SQLINTEGER SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
	enum SQLINTEGER SQL_DESC_NULLABLE = 1008;
	enum SQLINTEGER SQL_DESC_INDICATOR_PTR = 1009;
	enum SQLINTEGER SQL_DESC_DATA_PTR = 1010;
	enum SQLINTEGER SQL_DESC_NAME = 1011;
	enum SQLINTEGER SQL_DESC_UNNAMED = 1012;
	enum SQLINTEGER SQL_DESC_OCTET_LENGTH = 1013;
	enum SQLINTEGER SQL_DESC_ALLOC_TYPE = 1099;
	enum SQLINTEGER SQL_DESC_CHARACTER_SET_CATALOG = 1018;
	enum SQLINTEGER SQL_DESC_CHARACTER_SET_SCHEMA = 1019;
	enum SQLINTEGER SQL_DESC_CHARACTER_SET_NAME = 1020;
	enum SQLINTEGER SQL_DESC_COLLATION_CATALOG = 1015;
	enum SQLINTEGER SQL_DESC_COLLATION_SCHEMA = 1016;
	enum SQLINTEGER SQL_DESC_COLLATION_NAME = 1017;
	enum SQLINTEGER SQL_DESC_USER_DEFINED_TYPE_CATALOG = 1026;
	enum SQLINTEGER SQL_DESC_USER_DEFINED_TYPE_SCHEMA = 1027;
	enum SQLINTEGER SQL_DESC_USER_DEFINED_TYPE_NAME = 1028;
	enum SQLINTEGER SQL_DIAG_RETURNCODE = 1;
	enum SQLINTEGER SQL_DIAG_NUMBER = 2;
	enum SQLINTEGER SQL_DIAG_ROW_COUNT = 3;
	enum SQLINTEGER SQL_DIAG_SQLSTATE = 4;
	enum SQLINTEGER SQL_DIAG_NATIVE = 5;
	enum SQLINTEGER SQL_DIAG_MESSAGE_TEXT = 6;
	enum SQLINTEGER SQL_DIAG_DYNAMIC_FUNCTION = 7;
	enum SQLINTEGER SQL_DIAG_CLASS_ORIGIN = 8;
	enum SQLINTEGER SQL_DIAG_SUBCLASS_ORIGIN = 9;
	enum SQLINTEGER SQL_DIAG_CONNECTION_NAME = 10;
	enum SQLINTEGER SQL_DIAG_SERVER_NAME = 11;
	enum SQLINTEGER SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;
	enum SQLINTEGER SQL_DIAG_ALTER_DOMAIN = 3;
	enum SQLINTEGER SQL_DIAG_ALTER_TABLE = 4;
	enum SQLINTEGER SQL_DIAG_CALL = 7;
	enum SQLINTEGER SQL_DIAG_CREATE_ASSERTION = 6;
	enum SQLINTEGER SQL_DIAG_CREATE_CHARACTER_SET = 8;
	enum SQLINTEGER SQL_DIAG_CREATE_COLLATION = 10;
	enum SQLINTEGER SQL_DIAG_CREATE_DOMAIN = 23;
	auto SQL_DIAG_CREATE_INDEX()()
	{
		return -1;
	}
	enum SQLINTEGER SQL_DIAG_CREATE_SCHEMA = 64;
	enum SQLINTEGER SQL_DIAG_CREATE_TABLE = 77;
	enum SQLINTEGER SQL_DIAG_CREATE_TRANSLATION = 79;
	enum SQLINTEGER SQL_DIAG_CREATE_VIEW = 84;
	enum SQLINTEGER SQL_DIAG_DELETE_WHERE = 19;
	enum SQLINTEGER SQL_DIAG_DROP_ASSERTION = 24;
	enum SQLINTEGER SQL_DIAG_DROP_CHARACTER_SET = 25;
	enum SQLINTEGER SQL_DIAG_DROP_COLLATION = 26;
	enum SQLINTEGER SQL_DIAG_DROP_DOMAIN = 27;
	auto SQL_DIAG_DROP_INDEX()()
	{
		return -2;
	}
	enum SQLINTEGER SQL_DIAG_DROP_SCHEMA = 31;
	enum SQLINTEGER SQL_DIAG_DROP_TABLE = 32;
	enum SQLINTEGER SQL_DIAG_DROP_TRANSLATION = 33;
	enum SQLINTEGER SQL_DIAG_DROP_VIEW = 36;
	enum SQLINTEGER SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
	enum SQLINTEGER SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
	enum SQLINTEGER SQL_DIAG_GRANT = 48;
	enum SQLINTEGER SQL_DIAG_INSERT = 50;
	enum SQLINTEGER SQL_DIAG_REVOKE = 59;
	enum SQLINTEGER SQL_DIAG_SELECT_CURSOR = 85;
	enum SQLINTEGER SQL_DIAG_UNKNOWN_STATEMENT = 0;
	enum SQLINTEGER SQL_DIAG_UPDATE_WHERE = 82;
	enum SQLINTEGER SQL_UNKNOWN_TYPE = 0;
	enum SQLINTEGER SQL_CHAR = 1;
	enum SQLINTEGER SQL_NUMERIC = 2;
	enum SQLINTEGER SQL_DECIMAL = 3;
	enum SQLINTEGER SQL_INTEGER = 4;
	enum SQLINTEGER SQL_SMALLINT = 5;
	enum SQLINTEGER SQL_FLOAT = 6;
	enum SQLINTEGER SQL_REAL = 7;
	enum SQLINTEGER SQL_DOUBLE = 8;
	enum SQLINTEGER SQL_DATETIME = 9;
	enum SQLINTEGER SQL_VARCHAR = 12;
	enum SQLINTEGER SQL_UDT = 17;
	enum SQLINTEGER SQL_ROW = 19;
	enum SQLINTEGER SQL_ARRAY = 50;
	enum SQLINTEGER SQL_MULTISET = 55;
	enum SQLINTEGER SQL_TYPE_DATE = 91;
	enum SQLINTEGER SQL_TYPE_TIME = 92;
	enum SQLINTEGER SQL_TYPE_TIMESTAMP = 93;
	enum SQLINTEGER SQL_TYPE_TIME_WITH_TIMEZONE = 94;
	enum SQLINTEGER SQL_TYPE_TIMESTAMP_WITH_TIMEZONE = 95;
	enum SQLINTEGER SQL_UNSPECIFIED = 0;
	enum SQLINTEGER SQL_INSENSITIVE = 1;
	enum SQLINTEGER SQL_SENSITIVE = 2;
	enum SQLINTEGER SQL_ALL_TYPES = 0;
	enum SQLINTEGER SQL_DEFAULT = 99;
	auto SQL_ARD_TYPE()()
	{
		return -99;
	}
	auto SQL_APD_TYPE()()
	{
		return -100;
	}
	enum SQLINTEGER SQL_CODE_DATE = 1;
	enum SQLINTEGER SQL_CODE_TIME = 2;
	enum SQLINTEGER SQL_CODE_TIMESTAMP = 3;
	enum SQLINTEGER SQL_CODE_TIME_WITH_TIMEZONE = 4;
	enum SQLINTEGER SQL_CODE_TIMESTAMP_WITH_TIMEZONE = 5;
	enum SQLINTEGER SQL_FALSE = 0;
	enum SQLINTEGER SQL_TRUE = 1;
	enum SQLINTEGER SQL_NO_NULLS = 0;
	enum SQLINTEGER SQL_NULLABLE = 1;
	enum SQLINTEGER SQL_NULLABLE_UNKNOWN = 2;
	enum SQLINTEGER SQL_PRED_NONE = 0;
	enum SQLINTEGER SQL_PRED_CHAR = 1;
	enum SQLINTEGER SQL_PRED_BASIC = 2;
	enum SQLINTEGER SQL_NAMED = 0;
	enum SQLINTEGER SQL_UNNAMED = 1;
	enum SQLINTEGER SQL_DESC_ALLOC_AUTO = 1;
	enum SQLINTEGER SQL_DESC_ALLOC_USER = 2;
	enum SQLINTEGER SQL_CLOSE = 0;
	enum SQLINTEGER SQL_DROP = 1;
	enum SQLINTEGER SQL_UNBIND = 2;
	enum SQLINTEGER SQL_RESET_PARAMS = 3;
	enum SQLINTEGER SQL_FETCH_NEXT = 1;
	enum SQLINTEGER SQL_FETCH_FIRST = 2;
	enum SQLINTEGER SQL_FETCH_LAST = 3;
	enum SQLINTEGER SQL_FETCH_PRIOR = 4;
	enum SQLINTEGER SQL_FETCH_ABSOLUTE = 5;
	enum SQLINTEGER SQL_FETCH_RELATIVE = 6;
	enum SQLINTEGER SQL_COMMIT = 0;
	enum SQLINTEGER SQL_ROLLBACK = 1;
	enum SQLINTEGER SQL_NULL_HENV = 0;
	enum SQLINTEGER SQL_NULL_HDBC = 0;
	enum SQLINTEGER SQL_NULL_HSTMT = 0;
	enum SQLINTEGER SQL_NULL_HDESC = 0;
	enum SQLINTEGER SQL_NULL_HANDLE = 0;
	enum SQLINTEGER SQL_SCOPE_CURROW = 0;
	enum SQLINTEGER SQL_SCOPE_TRANSACTION = 1;
	enum SQLINTEGER SQL_SCOPE_SESSION = 2;
	enum SQLINTEGER SQL_PC_UNKNOWN = 0;
	enum SQLINTEGER SQL_PC_NON_PSEUDO = 1;
	enum SQLINTEGER SQL_PC_PSEUDO = 2;
	enum SQLINTEGER SQL_ROW_IDENTIFIER = 1;
	enum SQLINTEGER SQL_INDEX_UNIQUE = 0;
	enum SQLINTEGER SQL_INDEX_ALL = 1;
	enum SQLINTEGER SQL_INDEX_CLUSTERED = 1;
	enum SQLINTEGER SQL_INDEX_HASHED = 2;
	enum SQLINTEGER SQL_INDEX_OTHER = 3;
	enum SQLINTEGER SQL_MAX_DRIVER_CONNECTIONS = 0;
	enum SQLINTEGER SQL_MAX_CONCURRENT_ACTIVITIES = 1;
	enum SQLINTEGER SQL_DATA_SOURCE_NAME = 2;
	enum SQLINTEGER SQL_FETCH_DIRECTION = 8;
	enum SQLINTEGER SQL_SERVER_NAME = 13;
	enum SQLINTEGER SQL_SEARCH_PATTERN_ESCAPE = 14;
	enum SQLINTEGER SQL_DBMS_NAME = 17;
	enum SQLINTEGER SQL_DBMS_VER = 18;
	enum SQLINTEGER SQL_ACCESSIBLE_TABLES = 19;
	enum SQLINTEGER SQL_ACCESSIBLE_PROCEDURES = 20;
	enum SQLINTEGER SQL_CURSOR_COMMIT_BEHAVIOR = 23;
	enum SQLINTEGER SQL_DATA_SOURCE_READ_ONLY = 25;
	enum SQLINTEGER SQL_DEFAULT_TXN_ISOLATION = 26;
	enum SQLINTEGER SQL_IDENTIFIER_CASE = 28;
	enum SQLINTEGER SQL_IDENTIFIER_QUOTE_CHAR = 29;
	enum SQLINTEGER SQL_MAX_COLUMN_NAME_LEN = 30;
	enum SQLINTEGER SQL_MAX_CURSOR_NAME_LEN = 31;
	enum SQLINTEGER SQL_MAX_SCHEMA_NAME_LEN = 32;
	enum SQLINTEGER SQL_MAX_CATALOG_NAME_LEN = 34;
	enum SQLINTEGER SQL_MAX_TABLE_NAME_LEN = 35;
	enum SQLINTEGER SQL_SCROLL_CONCURRENCY = 43;
	enum SQLINTEGER SQL_TXN_CAPABLE = 46;
	enum SQLINTEGER SQL_USER_NAME = 47;
	enum SQLINTEGER SQL_TXN_ISOLATION_OPTION = 72;
	enum SQLINTEGER SQL_INTEGRITY = 73;
	enum SQLINTEGER SQL_GETDATA_EXTENSIONS = 81;
	enum SQLINTEGER SQL_NULL_COLLATION = 85;
	enum SQLINTEGER SQL_ALTER_TABLE = 86;
	enum SQLINTEGER SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
	enum SQLINTEGER SQL_SPECIAL_CHARACTERS = 94;
	enum SQLINTEGER SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
	enum SQLINTEGER SQL_MAX_COLUMNS_IN_INDEX = 98;
	enum SQLINTEGER SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
	enum SQLINTEGER SQL_MAX_COLUMNS_IN_SELECT = 100;
	enum SQLINTEGER SQL_MAX_COLUMNS_IN_TABLE = 101;
	enum SQLINTEGER SQL_MAX_INDEX_SIZE = 102;
	enum SQLINTEGER SQL_MAX_ROW_SIZE = 104;
	enum SQLINTEGER SQL_MAX_STATEMENT_LEN = 105;
	enum SQLINTEGER SQL_MAX_TABLES_IN_SELECT = 106;
	enum SQLINTEGER SQL_MAX_USER_NAME_LEN = 107;
	enum SQLINTEGER SQL_OJ_CAPABILITIES = 115;
	enum SQLINTEGER SQL_XOPEN_CLI_YEAR = 10000;
	enum SQLINTEGER SQL_CURSOR_SENSITIVITY = 10001;
	enum SQLINTEGER SQL_DESCRIBE_PARAMETER = 10002;
	enum SQLINTEGER SQL_CATALOG_NAME = 10003;
	enum SQLINTEGER SQL_COLLATION_SEQ = 10004;
	enum SQLINTEGER SQL_MAX_IDENTIFIER_LEN = 10005;
	enum SQLINTEGER SQL_AT_ADD_COLUMN = 1;
	enum SQLINTEGER SQL_AT_DROP_COLUMN = 2;
	enum SQLINTEGER SQL_AT_ADD_CONSTRAINT = 8;
	enum SQLINTEGER SQL_AM_NONE = 0;
	enum SQLINTEGER SQL_AM_CONNECTION = 1;
	enum SQLINTEGER SQL_AM_STATEMENT = 2;
	enum SQLINTEGER SQL_CB_DELETE = 0;
	enum SQLINTEGER SQL_CB_CLOSE = 1;
	enum SQLINTEGER SQL_CB_PRESERVE = 2;
	enum SQLINTEGER SQL_FD_FETCH_NEXT = 1;
	enum SQLINTEGER SQL_FD_FETCH_FIRST = 2;
	enum SQLINTEGER SQL_FD_FETCH_LAST = 4;
	enum SQLINTEGER SQL_FD_FETCH_PRIOR = 8;
	enum SQLINTEGER SQL_FD_FETCH_ABSOLUTE = 16;
	enum SQLINTEGER SQL_FD_FETCH_RELATIVE = 32;
	enum SQLINTEGER SQL_GD_ANY_COLUMN = 1;
	enum SQLINTEGER SQL_GD_ANY_ORDER = 2;
	enum SQLINTEGER SQL_IC_UPPER = 1;
	enum SQLINTEGER SQL_IC_LOWER = 2;
	enum SQLINTEGER SQL_IC_SENSITIVE = 3;
	enum SQLINTEGER SQL_IC_MIXED = 4;
	enum SQLINTEGER SQL_OJ_LEFT = 1;
	enum SQLINTEGER SQL_OJ_RIGHT = 2;
	enum SQLINTEGER SQL_OJ_FULL = 4;
	enum SQLINTEGER SQL_OJ_NESTED = 8;
	enum SQLINTEGER SQL_OJ_NOT_ORDERED = 16;
	enum SQLINTEGER SQL_OJ_INNER = 32;
	enum SQLINTEGER SQL_OJ_ALL_COMPARISON_OPS = 64;
	enum SQLINTEGER SQL_SCCO_READ_ONLY = 1;
	enum SQLINTEGER SQL_SCCO_LOCK = 2;
	enum SQLINTEGER SQL_SCCO_OPT_ROWVER = 4;
	enum SQLINTEGER SQL_SCCO_OPT_VALUES = 8;
	enum SQLINTEGER SQL_TC_NONE = 0;
	enum SQLINTEGER SQL_TC_DML = 1;
	enum SQLINTEGER SQL_TC_ALL = 2;
	enum SQLINTEGER SQL_TC_DDL_COMMIT = 3;
	enum SQLINTEGER SQL_TC_DDL_IGNORE = 4;
	enum SQLINTEGER SQL_TXN_READ_UNCOMMITTED = 1;
	enum SQLINTEGER SQL_TXN_READ_COMMITTED = 2;
	enum SQLINTEGER SQL_TXN_REPEATABLE_READ = 4;
	enum SQLINTEGER SQL_TXN_SERIALIZABLE = 8;
	enum SQLINTEGER SQL_NC_HIGH = 0;
	enum SQLINTEGER SQL_NC_LOW = 1;
	enum SQLINTEGER SQL_SPEC_MAJOR = 4;
	enum SQLINTEGER SQL_SPEC_MINOR = 0;
	enum SQL_SPEC_STRING = "04.00";
	enum SQLINTEGER SQL_SQLSTATE_SIZE = 5;
	enum SQLINTEGER SQL_MAX_DSN_LENGTH = 32;
	enum SQLINTEGER SQL_MAX_OPTION_STRING_LENGTH = 256;
	enum SQLINTEGER SQL_DATA_AVAILABLE = 102;
	enum SQLINTEGER SQL_METADATA_CHANGED = 103;
	enum SQLINTEGER SQL_MORE_DATA = 104;
	enum SQLINTEGER SQL_HANDLE_SENV = 5;
	enum SQLINTEGER SQL_ATTR_ODBC_VERSION = 200;
	enum SQLINTEGER SQL_ATTR_CONNECTION_POOLING = 201;
	enum SQLINTEGER SQL_ATTR_CP_MATCH = 202;
	enum SQLINTEGER SQL_ATTR_APPLICATION_KEY = 203;
	enum SQLUINTEGER SQL_CP_OFF = 0u;
	enum SQLUINTEGER SQL_CP_ONE_PER_DRIVER = 1u;
	enum SQLUINTEGER SQL_CP_ONE_PER_HENV = 2u;
	enum SQLUINTEGER SQL_CP_DRIVER_AWARE = 3u;
	enum SQLUINTEGER SQL_CP_STRICT_MATCH = 0u;
	enum SQLUINTEGER SQL_CP_RELAXED_MATCH = 1u;
	enum SQLUINTEGER SQL_OV_ODBC2 = 2u;
	enum SQLUINTEGER SQL_OV_ODBC3 = 3u;
	enum SQLUINTEGER SQL_OV_ODBC3_80 = 380u;
	enum SQLUINTEGER SQL_OV_ODBC4 = 400u;
	enum SQLINTEGER SQL_ACCESS_MODE = 101;
	enum SQLINTEGER SQL_AUTOCOMMIT = 102;
	enum SQLINTEGER SQL_LOGIN_TIMEOUT = 103;
	enum SQLINTEGER SQL_OPT_TRACE = 104;
	enum SQLINTEGER SQL_OPT_TRACEFILE = 105;
	enum SQLINTEGER SQL_TRANSLATE_DLL = 106;
	enum SQLINTEGER SQL_TRANSLATE_OPTION = 107;
	enum SQLINTEGER SQL_TXN_ISOLATION = 108;
	enum SQLINTEGER SQL_CURRENT_QUALIFIER = 109;
	enum SQLINTEGER SQL_ODBC_CURSORS = 110;
	enum SQLINTEGER SQL_QUIET_MODE = 111;
	enum SQLINTEGER SQL_PACKET_SIZE = 112;
	enum SQLINTEGER SQL_ATTR_CONNECTION_TIMEOUT = 113;
	enum SQLINTEGER SQL_ATTR_DISCONNECT_BEHAVIOR = 114;
	enum SQLINTEGER SQL_ATTR_ENLIST_IN_DTC = 1207;
	enum SQLINTEGER SQL_ATTR_ENLIST_IN_XA = 1208;
	enum SQLINTEGER SQL_ATTR_CONNECTION_DEAD = 1209;
	enum SQLINTEGER SQL_ATTR_ANSI_APP = 115;
	enum SQLINTEGER SQL_ATTR_RESET_CONNECTION = 116;
	enum SQLINTEGER SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE = 117;
	enum SQLINTEGER SQL_ATTR_ASYNC_DBC_EVENT = 119;
	enum SQLINTEGER SQL_ATTR_CREDENTIALS = 122;
	enum SQLINTEGER SQL_ATTR_REFRESH_CONNECTION = 123;
	enum SQLUINTEGER SQL_MODE_READ_WRITE = 0u;
	enum SQLUINTEGER SQL_MODE_READ_ONLY = 1u;
	enum SQLUINTEGER SQL_AUTOCOMMIT_OFF = 0u;
	enum SQLUINTEGER SQL_AUTOCOMMIT_ON = 1u;
	enum SQLUINTEGER SQL_LOGIN_TIMEOUT_DEFAULT = 15u;
	enum SQLUINTEGER SQL_OPT_TRACE_OFF = 0u;
	enum SQLUINTEGER SQL_OPT_TRACE_ON = 1u;
	enum SQL_OPT_TRACE_FILE_DEFAULT = "\\SQL.LOG";
	enum SQLUINTEGER SQL_CUR_USE_IF_NEEDED = 0u;
	enum SQLUINTEGER SQL_CUR_USE_ODBC = 1u;
	enum SQLUINTEGER SQL_CUR_USE_DRIVER = 2u;
	enum SQLUINTEGER SQL_DB_RETURN_TO_POOL = 0u;
	enum SQLUINTEGER SQL_DB_DISCONNECT = 1u;
	enum SQLINTEGER SQL_DTC_DONE = 0;
	enum SQLINTEGER SQL_CD_TRUE = 1;
	enum SQLINTEGER SQL_CD_FALSE = 0;
	enum SQLINTEGER SQL_AA_TRUE = 1;
	enum SQLINTEGER SQL_AA_FALSE = 0;
	enum SQLUINTEGER SQL_RESET_CONNECTION_YES = 1u;
	enum SQLUINTEGER SQL_ASYNC_DBC_ENABLE_ON = 1u;
	enum SQLUINTEGER SQL_ASYNC_DBC_ENABLE_OFF = 0u;
	enum SQLINTEGER SQL_REFRESH_NOW = -1;
	enum SQLINTEGER SQL_REFRESH_AUTO = 0;
	enum SQLINTEGER SQL_REFRESH_MANUAL = 1;
	enum SQLINTEGER SQL_QUERY_TIMEOUT = 0;
	enum SQLINTEGER SQL_MAX_ROWS = 1;
	enum SQLINTEGER SQL_NOSCAN = 2;
	enum SQLINTEGER SQL_MAX_LENGTH = 3;
	enum SQLINTEGER SQL_ASYNC_ENABLE = 4;
	enum SQLINTEGER SQL_BIND_TYPE = 5;
	enum SQLINTEGER SQL_CURSOR_TYPE = 6;
	enum SQLINTEGER SQL_CONCURRENCY = 7;
	enum SQLINTEGER SQL_KEYSET_SIZE = 8;
	enum SQLINTEGER SQL_ROWSET_SIZE = 9;
	enum SQLINTEGER SQL_SIMULATE_CURSOR = 10;
	enum SQLINTEGER SQL_RETRIEVE_DATA = 11;
	enum SQLINTEGER SQL_USE_BOOKMARKS = 12;
	enum SQLINTEGER SQL_GET_BOOKMARK = 13;
	enum SQLINTEGER SQL_ROW_NUMBER = 14;
	enum SQLINTEGER SQL_ATTR_ENABLE_AUTO_IPD = 15;
	enum SQLINTEGER SQL_ATTR_FETCH_BOOKMARK_PTR = 16;
	enum SQLINTEGER SQL_ATTR_PARAM_BIND_OFFSET_PTR = 17;
	enum SQLINTEGER SQL_ATTR_PARAM_BIND_TYPE = 18;
	enum SQLINTEGER SQL_ATTR_PARAM_OPERATION_PTR = 19;
	enum SQLINTEGER SQL_ATTR_PARAM_STATUS_PTR = 20;
	enum SQLINTEGER SQL_ATTR_PARAMS_PROCESSED_PTR = 21;
	enum SQLINTEGER SQL_ATTR_PARAMSET_SIZE = 22;
	enum SQLINTEGER SQL_ATTR_ROW_BIND_OFFSET_PTR = 23;
	enum SQLINTEGER SQL_ATTR_ROW_OPERATION_PTR = 24;
	enum SQLINTEGER SQL_ATTR_ROW_STATUS_PTR = 25;
	enum SQLINTEGER SQL_ATTR_ROWS_FETCHED_PTR = 26;
	enum SQLINTEGER SQL_ATTR_ROW_ARRAY_SIZE = 27;
	enum SQLINTEGER SQL_ATTR_ASYNC_STMT_EVENT = 29;
	enum SQLINTEGER SQL_ATTR_SAMPLE_SIZE = 30;
	enum SQLINTEGER SQL_ATTR_DYNAMIC_COLUMNS = 31;
	enum SQLINTEGER SQL_ATTR_TYPE_EXCEPTION_BEHAVIOR = 32;
	enum SQLINTEGER SQL_ATTR_LENGTH_EXCEPTION_BEHAVIOR = 33;
	enum SQLINTEGER SQL_TE_ERROR = 1;
	enum SQLINTEGER SQL_TE_CONTINUE = 2;
	enum SQLINTEGER SQL_TE_REPORT = 3;
	enum SQLINTEGER SQL_LE_CONTINUE = 1;
	enum SQLINTEGER SQL_LE_REPORT = 2;
	auto SQL_IS_POINTER()()
	{
		return -4;
	}
	auto SQL_IS_UINTEGER()()
	{
		return -5;
	}
	auto SQL_IS_INTEGER()()
	{
		return -6;
	}
	auto SQL_IS_USMALLINT()()
	{
		return -7;
	}
	auto SQL_IS_SMALLINT()()
	{
		return -8;
	}
	enum SQLUINTEGER SQL_PARAM_BIND_BY_COLUMN = 0u;
	enum SQLUINTEGER SQL_QUERY_TIMEOUT_DEFAULT = 0u;
	enum SQLUINTEGER SQL_MAX_ROWS_DEFAULT = 0u;
	enum SQLUINTEGER SQL_NOSCAN_OFF = 0u;
	enum SQLUINTEGER SQL_NOSCAN_ON = 1u;
	enum SQLUINTEGER SQL_MAX_LENGTH_DEFAULT = 0u;
	enum SQLUINTEGER SQL_ASYNC_ENABLE_OFF = 0u;
	enum SQLUINTEGER SQL_ASYNC_ENABLE_ON = 1u;
	enum SQLUINTEGER SQL_BIND_BY_COLUMN = 0u;
	enum SQLINTEGER SQL_CONCUR_READ_ONLY = 1;
	enum SQLINTEGER SQL_CONCUR_LOCK = 2;
	enum SQLINTEGER SQL_CONCUR_ROWVER = 3;
	enum SQLINTEGER SQL_CONCUR_VALUES = 4;
	enum SQLUINTEGER SQL_CURSOR_FORWARD_ONLY = 0u;
	enum SQLUINTEGER SQL_CURSOR_KEYSET_DRIVEN = 1u;
	enum SQLUINTEGER SQL_CURSOR_DYNAMIC = 2u;
	enum SQLUINTEGER SQL_CURSOR_STATIC = 3u;
	enum SQLUINTEGER SQL_ROWSET_SIZE_DEFAULT = 1u;
	enum SQLUINTEGER SQL_KEYSET_SIZE_DEFAULT = 0u;
	enum SQLUINTEGER SQL_SC_NON_UNIQUE = 0u;
	enum SQLUINTEGER SQL_SC_TRY_UNIQUE = 1u;
	enum SQLUINTEGER SQL_SC_UNIQUE = 2u;
	enum SQLUINTEGER SQL_RD_OFF = 0u;
	enum SQLUINTEGER SQL_RD_ON = 1u;
	enum SQLUINTEGER SQL_UB_OFF = 0u;
	enum SQLUINTEGER SQL_UB_ON = 1u;
	enum SQLUINTEGER SQL_UB_VARIABLE = 2u;
	enum SQLINTEGER SQL_DESC_ARRAY_SIZE = 20;
	enum SQLINTEGER SQL_DESC_ARRAY_STATUS_PTR = 21;
	enum SQLINTEGER SQL_DESC_BASE_COLUMN_NAME = 22;
	enum SQLINTEGER SQL_DESC_BASE_TABLE_NAME = 23;
	enum SQLINTEGER SQL_DESC_BIND_OFFSET_PTR = 24;
	enum SQLINTEGER SQL_DESC_BIND_TYPE = 25;
	enum SQLINTEGER SQL_DESC_DATETIME_INTERVAL_PRECISION = 26;
	enum SQLINTEGER SQL_DESC_LITERAL_PREFIX = 27;
	enum SQLINTEGER SQL_DESC_LITERAL_SUFFIX = 28;
	enum SQLINTEGER SQL_DESC_LOCAL_TYPE_NAME = 29;
	enum SQLINTEGER SQL_DESC_MAXIMUM_SCALE = 30;
	enum SQLINTEGER SQL_DESC_MINIMUM_SCALE = 31;
	enum SQLINTEGER SQL_DESC_NUM_PREC_RADIX = 32;
	enum SQLINTEGER SQL_DESC_PARAMETER_TYPE = 33;
	enum SQLINTEGER SQL_DESC_ROWS_PROCESSED_PTR = 34;
	enum SQLINTEGER SQL_DESC_ROWVER = 35;
	enum SQLINTEGER SQL_DESC_MIME_TYPE = 36;
	auto SQL_DIAG_CURSOR_ROW_COUNT()()
	{
		return -1249;
	}
	auto SQL_DIAG_ROW_NUMBER()()
	{
		return -1248;
	}
	auto SQL_DIAG_COLUMN_NUMBER()()
	{
		return -1247;
	}
	enum SQLINTEGER SQL_DATE = 9;
	enum SQLINTEGER SQL_INTERVAL = 10;
	enum SQLINTEGER SQL_TIME = 10;
	enum SQLINTEGER SQL_TIMESTAMP = 11;
	auto SQL_LONGVARCHAR()()
	{
		return -1;
	}
	auto SQL_BINARY()()
	{
		return -2;
	}
	auto SQL_VARBINARY()()
	{
		return -3;
	}
	auto SQL_LONGVARBINARY()()
	{
		return -4;
	}
	auto SQL_BIGINT()()
	{
		return -5;
	}
	auto SQL_TINYINT()()
	{
		return -6;
	}
	auto SQL_BIT()()
	{
		return -7;
	}
	auto SQL_GUID()()
	{
		return -11;
	}
	enum SQLINTEGER SQL_CODE_YEAR = 1;
	enum SQLINTEGER SQL_CODE_MONTH = 2;
	enum SQLINTEGER SQL_CODE_DAY = 3;
	enum SQLINTEGER SQL_CODE_HOUR = 4;
	enum SQLINTEGER SQL_CODE_MINUTE = 5;
	enum SQLINTEGER SQL_CODE_SECOND = 6;
	enum SQLINTEGER SQL_CODE_YEAR_TO_MONTH = 7;
	enum SQLINTEGER SQL_CODE_DAY_TO_HOUR = 8;
	enum SQLINTEGER SQL_CODE_DAY_TO_MINUTE = 9;
	enum SQLINTEGER SQL_CODE_DAY_TO_SECOND = 10;
	enum SQLINTEGER SQL_CODE_HOUR_TO_MINUTE = 11;
	enum SQLINTEGER SQL_CODE_HOUR_TO_SECOND = 12;
	enum SQLINTEGER SQL_CODE_MINUTE_TO_SECOND = 13;
	auto SQL_INTERVAL_YEAR()()
	{
		return 100 + SQL_CODE_YEAR;
	}
	auto SQL_INTERVAL_MONTH()()
	{
		return 100 + SQL_CODE_MONTH;
	}
	auto SQL_INTERVAL_DAY()()
	{
		return 100 + SQL_CODE_DAY;
	}
	auto SQL_INTERVAL_HOUR()()
	{
		return 100 + SQL_CODE_HOUR;
	}
	auto SQL_INTERVAL_MINUTE()()
	{
		return 100 + SQL_CODE_MINUTE;
	}
	auto SQL_INTERVAL_SECOND()()
	{
		return 100 + SQL_CODE_SECOND;
	}
	auto SQL_INTERVAL_YEAR_TO_MONTH()()
	{
		return 100 + SQL_CODE_YEAR_TO_MONTH;
	}
	auto SQL_INTERVAL_DAY_TO_HOUR()()
	{
		return 100 + SQL_CODE_DAY_TO_HOUR;
	}
	auto SQL_INTERVAL_DAY_TO_MINUTE()()
	{
		return 100 + SQL_CODE_DAY_TO_MINUTE;
	}
	auto SQL_INTERVAL_DAY_TO_SECOND()()
	{
		return 100 + SQL_CODE_DAY_TO_SECOND;
	}
	auto SQL_INTERVAL_HOUR_TO_MINUTE()()
	{
		return 100 + SQL_CODE_HOUR_TO_MINUTE;
	}
	auto SQL_INTERVAL_HOUR_TO_SECOND()()
	{
		return 100 + SQL_CODE_HOUR_TO_SECOND;
	}
	auto SQL_INTERVAL_MINUTE_TO_SECOND()()
	{
		return 100 + SQL_CODE_MINUTE_TO_SECOND;
	}
	enum SQLINTEGER SQL_C_DEFAULT = 99;
	auto SQL_SIGNED_OFFSET()()
	{
		return -20;
	}
	auto SQL_UNSIGNED_OFFSET()()
	{
		return -22;
	}
	auto SQL_C_SBIGINT()()
	{
		return SQL_BIGINT + SQL_SIGNED_OFFSET;
	}
	auto SQL_C_UBIGINT()()
	{
		return SQL_BIGINT + SQL_UNSIGNED_OFFSET;
	}
	auto SQL_C_SLONG()()
	{
		return SQL_C_LONG + SQL_SIGNED_OFFSET;
	}
	auto SQL_C_SSHORT()()
	{
		return SQL_C_SHORT + SQL_SIGNED_OFFSET;
	}
	auto SQL_C_STINYINT()()
	{
		return SQL_TINYINT + SQL_SIGNED_OFFSET;
	}
	auto SQL_C_ULONG()()
	{
		return SQL_C_LONG + SQL_UNSIGNED_OFFSET;
	}
	auto SQL_C_USHORT()()
	{
		return SQL_C_SHORT + SQL_UNSIGNED_OFFSET;
	}
	auto SQL_C_UTINYINT()()
	{
		return SQL_TINYINT + SQL_UNSIGNED_OFFSET;
	}
	enum SQLINTEGER SQL_TYPE_NULL = 0;
	enum SQLINTEGER SQL_DRIVER_C_TYPE_BASE = 16384;
	enum SQLINTEGER SQL_DRIVER_SQL_TYPE_BASE = 16384;
	enum SQLINTEGER SQL_DRIVER_DESC_FIELD_BASE = 16384;
	enum SQLINTEGER SQL_DRIVER_DIAG_FIELD_BASE = 16384;
	enum SQLINTEGER SQL_DRIVER_INFO_TYPE_BASE = 16384;
	enum SQLINTEGER SQL_DRIVER_CONN_ATTR_BASE = 16384;
	enum SQLINTEGER SQL_DRIVER_STMT_ATTR_BASE = 16384;
	auto SQL_NO_ROW_NUMBER()()
	{
		return -1;
	}
	auto SQL_NO_COLUMN_NUMBER()()
	{
		return -1;
	}
	auto SQL_ROW_NUMBER_UNKNOWN()()
	{
		return -2;
	}
	auto SQL_COLUMN_NUMBER_UNKNOWN()()
	{
		return -2;
	}
	auto SQL_DEFAULT_PARAM()()
	{
		return -5;
	}
	auto SQL_IGNORE()()
	{
		return -6;
	}
	auto SQL_LEN_DATA_AT_EXEC_OFFSET()()
	{
		return -100;
	}
	auto SQL_LEN_DATA_AT_EXEC(__MP2)(__MP2 length)
	{
		return -length + SQL_LEN_DATA_AT_EXEC_OFFSET;
	}
	auto SQL_LEN_BINARY_ATTR_OFFSET()()
	{
		return -100;
	}
	auto SQL_LEN_BINARY_ATTR(__MP3)(__MP3 length)
	{
		return -length + SQL_LEN_BINARY_ATTR_OFFSET;
	}
	auto SQL_SETPARAM_VALUE_MAX()()
	{
		return -1;
	}
	auto SQL_TYPE_EXCEPTION()()
	{
		return -20;
	}
	enum SQLINTEGER SQL_COLUMN_COUNT = 0;
	enum SQLINTEGER SQL_COLUMN_NAME = 1;
	enum SQLINTEGER SQL_COLUMN_TYPE = 2;
	enum SQLINTEGER SQL_COLUMN_LENGTH = 3;
	enum SQLINTEGER SQL_COLUMN_PRECISION = 4;
	enum SQLINTEGER SQL_COLUMN_SCALE = 5;
	enum SQLINTEGER SQL_COLUMN_DISPLAY_SIZE = 6;
	enum SQLINTEGER SQL_COLUMN_NULLABLE = 7;
	enum SQLINTEGER SQL_COLUMN_UNSIGNED = 8;
	enum SQLINTEGER SQL_COLUMN_MONEY = 9;
	enum SQLINTEGER SQL_COLUMN_UPDATABLE = 10;
	enum SQLINTEGER SQL_COLUMN_AUTO_INCREMENT = 11;
	enum SQLINTEGER SQL_COLUMN_CASE_SENSITIVE = 12;
	enum SQLINTEGER SQL_COLUMN_SEARCHABLE = 13;
	enum SQLINTEGER SQL_COLUMN_TYPE_NAME = 14;
	enum SQLINTEGER SQL_COLUMN_TABLE_NAME = 15;
	enum SQLINTEGER SQL_COLUMN_OWNER_NAME = 16;
	enum SQLINTEGER SQL_COLUMN_QUALIFIER_NAME = 17;
	enum SQLINTEGER SQL_COLUMN_LABEL = 18;
	enum SQLINTEGER SQL_ATTR_READONLY = 0;
	enum SQLINTEGER SQL_ATTR_WRITE = 1;
	enum SQLINTEGER SQL_ATTR_READWRITE_UNKNOWN = 2;
	enum SQLINTEGER SQL_UNSEARCHABLE = 0;
	enum SQLINTEGER SQL_LIKE_ONLY = 1;
	enum SQLINTEGER SQL_ALL_EXCEPT_LIKE = 2;
	enum SQLINTEGER SQL_SEARCHABLE = 3;
	auto SQL_NO_TOTAL()()
	{
		return -4;
	}
	enum SQLINTEGER SQL_INFO_FIRST = 0;
	enum SQLINTEGER SQL_ACTIVE_CONNECTIONS = 0;
	enum SQLINTEGER SQL_ACTIVE_STATEMENTS = 1;
	enum SQLINTEGER SQL_DRIVER_HDBC = 3;
	enum SQLINTEGER SQL_DRIVER_HENV = 4;
	enum SQLINTEGER SQL_DRIVER_HSTMT = 5;
	enum SQLINTEGER SQL_DRIVER_NAME = 6;
	enum SQLINTEGER SQL_DRIVER_VER = 7;
	enum SQLINTEGER SQL_ODBC_API_CONFORMANCE = 9;
	enum SQLINTEGER SQL_ODBC_VER = 10;
	enum SQLINTEGER SQL_ROW_UPDATES = 11;
	enum SQLINTEGER SQL_ODBC_SAG_CLI_CONFORMANCE = 12;
	enum SQLINTEGER SQL_ODBC_SQL_CONFORMANCE = 15;
	enum SQLINTEGER SQL_PROCEDURES = 21;
	enum SQLINTEGER SQL_CONCAT_NULL_BEHAVIOR = 22;
	enum SQLINTEGER SQL_CURSOR_ROLLBACK_BEHAVIOR = 24;
	enum SQLINTEGER SQL_EXPRESSIONS_IN_ORDERBY = 27;
	enum SQLINTEGER SQL_MAX_OWNER_NAME_LEN = 32;
	enum SQLINTEGER SQL_MAX_PROCEDURE_NAME_LEN = 33;
	enum SQLINTEGER SQL_MAX_QUALIFIER_NAME_LEN = 34;
	enum SQLINTEGER SQL_MULT_RESULT_SETS = 36;
	enum SQLINTEGER SQL_MULTIPLE_ACTIVE_TXN = 37;
	enum SQLINTEGER SQL_OUTER_JOINS = 38;
	enum SQLINTEGER SQL_OWNER_TERM = 39;
	enum SQLINTEGER SQL_PROCEDURE_TERM = 40;
	enum SQLINTEGER SQL_QUALIFIER_NAME_SEPARATOR = 41;
	enum SQLINTEGER SQL_QUALIFIER_TERM = 42;
	enum SQLINTEGER SQL_SCROLL_OPTIONS = 44;
	enum SQLINTEGER SQL_TABLE_TERM = 45;
	enum SQLINTEGER SQL_CONVERT_FUNCTIONS = 48;
	enum SQLINTEGER SQL_NUMERIC_FUNCTIONS = 49;
	enum SQLINTEGER SQL_STRING_FUNCTIONS = 50;
	enum SQLINTEGER SQL_SYSTEM_FUNCTIONS = 51;
	enum SQLINTEGER SQL_TIMEDATE_FUNCTIONS = 52;
	enum SQLINTEGER SQL_CONVERT_BIGINT = 53;
	enum SQLINTEGER SQL_CONVERT_BINARY = 54;
	enum SQLINTEGER SQL_CONVERT_BIT = 55;
	enum SQLINTEGER SQL_CONVERT_CHAR = 56;
	enum SQLINTEGER SQL_CONVERT_DATE = 57;
	enum SQLINTEGER SQL_CONVERT_DECIMAL = 58;
	enum SQLINTEGER SQL_CONVERT_DOUBLE = 59;
	enum SQLINTEGER SQL_CONVERT_FLOAT = 60;
	enum SQLINTEGER SQL_CONVERT_INTEGER = 61;
	enum SQLINTEGER SQL_CONVERT_LONGVARCHAR = 62;
	enum SQLINTEGER SQL_CONVERT_NUMERIC = 63;
	enum SQLINTEGER SQL_CONVERT_REAL = 64;
	enum SQLINTEGER SQL_CONVERT_SMALLINT = 65;
	enum SQLINTEGER SQL_CONVERT_TIME = 66;
	enum SQLINTEGER SQL_CONVERT_TIMESTAMP = 67;
	enum SQLINTEGER SQL_CONVERT_TINYINT = 68;
	enum SQLINTEGER SQL_CONVERT_VARBINARY = 69;
	enum SQLINTEGER SQL_CONVERT_VARCHAR = 70;
	enum SQLINTEGER SQL_CONVERT_LONGVARBINARY = 71;
	enum SQLINTEGER SQL_ODBC_SQL_OPT_IEF = 73;
	enum SQLINTEGER SQL_CORRELATION_NAME = 74;
	enum SQLINTEGER SQL_NON_NULLABLE_COLUMNS = 75;
	enum SQLINTEGER SQL_DRIVER_HLIB = 76;
	enum SQLINTEGER SQL_DRIVER_ODBC_VER = 77;
	enum SQLINTEGER SQL_LOCK_TYPES = 78;
	enum SQLINTEGER SQL_POS_OPERATIONS = 79;
	enum SQLINTEGER SQL_POSITIONED_STATEMENTS = 80;
	enum SQLINTEGER SQL_BOOKMARK_PERSISTENCE = 82;
	enum SQLINTEGER SQL_STATIC_SENSITIVITY = 83;
	enum SQLINTEGER SQL_FILE_USAGE = 84;
	enum SQLINTEGER SQL_COLUMN_ALIAS = 87;
	enum SQLINTEGER SQL_GROUP_BY = 88;
	enum SQLINTEGER SQL_KEYWORDS = 89;
	enum SQLINTEGER SQL_OWNER_USAGE = 91;
	enum SQLINTEGER SQL_QUALIFIER_USAGE = 92;
	enum SQLINTEGER SQL_QUOTED_IDENTIFIER_CASE = 93;
	enum SQLINTEGER SQL_SUBQUERIES = 95;
	enum SQLINTEGER SQL_UNION = 96;
	enum SQLINTEGER SQL_MAX_ROW_SIZE_INCLUDES_LONG = 103;
	enum SQLINTEGER SQL_MAX_CHAR_LITERAL_LEN = 108;
	enum SQLINTEGER SQL_TIMEDATE_ADD_INTERVALS = 109;
	enum SQLINTEGER SQL_TIMEDATE_DIFF_INTERVALS = 110;
	enum SQLINTEGER SQL_NEED_LONG_DATA_LEN = 111;
	enum SQLINTEGER SQL_MAX_BINARY_LITERAL_LEN = 112;
	enum SQLINTEGER SQL_LIKE_ESCAPE_CLAUSE = 113;
	enum SQLINTEGER SQL_QUALIFIER_LOCATION = 114;
	enum SQLINTEGER SQL_ACTIVE_ENVIRONMENTS = 116;
	enum SQLINTEGER SQL_ALTER_DOMAIN = 117;
	enum SQLINTEGER SQL_SQL_CONFORMANCE = 118;
	enum SQLINTEGER SQL_DATETIME_LITERALS = 119;
	enum SQLINTEGER SQL_ASYNC_MODE = 10021;
	enum SQLINTEGER SQL_BATCH_ROW_COUNT = 120;
	enum SQLINTEGER SQL_BATCH_SUPPORT = 121;
	enum SQLINTEGER SQL_CONVERT_WCHAR = 122;
	enum SQLINTEGER SQL_CONVERT_INTERVAL_DAY_TIME = 123;
	enum SQLINTEGER SQL_CONVERT_INTERVAL_YEAR_MONTH = 124;
	enum SQLINTEGER SQL_CONVERT_WLONGVARCHAR = 125;
	enum SQLINTEGER SQL_CONVERT_WVARCHAR = 126;
	enum SQLINTEGER SQL_CREATE_ASSERTION = 127;
	enum SQLINTEGER SQL_CREATE_CHARACTER_SET = 128;
	enum SQLINTEGER SQL_CREATE_COLLATION = 129;
	enum SQLINTEGER SQL_CREATE_DOMAIN = 130;
	enum SQLINTEGER SQL_CREATE_SCHEMA = 131;
	enum SQLINTEGER SQL_CREATE_TABLE = 132;
	enum SQLINTEGER SQL_CREATE_TRANSLATION = 133;
	enum SQLINTEGER SQL_CREATE_VIEW = 134;
	enum SQLINTEGER SQL_DRIVER_HDESC = 135;
	enum SQLINTEGER SQL_DROP_ASSERTION = 136;
	enum SQLINTEGER SQL_DROP_CHARACTER_SET = 137;
	enum SQLINTEGER SQL_DROP_COLLATION = 138;
	enum SQLINTEGER SQL_DROP_DOMAIN = 139;
	enum SQLINTEGER SQL_DROP_SCHEMA = 140;
	enum SQLINTEGER SQL_DROP_TABLE = 141;
	enum SQLINTEGER SQL_DROP_TRANSLATION = 142;
	enum SQLINTEGER SQL_DROP_VIEW = 143;
	enum SQLINTEGER SQL_DYNAMIC_CURSOR_ATTRIBUTES1 = 144;
	enum SQLINTEGER SQL_DYNAMIC_CURSOR_ATTRIBUTES2 = 145;
	enum SQLINTEGER SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 = 146;
	enum SQLINTEGER SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 = 147;
	enum SQLINTEGER SQL_INDEX_KEYWORDS = 148;
	enum SQLINTEGER SQL_INFO_SCHEMA_VIEWS = 149;
	enum SQLINTEGER SQL_KEYSET_CURSOR_ATTRIBUTES1 = 150;
	enum SQLINTEGER SQL_KEYSET_CURSOR_ATTRIBUTES2 = 151;
	enum SQLINTEGER SQL_MAX_ASYNC_CONCURRENT_STATEMENTS = 10022;
	enum SQLINTEGER SQL_ODBC_INTERFACE_CONFORMANCE = 152;
	enum SQLINTEGER SQL_PARAM_ARRAY_ROW_COUNTS = 153;
	enum SQLINTEGER SQL_PARAM_ARRAY_SELECTS = 154;
	enum SQLINTEGER SQL_SQL92_DATETIME_FUNCTIONS = 155;
	enum SQLINTEGER SQL_SQL92_FOREIGN_KEY_DELETE_RULE = 156;
	enum SQLINTEGER SQL_SQL92_FOREIGN_KEY_UPDATE_RULE = 157;
	enum SQLINTEGER SQL_SQL92_GRANT = 158;
	enum SQLINTEGER SQL_SQL92_NUMERIC_VALUE_FUNCTIONS = 159;
	enum SQLINTEGER SQL_SQL92_PREDICATES = 160;
	enum SQLINTEGER SQL_SQL92_RELATIONAL_JOIN_OPERATORS = 161;
	enum SQLINTEGER SQL_SQL92_REVOKE = 162;
	enum SQLINTEGER SQL_SQL92_ROW_VALUE_CONSTRUCTOR = 163;
	enum SQLINTEGER SQL_SQL92_STRING_FUNCTIONS = 164;
	enum SQLINTEGER SQL_SQL92_VALUE_EXPRESSIONS = 165;
	enum SQLINTEGER SQL_STANDARD_CLI_CONFORMANCE = 166;
	enum SQLINTEGER SQL_STATIC_CURSOR_ATTRIBUTES1 = 167;
	enum SQLINTEGER SQL_STATIC_CURSOR_ATTRIBUTES2 = 168;
	enum SQLINTEGER SQL_AGGREGATE_FUNCTIONS = 169;
	enum SQLINTEGER SQL_DDL_INDEX = 170;
	enum SQLINTEGER SQL_DM_VER = 171;
	enum SQLINTEGER SQL_INSERT_STATEMENT = 172;
	enum SQLINTEGER SQL_CONVERT_GUID = 173;
	enum SQLINTEGER SQL_SCHEMA_INFERENCE = 174;
	enum SQLINTEGER SQL_BINARY_FUNCTIONS = 175;
	enum SQLINTEGER SQL_ISO_STRING_FUNCTIONS = 176;
	enum SQLINTEGER SQL_ISO_BINARY_FUNCTIONS = 177;
	enum SQLINTEGER SQL_LIMIT_ESCAPE_CLAUSE = 178;
	enum SQLINTEGER SQL_NATIVE_ESCAPE_CLAUSE = 179;
	enum SQLINTEGER SQL_RETURN_ESCAPE_CLAUSE = 180;
	enum SQLINTEGER SQL_FORMAT_ESCAPE_CLAUSE = 181;
	enum SQLINTEGER SQL_ASYNC_DBC_FUNCTIONS = 10023;
	enum SQLINTEGER SQL_DRIVER_AWARE_POOLING_SUPPORTED = 10024;
	enum SQLINTEGER SQL_ASYNC_NOTIFICATION = 10025;
	enum SQLINTEGER SQL_ASYNC_NOTIFICATION_NOT_CAPABLE = 0;
	enum SQLINTEGER SQL_ASYNC_NOTIFICATION_CAPABLE = 1;
	enum SQLINTEGER SQL_DTC_TRANSITION_COST = 1750;
	enum SQLINTEGER SQL_AT_ADD_COLUMN_SINGLE = 32;
	enum SQLINTEGER SQL_AT_ADD_COLUMN_DEFAULT = 64;
	enum SQLINTEGER SQL_AT_ADD_COLUMN_COLLATION = 128;
	enum SQLINTEGER SQL_AT_SET_COLUMN_DEFAULT = 256;
	enum SQLINTEGER SQL_AT_DROP_COLUMN_DEFAULT = 512;
	enum SQLINTEGER SQL_AT_DROP_COLUMN_CASCADE = 1024;
	enum SQLINTEGER SQL_AT_DROP_COLUMN_RESTRICT = 2048;
	enum SQLINTEGER SQL_AT_ADD_TABLE_CONSTRAINT = 4096;
	enum SQLINTEGER SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE = 8192;
	enum SQLINTEGER SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT = 16384;
	enum SQLINTEGER SQL_AT_CONSTRAINT_NAME_DEFINITION = 32768;
	enum SQLINTEGER SQL_AT_CONSTRAINT_INITIALLY_DEFERRED = 65536;
	enum SQLINTEGER SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE = 131072;
	enum SQLINTEGER SQL_AT_CONSTRAINT_DEFERRABLE = 262144;
	enum SQLINTEGER SQL_AT_CONSTRAINT_NON_DEFERRABLE = 524288;
	enum SQLINTEGER SQL_CVT_CHAR = 1;
	enum SQLINTEGER SQL_CVT_NUMERIC = 2;
	enum SQLINTEGER SQL_CVT_DECIMAL = 4;
	enum SQLINTEGER SQL_CVT_INTEGER = 8;
	enum SQLINTEGER SQL_CVT_SMALLINT = 16;
	enum SQLINTEGER SQL_CVT_FLOAT = 32;
	enum SQLINTEGER SQL_CVT_REAL = 64;
	enum SQLINTEGER SQL_CVT_DOUBLE = 128;
	enum SQLINTEGER SQL_CVT_VARCHAR = 256;
	enum SQLINTEGER SQL_CVT_LONGVARCHAR = 512;
	enum SQLINTEGER SQL_CVT_BINARY = 1024;
	enum SQLINTEGER SQL_CVT_VARBINARY = 2048;
	enum SQLINTEGER SQL_CVT_BIT = 4096;
	enum SQLINTEGER SQL_CVT_TINYINT = 8192;
	enum SQLINTEGER SQL_CVT_BIGINT = 16384;
	enum SQLINTEGER SQL_CVT_DATE = 32768;
	enum SQLINTEGER SQL_CVT_TIME = 65536;
	enum SQLINTEGER SQL_CVT_TIMESTAMP = 131072;
	enum SQLINTEGER SQL_CVT_LONGVARBINARY = 262144;
	enum SQLINTEGER SQL_CVT_INTERVAL_YEAR_MONTH = 524288;
	enum SQLINTEGER SQL_CVT_INTERVAL_DAY_TIME = 1048576;
	enum SQLINTEGER SQL_CVT_WCHAR = 2097152;
	enum SQLINTEGER SQL_CVT_WLONGVARCHAR = 4194304;
	enum SQLINTEGER SQL_CVT_WVARCHAR = 8388608;
	enum SQLINTEGER SQL_CVT_GUID = 16777216;
	enum SQLINTEGER SQL_FN_CVT_CONVERT = 1;
	enum SQLINTEGER SQL_FN_CVT_CAST = 2;
	enum SQLINTEGER SQL_FN_STR_CONCAT = 1;
	enum SQLINTEGER SQL_FN_STR_INSERT = 2;
	enum SQLINTEGER SQL_FN_STR_LEFT = 4;
	enum SQLINTEGER SQL_FN_STR_LTRIM = 8;
	enum SQLINTEGER SQL_FN_STR_LENGTH = 16;
	enum SQLINTEGER SQL_FN_STR_LOCATE = 32;
	enum SQLINTEGER SQL_FN_STR_LCASE = 64;
	enum SQLINTEGER SQL_FN_STR_REPEAT = 128;
	enum SQLINTEGER SQL_FN_STR_REPLACE = 256;
	enum SQLINTEGER SQL_FN_STR_RIGHT = 512;
	enum SQLINTEGER SQL_FN_STR_RTRIM = 1024;
	enum SQLINTEGER SQL_FN_STR_SUBSTRING = 2048;
	enum SQLINTEGER SQL_FN_STR_UCASE = 4096;
	enum SQLINTEGER SQL_FN_STR_ASCII = 8192;
	enum SQLINTEGER SQL_FN_STR_CHAR = 16384;
	enum SQLINTEGER SQL_FN_STR_DIFFERENCE = 32768;
	enum SQLINTEGER SQL_FN_STR_LOCATE_2 = 65536;
	enum SQLINTEGER SQL_FN_STR_SOUNDEX = 131072;
	enum SQLINTEGER SQL_FN_STR_SPACE = 262144;
	enum SQLINTEGER SQL_FN_STR_BIT_LENGTH = 524288;
	enum SQLINTEGER SQL_FN_STR_CHAR_LENGTH = 1048576;
	enum SQLINTEGER SQL_FN_STR_CHARACTER_LENGTH = 2097152;
	enum SQLINTEGER SQL_FN_STR_OCTET_LENGTH = 4194304;
	enum SQLINTEGER SQL_FN_STR_POSITION = 8388608;
	enum SQLINTEGER SQL_SSF_CONVERT = 1;
	enum SQLINTEGER SQL_SSF_LOWER = 2;
	enum SQLINTEGER SQL_SSF_UPPER = 4;
	enum SQLINTEGER SQL_SSF_SUBSTRING = 8;
	enum SQLINTEGER SQL_SSF_TRANSLATE = 16;
	enum SQLINTEGER SQL_SSF_TRIM_BOTH = 32;
	enum SQLINTEGER SQL_SSF_TRIM_LEADING = 64;
	enum SQLINTEGER SQL_SSF_TRIM_TRAILING = 128;
	enum SQLINTEGER SQL_SSF_OVERLAY = 256;
	enum SQLINTEGER SQL_SSF_LENGTH = 512;
	enum SQLINTEGER SQL_SSF_POSITION = 1024;
	enum SQLINTEGER SQL_SSF_CONCAT = 2048;
	enum SQLINTEGER SQL_FN_NUM_ABS = 1;
	enum SQLINTEGER SQL_FN_NUM_ACOS = 2;
	enum SQLINTEGER SQL_FN_NUM_ASIN = 4;
	enum SQLINTEGER SQL_FN_NUM_ATAN = 8;
	enum SQLINTEGER SQL_FN_NUM_ATAN2 = 16;
	enum SQLINTEGER SQL_FN_NUM_CEILING = 32;
	enum SQLINTEGER SQL_FN_NUM_COS = 64;
	enum SQLINTEGER SQL_FN_NUM_COT = 128;
	enum SQLINTEGER SQL_FN_NUM_EXP = 256;
	enum SQLINTEGER SQL_FN_NUM_FLOOR = 512;
	enum SQLINTEGER SQL_FN_NUM_LOG = 1024;
	enum SQLINTEGER SQL_FN_NUM_MOD = 2048;
	enum SQLINTEGER SQL_FN_NUM_SIGN = 4096;
	enum SQLINTEGER SQL_FN_NUM_SIN = 8192;
	enum SQLINTEGER SQL_FN_NUM_SQRT = 16384;
	enum SQLINTEGER SQL_FN_NUM_TAN = 32768;
	enum SQLINTEGER SQL_FN_NUM_PI = 65536;
	enum SQLINTEGER SQL_FN_NUM_RAND = 131072;
	enum SQLINTEGER SQL_FN_NUM_DEGREES = 262144;
	enum SQLINTEGER SQL_FN_NUM_LOG10 = 524288;
	enum SQLINTEGER SQL_FN_NUM_POWER = 1048576;
	enum SQLINTEGER SQL_FN_NUM_RADIANS = 2097152;
	enum SQLINTEGER SQL_FN_NUM_ROUND = 4194304;
	enum SQLINTEGER SQL_FN_NUM_TRUNCATE = 8388608;
	enum SQLINTEGER SQL_SNVF_BIT_LENGTH = 1;
	enum SQLINTEGER SQL_SNVF_CHAR_LENGTH = 2;
	enum SQLINTEGER SQL_SNVF_CHARACTER_LENGTH = 4;
	enum SQLINTEGER SQL_SNVF_EXTRACT = 8;
	enum SQLINTEGER SQL_SNVF_OCTET_LENGTH = 16;
	enum SQLINTEGER SQL_SNVF_POSITION = 32;
	enum SQLINTEGER SQL_FN_TD_NOW = 1;
	enum SQLINTEGER SQL_FN_TD_CURDATE = 2;
	enum SQLINTEGER SQL_FN_TD_DAYOFMONTH = 4;
	enum SQLINTEGER SQL_FN_TD_DAYOFWEEK = 8;
	enum SQLINTEGER SQL_FN_TD_DAYOFYEAR = 16;
	enum SQLINTEGER SQL_FN_TD_MONTH = 32;
	enum SQLINTEGER SQL_FN_TD_QUARTER = 64;
	enum SQLINTEGER SQL_FN_TD_WEEK = 128;
	enum SQLINTEGER SQL_FN_TD_YEAR = 256;
	enum SQLINTEGER SQL_FN_TD_CURTIME = 512;
	enum SQLINTEGER SQL_FN_TD_HOUR = 1024;
	enum SQLINTEGER SQL_FN_TD_MINUTE = 2048;
	enum SQLINTEGER SQL_FN_TD_SECOND = 4096;
	enum SQLINTEGER SQL_FN_TD_TIMESTAMPADD = 8192;
	enum SQLINTEGER SQL_FN_TD_TIMESTAMPDIFF = 16384;
	enum SQLINTEGER SQL_FN_TD_DAYNAME = 32768;
	enum SQLINTEGER SQL_FN_TD_MONTHNAME = 65536;
	enum SQLINTEGER SQL_FN_TD_CURRENT_DATE = 131072;
	enum SQLINTEGER SQL_FN_TD_CURRENT_TIME = 262144;
	enum SQLINTEGER SQL_FN_TD_CURRENT_TIMESTAMP = 524288;
	enum SQLINTEGER SQL_FN_TD_EXTRACT = 1048576;
	enum SQLINTEGER SQL_SDF_CURRENT_DATE = 1;
	enum SQLINTEGER SQL_SDF_CURRENT_TIME = 2;
	enum SQLINTEGER SQL_SDF_CURRENT_TIMESTAMP = 4;
	enum SQLINTEGER SQL_FN_SYS_USERNAME = 1;
	enum SQLINTEGER SQL_FN_SYS_DBNAME = 2;
	enum SQLINTEGER SQL_FN_SYS_IFNULL = 4;
	enum SQLINTEGER SQL_FN_TSI_FRAC_SECOND = 1;
	enum SQLINTEGER SQL_FN_TSI_SECOND = 2;
	enum SQLINTEGER SQL_FN_TSI_MINUTE = 4;
	enum SQLINTEGER SQL_FN_TSI_HOUR = 8;
	enum SQLINTEGER SQL_FN_TSI_DAY = 16;
	enum SQLINTEGER SQL_FN_TSI_WEEK = 32;
	enum SQLINTEGER SQL_FN_TSI_MONTH = 64;
	enum SQLINTEGER SQL_FN_TSI_QUARTER = 128;
	enum SQLINTEGER SQL_FN_TSI_YEAR = 256;
	enum SQLINTEGER SQL_CA1_NEXT = 1;
	enum SQLINTEGER SQL_CA1_ABSOLUTE = 2;
	enum SQLINTEGER SQL_CA1_RELATIVE = 4;
	enum SQLINTEGER SQL_CA1_BOOKMARK = 8;
	enum SQLINTEGER SQL_CA1_LOCK_NO_CHANGE = 64;
	enum SQLINTEGER SQL_CA1_LOCK_EXCLUSIVE = 128;
	enum SQLINTEGER SQL_CA1_LOCK_UNLOCK = 256;
	enum SQLINTEGER SQL_CA1_POS_POSITION = 512;
	enum SQLINTEGER SQL_CA1_POS_UPDATE = 1024;
	enum SQLINTEGER SQL_CA1_POS_DELETE = 2048;
	enum SQLINTEGER SQL_CA1_POS_REFRESH = 4096;
	enum SQLINTEGER SQL_CA1_POSITIONED_UPDATE = 8192;
	enum SQLINTEGER SQL_CA1_POSITIONED_DELETE = 16384;
	enum SQLINTEGER SQL_CA1_SELECT_FOR_UPDATE = 32768;
	enum SQLINTEGER SQL_CA1_BULK_ADD = 65536;
	enum SQLINTEGER SQL_CA1_BULK_UPDATE_BY_BOOKMARK = 131072;
	enum SQLINTEGER SQL_CA1_BULK_DELETE_BY_BOOKMARK = 262144;
	enum SQLINTEGER SQL_CA1_BULK_FETCH_BY_BOOKMARK = 524288;
	enum SQLINTEGER SQL_CA2_READ_ONLY_CONCURRENCY = 1;
	enum SQLINTEGER SQL_CA2_LOCK_CONCURRENCY = 2;
	enum SQLINTEGER SQL_CA2_OPT_ROWVER_CONCURRENCY = 4;
	enum SQLINTEGER SQL_CA2_OPT_VALUES_CONCURRENCY = 8;
	enum SQLINTEGER SQL_CA2_SENSITIVITY_ADDITIONS = 16;
	enum SQLINTEGER SQL_CA2_SENSITIVITY_DELETIONS = 32;
	enum SQLINTEGER SQL_CA2_SENSITIVITY_UPDATES = 64;
	enum SQLINTEGER SQL_CA2_MAX_ROWS_SELECT = 128;
	enum SQLINTEGER SQL_CA2_MAX_ROWS_INSERT = 256;
	enum SQLINTEGER SQL_CA2_MAX_ROWS_DELETE = 512;
	enum SQLINTEGER SQL_CA2_MAX_ROWS_UPDATE = 1024;
	enum SQLINTEGER SQL_CA2_MAX_ROWS_CATALOG = 2048;
	enum SQLINTEGER SQL_CA2_CRC_EXACT = 4096;
	enum SQLINTEGER SQL_CA2_CRC_APPROXIMATE = 8192;
	enum SQLINTEGER SQL_CA2_SIMULATE_NON_UNIQUE = 16384;
	enum SQLINTEGER SQL_CA2_SIMULATE_TRY_UNIQUE = 32768;
	enum SQLINTEGER SQL_CA2_SIMULATE_UNIQUE = 65536;
	enum SQLINTEGER SQL_OAC_NONE = 0;
	enum SQLINTEGER SQL_OAC_LEVEL1 = 1;
	enum SQLINTEGER SQL_OAC_LEVEL2 = 2;
	enum SQLINTEGER SQL_OSCC_NOT_COMPLIANT = 0;
	enum SQLINTEGER SQL_OSCC_COMPLIANT = 1;
	enum SQLINTEGER SQL_OSC_MINIMUM = 0;
	enum SQLINTEGER SQL_OSC_CORE = 1;
	enum SQLINTEGER SQL_OSC_EXTENDED = 2;
	enum SQLINTEGER SQL_CB_NULL = 0;
	enum SQLINTEGER SQL_CB_NON_NULL = 1;
	enum SQLINTEGER SQL_SO_FORWARD_ONLY = 1;
	enum SQLINTEGER SQL_SO_KEYSET_DRIVEN = 2;
	enum SQLINTEGER SQL_SO_DYNAMIC = 4;
	enum SQLINTEGER SQL_SO_MIXED = 8;
	enum SQLINTEGER SQL_SO_STATIC = 16;
	enum SQLINTEGER SQL_FD_FETCH_BOOKMARK = 128;
	enum SQLINTEGER SQL_CN_NONE = 0;
	enum SQLINTEGER SQL_CN_DIFFERENT = 1;
	enum SQLINTEGER SQL_CN_ANY = 2;
	enum SQLINTEGER SQL_NNC_NULL = 0;
	enum SQLINTEGER SQL_NNC_NON_NULL = 1;
	enum SQLINTEGER SQL_NC_START = 2;
	enum SQLINTEGER SQL_NC_END = 4;
	enum SQLINTEGER SQL_FILE_NOT_SUPPORTED = 0;
	enum SQLINTEGER SQL_FILE_TABLE = 1;
	enum SQLINTEGER SQL_FILE_QUALIFIER = 2;
	enum SQLINTEGER SQL_GD_BLOCK = 4;
	enum SQLINTEGER SQL_GD_BOUND = 8;
	enum SQLINTEGER SQL_GD_OUTPUT_PARAMS = 16;
	enum SQLINTEGER SQL_GD_CONCURRENT = 32;
	enum SQLINTEGER SQL_PS_POSITIONED_DELETE = 1;
	enum SQLINTEGER SQL_PS_POSITIONED_UPDATE = 2;
	enum SQLINTEGER SQL_PS_SELECT_FOR_UPDATE = 4;
	enum SQLINTEGER SQL_GB_NOT_SUPPORTED = 0;
	enum SQLINTEGER SQL_GB_GROUP_BY_EQUALS_SELECT = 1;
	enum SQLINTEGER SQL_GB_GROUP_BY_CONTAINS_SELECT = 2;
	enum SQLINTEGER SQL_GB_NO_RELATION = 3;
	enum SQLINTEGER SQL_GB_COLLATE = 4;
	enum SQLINTEGER SQL_OU_DML_STATEMENTS = 1;
	enum SQLINTEGER SQL_OU_PROCEDURE_INVOCATION = 2;
	enum SQLINTEGER SQL_OU_TABLE_DEFINITION = 4;
	enum SQLINTEGER SQL_OU_INDEX_DEFINITION = 8;
	enum SQLINTEGER SQL_OU_PRIVILEGE_DEFINITION = 16;
	enum SQLINTEGER SQL_QU_DML_STATEMENTS = 1;
	enum SQLINTEGER SQL_QU_PROCEDURE_INVOCATION = 2;
	enum SQLINTEGER SQL_QU_TABLE_DEFINITION = 4;
	enum SQLINTEGER SQL_QU_INDEX_DEFINITION = 8;
	enum SQLINTEGER SQL_QU_PRIVILEGE_DEFINITION = 16;
	enum SQLINTEGER SQL_SQ_COMPARISON = 1;
	enum SQLINTEGER SQL_SQ_EXISTS = 2;
	enum SQLINTEGER SQL_SQ_IN = 4;
	enum SQLINTEGER SQL_SQ_QUANTIFIED = 8;
	enum SQLINTEGER SQL_SQ_CORRELATED_SUBQUERIES = 16;
	enum SQLINTEGER SQL_U_UNION = 1;
	enum SQLINTEGER SQL_U_UNION_ALL = 2;
	enum SQLINTEGER SQL_BP_CLOSE = 1;
	enum SQLINTEGER SQL_BP_DELETE = 2;
	enum SQLINTEGER SQL_BP_DROP = 4;
	enum SQLINTEGER SQL_BP_TRANSACTION = 8;
	enum SQLINTEGER SQL_BP_UPDATE = 16;
	enum SQLINTEGER SQL_BP_OTHER_HSTMT = 32;
	enum SQLINTEGER SQL_BP_SCROLL = 64;
	enum SQLINTEGER SQL_SS_ADDITIONS = 1;
	enum SQLINTEGER SQL_SS_DELETIONS = 2;
	enum SQLINTEGER SQL_SS_UPDATES = 4;
	enum SQLINTEGER SQL_CV_CREATE_VIEW = 1;
	enum SQLINTEGER SQL_CV_CHECK_OPTION = 2;
	enum SQLINTEGER SQL_CV_CASCADED = 4;
	enum SQLINTEGER SQL_CV_LOCAL = 8;
	enum SQLINTEGER SQL_LCK_NO_CHANGE = 1;
	enum SQLINTEGER SQL_LCK_EXCLUSIVE = 2;
	enum SQLINTEGER SQL_LCK_UNLOCK = 4;
	enum SQLINTEGER SQL_POS_POSITION = 1;
	enum SQLINTEGER SQL_POS_REFRESH = 2;
	enum SQLINTEGER SQL_POS_UPDATE = 4;
	enum SQLINTEGER SQL_POS_DELETE = 8;
	enum SQLINTEGER SQL_POS_ADD = 16;
	enum SQLINTEGER SQL_QL_START = 1;
	enum SQLINTEGER SQL_QL_END = 2;
	enum SQLINTEGER SQL_AF_AVG = 1;
	enum SQLINTEGER SQL_AF_COUNT = 2;
	enum SQLINTEGER SQL_AF_MAX = 4;
	enum SQLINTEGER SQL_AF_MIN = 8;
	enum SQLINTEGER SQL_AF_SUM = 16;
	enum SQLINTEGER SQL_AF_DISTINCT = 32;
	enum SQLINTEGER SQL_AF_ALL = 64;
	enum SQLINTEGER SQL_AF_EVERY = 128;
	enum SQLINTEGER SQL_AF_ANY = 256;
	enum SQLINTEGER SQL_AF_STDEV_OP = 512;
	enum SQLINTEGER SQL_AF_STDEV_SAMP = 1024;
	enum SQLINTEGER SQL_AF_VAR_SAMP = 2048;
	enum SQLINTEGER SQL_AF_VAR_POP = 4096;
	enum SQLINTEGER SQL_AF_ARRAY_AGG = 8192;
	enum SQLINTEGER SQL_AF_COLLECT = 16384;
	enum SQLINTEGER SQL_AF_FUSION = 32768;
	enum SQLINTEGER SQL_AF_INTERSECTION = 65536;
	enum SQLINTEGER SQL_SC_SQL92_ENTRY = 1;
	enum SQLINTEGER SQL_SC_FIPS127_2_TRANSITIONAL = 2;
	enum SQLINTEGER SQL_SC_SQL92_INTERMEDIATE = 4;
	enum SQLINTEGER SQL_SC_SQL92_FULL = 8;
	enum SQLINTEGER SQL_DL_SQL92_DATE = 1;
	enum SQLINTEGER SQL_DL_SQL92_TIME = 2;
	enum SQLINTEGER SQL_DL_SQL92_TIMESTAMP = 4;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_YEAR = 8;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_MONTH = 16;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_DAY = 32;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_HOUR = 64;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_MINUTE = 128;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_SECOND = 256;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH = 512;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR = 1024;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE = 2048;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND = 4096;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE = 8192;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND = 16384;
	enum SQLINTEGER SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND = 32768;
	enum SQLINTEGER SQL_BRC_PROCEDURES = 1;
	enum SQLINTEGER SQL_BRC_EXPLICIT = 2;
	enum SQLINTEGER SQL_BRC_ROLLED_UP = 4;
	enum SQLINTEGER SQL_BS_SELECT_EXPLICIT = 1;
	enum SQLINTEGER SQL_BS_ROW_COUNT_EXPLICIT = 2;
	enum SQLINTEGER SQL_BS_SELECT_PROC = 4;
	enum SQLINTEGER SQL_BS_ROW_COUNT_PROC = 8;
	enum SQLINTEGER SQL_PARC_BATCH = 1;
	enum SQLINTEGER SQL_PARC_NO_BATCH = 2;
	enum SQLINTEGER SQL_PAS_BATCH = 1;
	enum SQLINTEGER SQL_PAS_NO_BATCH = 2;
	enum SQLINTEGER SQL_PAS_NO_SELECT = 3;
	enum SQLINTEGER SQL_IK_NONE = 0;
	enum SQLINTEGER SQL_IK_ASC = 1;
	enum SQLINTEGER SQL_IK_DESC = 2;
	auto SQL_IK_ALL()()
	{
		return SQL_IK_ASC | SQL_IK_DESC;
	}
	enum SQLINTEGER SQL_ISV_ASSERTIONS = 1;
	enum SQLINTEGER SQL_ISV_CHARACTER_SETS = 2;
	enum SQLINTEGER SQL_ISV_CHECK_CONSTRAINTS = 4;
	enum SQLINTEGER SQL_ISV_COLLATIONS = 8;
	enum SQLINTEGER SQL_ISV_COLUMN_DOMAIN_USAGE = 16;
	enum SQLINTEGER SQL_ISV_COLUMN_PRIVILEGES = 32;
	enum SQLINTEGER SQL_ISV_COLUMNS = 64;
	enum SQLINTEGER SQL_ISV_CONSTRAINT_COLUMN_USAGE = 128;
	enum SQLINTEGER SQL_ISV_CONSTRAINT_TABLE_USAGE = 256;
	enum SQLINTEGER SQL_ISV_DOMAIN_CONSTRAINTS = 512;
	enum SQLINTEGER SQL_ISV_DOMAINS = 1024;
	enum SQLINTEGER SQL_ISV_KEY_COLUMN_USAGE = 2048;
	enum SQLINTEGER SQL_ISV_REFERENTIAL_CONSTRAINTS = 4096;
	enum SQLINTEGER SQL_ISV_SCHEMATA = 8192;
	enum SQLINTEGER SQL_ISV_SQL_LANGUAGES = 16384;
	enum SQLINTEGER SQL_ISV_TABLE_CONSTRAINTS = 32768;
	enum SQLINTEGER SQL_ISV_TABLE_PRIVILEGES = 65536;
	enum SQLINTEGER SQL_ISV_TABLES = 131072;
	enum SQLINTEGER SQL_ISV_TRANSLATIONS = 262144;
	enum SQLINTEGER SQL_ISV_USAGE_PRIVILEGES = 524288;
	enum SQLINTEGER SQL_ISV_VIEW_COLUMN_USAGE = 1048576;
	enum SQLINTEGER SQL_ISV_VIEW_TABLE_USAGE = 2097152;
	enum SQLINTEGER SQL_ISV_VIEWS = 4194304;
	enum SQLINTEGER SQL_AD_CONSTRAINT_NAME_DEFINITION = 1;
	enum SQLINTEGER SQL_AD_ADD_DOMAIN_CONSTRAINT = 2;
	enum SQLINTEGER SQL_AD_DROP_DOMAIN_CONSTRAINT = 4;
	enum SQLINTEGER SQL_AD_ADD_DOMAIN_DEFAULT = 8;
	enum SQLINTEGER SQL_AD_DROP_DOMAIN_DEFAULT = 16;
	enum SQLINTEGER SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED = 32;
	enum SQLINTEGER SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE = 64;
	enum SQLINTEGER SQL_AD_ADD_CONSTRAINT_DEFERRABLE = 128;
	enum SQLINTEGER SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE = 256;
	enum SQLINTEGER SQL_CS_CREATE_SCHEMA = 1;
	enum SQLINTEGER SQL_CS_AUTHORIZATION = 2;
	enum SQLINTEGER SQL_CS_DEFAULT_CHARACTER_SET = 4;
	enum SQLINTEGER SQL_CTR_CREATE_TRANSLATION = 1;
	enum SQLINTEGER SQL_CA_CREATE_ASSERTION = 1;
	enum SQLINTEGER SQL_CA_CONSTRAINT_INITIALLY_DEFERRED = 16;
	enum SQLINTEGER SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE = 32;
	enum SQLINTEGER SQL_CA_CONSTRAINT_DEFERRABLE = 64;
	enum SQLINTEGER SQL_CA_CONSTRAINT_NON_DEFERRABLE = 128;
	enum SQLINTEGER SQL_CCS_CREATE_CHARACTER_SET = 1;
	enum SQLINTEGER SQL_CCS_COLLATE_CLAUSE = 2;
	enum SQLINTEGER SQL_CCS_LIMITED_COLLATION = 4;
	enum SQLINTEGER SQL_CCOL_CREATE_COLLATION = 1;
	enum SQLINTEGER SQL_CDO_CREATE_DOMAIN = 1;
	enum SQLINTEGER SQL_CDO_DEFAULT = 2;
	enum SQLINTEGER SQL_CDO_CONSTRAINT = 4;
	enum SQLINTEGER SQL_CDO_COLLATION = 8;
	enum SQLINTEGER SQL_CDO_CONSTRAINT_NAME_DEFINITION = 16;
	enum SQLINTEGER SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED = 32;
	enum SQLINTEGER SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE = 64;
	enum SQLINTEGER SQL_CDO_CONSTRAINT_DEFERRABLE = 128;
	enum SQLINTEGER SQL_CDO_CONSTRAINT_NON_DEFERRABLE = 256;
	enum SQLINTEGER SQL_CT_CREATE_TABLE = 1;
	enum SQLINTEGER SQL_CT_COMMIT_PRESERVE = 2;
	enum SQLINTEGER SQL_CT_COMMIT_DELETE = 4;
	enum SQLINTEGER SQL_CT_GLOBAL_TEMPORARY = 8;
	enum SQLINTEGER SQL_CT_LOCAL_TEMPORARY = 16;
	enum SQLINTEGER SQL_CT_CONSTRAINT_INITIALLY_DEFERRED = 32;
	enum SQLINTEGER SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE = 64;
	enum SQLINTEGER SQL_CT_CONSTRAINT_DEFERRABLE = 128;
	enum SQLINTEGER SQL_CT_CONSTRAINT_NON_DEFERRABLE = 256;
	enum SQLINTEGER SQL_CT_COLUMN_CONSTRAINT = 512;
	enum SQLINTEGER SQL_CT_COLUMN_DEFAULT = 1024;
	enum SQLINTEGER SQL_CT_COLUMN_COLLATION = 2048;
	enum SQLINTEGER SQL_CT_TABLE_CONSTRAINT = 4096;
	enum SQLINTEGER SQL_CT_CONSTRAINT_NAME_DEFINITION = 8192;
	enum SQLINTEGER SQL_DI_CREATE_INDEX = 1;
	enum SQLINTEGER SQL_DI_DROP_INDEX = 2;
	enum SQLINTEGER SQL_DC_DROP_COLLATION = 1;
	enum SQLINTEGER SQL_DD_DROP_DOMAIN = 1;
	enum SQLINTEGER SQL_DD_RESTRICT = 2;
	enum SQLINTEGER SQL_DD_CASCADE = 4;
	enum SQLINTEGER SQL_DS_DROP_SCHEMA = 1;
	enum SQLINTEGER SQL_DS_RESTRICT = 2;
	enum SQLINTEGER SQL_DS_CASCADE = 4;
	enum SQLINTEGER SQL_DCS_DROP_CHARACTER_SET = 1;
	enum SQLINTEGER SQL_DA_DROP_ASSERTION = 1;
	enum SQLINTEGER SQL_DT_DROP_TABLE = 1;
	enum SQLINTEGER SQL_DT_RESTRICT = 2;
	enum SQLINTEGER SQL_DT_CASCADE = 4;
	enum SQLINTEGER SQL_DTR_DROP_TRANSLATION = 1;
	enum SQLINTEGER SQL_DV_DROP_VIEW = 1;
	enum SQLINTEGER SQL_DV_RESTRICT = 2;
	enum SQLINTEGER SQL_DV_CASCADE = 4;
	enum SQLINTEGER SQL_IS_INSERT_LITERALS = 1;
	enum SQLINTEGER SQL_IS_INSERT_SEARCHED = 2;
	enum SQLINTEGER SQL_IS_SELECT_INTO = 4;
	enum SQLUINTEGER SQL_OIC_CORE = 1u;
	enum SQLUINTEGER SQL_OIC_LEVEL1 = 2u;
	enum SQLUINTEGER SQL_OIC_LEVEL2 = 3u;
	enum SQLINTEGER SQL_SFKD_CASCADE = 1;
	enum SQLINTEGER SQL_SFKD_NO_ACTION = 2;
	enum SQLINTEGER SQL_SFKD_SET_DEFAULT = 4;
	enum SQLINTEGER SQL_SFKD_SET_NULL = 8;
	enum SQLINTEGER SQL_SFKU_CASCADE = 1;
	enum SQLINTEGER SQL_SFKU_NO_ACTION = 2;
	enum SQLINTEGER SQL_SFKU_SET_DEFAULT = 4;
	enum SQLINTEGER SQL_SFKU_SET_NULL = 8;
	enum SQLINTEGER SQL_SG_USAGE_ON_DOMAIN = 1;
	enum SQLINTEGER SQL_SG_USAGE_ON_CHARACTER_SET = 2;
	enum SQLINTEGER SQL_SG_USAGE_ON_COLLATION = 4;
	enum SQLINTEGER SQL_SG_USAGE_ON_TRANSLATION = 8;
	enum SQLINTEGER SQL_SG_WITH_GRANT_OPTION = 16;
	enum SQLINTEGER SQL_SG_DELETE_TABLE = 32;
	enum SQLINTEGER SQL_SG_INSERT_TABLE = 64;
	enum SQLINTEGER SQL_SG_INSERT_COLUMN = 128;
	enum SQLINTEGER SQL_SG_REFERENCES_TABLE = 256;
	enum SQLINTEGER SQL_SG_REFERENCES_COLUMN = 512;
	enum SQLINTEGER SQL_SG_SELECT_TABLE = 1024;
	enum SQLINTEGER SQL_SG_UPDATE_TABLE = 2048;
	enum SQLINTEGER SQL_SG_UPDATE_COLUMN = 4096;
	enum SQLINTEGER SQL_SP_EXISTS = 1;
	enum SQLINTEGER SQL_SP_ISNOTNULL = 2;
	enum SQLINTEGER SQL_SP_ISNULL = 4;
	enum SQLINTEGER SQL_SP_MATCH_FULL = 8;
	enum SQLINTEGER SQL_SP_MATCH_PARTIAL = 16;
	enum SQLINTEGER SQL_SP_MATCH_UNIQUE_FULL = 32;
	enum SQLINTEGER SQL_SP_MATCH_UNIQUE_PARTIAL = 64;
	enum SQLINTEGER SQL_SP_OVERLAPS = 128;
	enum SQLINTEGER SQL_SP_UNIQUE = 256;
	enum SQLINTEGER SQL_SP_LIKE = 512;
	enum SQLINTEGER SQL_SP_IN = 1024;
	enum SQLINTEGER SQL_SP_BETWEEN = 2048;
	enum SQLINTEGER SQL_SP_COMPARISON = 4096;
	enum SQLINTEGER SQL_SP_QUANTIFIED_COMPARISON = 8192;
	enum SQLINTEGER SQL_SRJO_CORRESPONDING_CLAUSE = 1;
	enum SQLINTEGER SQL_SRJO_CROSS_JOIN = 2;
	enum SQLINTEGER SQL_SRJO_EXCEPT_JOIN = 4;
	enum SQLINTEGER SQL_SRJO_FULL_OUTER_JOIN = 8;
	enum SQLINTEGER SQL_SRJO_INNER_JOIN = 16;
	enum SQLINTEGER SQL_SRJO_INTERSECT_JOIN = 32;
	enum SQLINTEGER SQL_SRJO_LEFT_OUTER_JOIN = 64;
	enum SQLINTEGER SQL_SRJO_NATURAL_JOIN = 128;
	enum SQLINTEGER SQL_SRJO_RIGHT_OUTER_JOIN = 256;
	enum SQLINTEGER SQL_SRJO_UNION_JOIN = 512;
	enum SQLINTEGER SQL_SR_USAGE_ON_DOMAIN = 1;
	enum SQLINTEGER SQL_SR_USAGE_ON_CHARACTER_SET = 2;
	enum SQLINTEGER SQL_SR_USAGE_ON_COLLATION = 4;
	enum SQLINTEGER SQL_SR_USAGE_ON_TRANSLATION = 8;
	enum SQLINTEGER SQL_SR_GRANT_OPTION_FOR = 16;
	enum SQLINTEGER SQL_SR_CASCADE = 32;
	enum SQLINTEGER SQL_SR_RESTRICT = 64;
	enum SQLINTEGER SQL_SR_DELETE_TABLE = 128;
	enum SQLINTEGER SQL_SR_INSERT_TABLE = 256;
	enum SQLINTEGER SQL_SR_INSERT_COLUMN = 512;
	enum SQLINTEGER SQL_SR_REFERENCES_TABLE = 1024;
	enum SQLINTEGER SQL_SR_REFERENCES_COLUMN = 2048;
	enum SQLINTEGER SQL_SR_SELECT_TABLE = 4096;
	enum SQLINTEGER SQL_SR_UPDATE_TABLE = 8192;
	enum SQLINTEGER SQL_SR_UPDATE_COLUMN = 16384;
	enum SQLINTEGER SQL_SRVC_VALUE_EXPRESSION = 1;
	enum SQLINTEGER SQL_SRVC_NULL = 2;
	enum SQLINTEGER SQL_SRVC_DEFAULT = 4;
	enum SQLINTEGER SQL_SRVC_ROW_SUBQUERY = 8;
	enum SQLINTEGER SQL_SVE_CASE = 1;
	enum SQLINTEGER SQL_SVE_CAST = 2;
	enum SQLINTEGER SQL_SVE_COALESCE = 4;
	enum SQLINTEGER SQL_SVE_NULLIF = 8;
	enum SQLINTEGER SQL_SCC_XOPEN_CLI_VERSION1 = 1;
	enum SQLINTEGER SQL_SCC_ISO92_CLI = 2;
	enum SQLINTEGER SQL_DRIVER_AWARE_POOLING_NOT_CAPABLE = 0;
	enum SQLINTEGER SQL_DRIVER_AWARE_POOLING_CAPABLE = 1;
	enum SQLINTEGER SQL_DTC_ENLIST_EXPENSIVE = 1;
	enum SQLINTEGER SQL_DTC_UNENLIST_EXPENSIVE = 2;
	enum SQLINTEGER SQL_ASYNC_DBC_NOT_CAPABLE = 0;
	enum SQLINTEGER SQL_ASYNC_DBC_CAPABLE = 1;
	enum SQLINTEGER SQL_LC_NONE = 0;
	enum SQLINTEGER SQL_LC_TAKE = 1;
	enum SQLINTEGER SQL_LC_SKIP = 3;
	enum SQLINTEGER SQL_RC_NONE = 0;
	enum SQLINTEGER SQL_RC_INSERT_SINGLE_ROWID = 1;
	auto SQL_RC_INSERT_SINGLE_ANY()()
	{
		return 2 | SQL_RC_INSERT_SINGLE_ROWID;
	}
	auto SQL_RC_INSERT_MULTIPLE_ROWID()()
	{
		return 4 | SQL_RC_INSERT_SINGLE_ROWID;
	}
	auto SQL_RC_INSERT_MULTIPLE_ANY()()
	{
		return 8 | SQL_RC_INSERT_MULTIPLE_ROWID | SQL_RC_INSERT_SINGLE_ANY;
	}
	enum SQLINTEGER SQL_RC_INSERT_SELECT_ROWID = 16;
	auto SQL_RC_INSERT_SELECT_ANY()()
	{
		return 32 | SQL_RC_INSERT_SELECT_ROWID;
	}
	enum SQLINTEGER SQL_RC_UPDATE_ROWID = 64;
	auto SQL_RC_UPDATE_ANY()()
	{
		return 128 | SQL_RC_UPDATE_ROWID;
	}
	enum SQLINTEGER SQL_RC_DELETE_ROWID = 256;
	auto SQL_RC_DELETE_ANY()()
	{
		return 512 | SQL_RC_DELETE_ROWID;
	}
	enum SQLINTEGER SQL_RC_SELECT_INTO_ROWID = 1024;
	auto SQL_RC_SELECT_INTO_ANY()()
	{
		return 2048 | SQL_RC_SELECT_INTO_ROWID;
	}
	enum SQLINTEGER SQL_FC_NONE = 0;
	enum SQLINTEGER SQL_FC_JSON = 1;
	enum SQLINTEGER SQL_FC_JSON_BINARY = 2;
	enum SQLINTEGER SQL_FETCH_FIRST_USER = 31;
	enum SQLINTEGER SQL_FETCH_FIRST_SYSTEM = 32;
	enum SQLINTEGER SQL_ENTIRE_ROWSET = 0;
	enum SQLINTEGER SQL_POSITION = 0;
	enum SQLINTEGER SQL_REFRESH = 1;
	enum SQLINTEGER SQL_UPDATE = 2;
	enum SQLINTEGER SQL_DELETE = 3;
	enum SQLINTEGER SQL_ADD = 4;
	enum SQLINTEGER SQL_UPDATE_BY_BOOKMARK = 5;
	enum SQLINTEGER SQL_DELETE_BY_BOOKMARK = 6;
	enum SQLINTEGER SQL_FETCH_BY_BOOKMARK = 7;
	enum SQLINTEGER SQL_LOCK_NO_CHANGE = 0;
	enum SQLINTEGER SQL_LOCK_EXCLUSIVE = 1;
	enum SQLINTEGER SQL_LOCK_UNLOCK = 2;
	auto SQL_POSITION_TO(__MP4, __MP5)(__MP4 hstmt, __MP5 irow)
	{
		return SQLSetPos(hstmt, irow, SQL_POSITION, SQL_LOCK_NO_CHANGE);
	}
	auto SQL_LOCK_RECORD(__MP6, __MP7, __MP8)(__MP6 hstmt, __MP7 irow, __MP8 fLock)
	{
		return SQLSetPos(hstmt, irow, SQL_POSITION, fLock);
	}
	auto SQL_REFRESH_RECORD(__MP9, __MP10, __MP11)(__MP9 hstmt, __MP10 irow, __MP11 fLock)
	{
		return SQLSetPos(hstmt, irow, SQL_REFRESH, fLock);
	}
	auto SQL_UPDATE_RECORD(__MP12, __MP13)(__MP12 hstmt, __MP13 irow)
	{
		return SQLSetPos(hstmt, irow, SQL_UPDATE, SQL_LOCK_NO_CHANGE);
	}
	auto SQL_DELETE_RECORD(__MP14, __MP15)(__MP14 hstmt, __MP15 irow)
	{
		return SQLSetPos(hstmt, irow, SQL_DELETE, SQL_LOCK_NO_CHANGE);
	}
	auto SQL_ADD_RECORD(__MP16, __MP17)(__MP16 hstmt, __MP17 irow)
	{
		return SQLSetPos(hstmt, irow, SQL_ADD, SQL_LOCK_NO_CHANGE);
	}
	enum SQLINTEGER SQL_BEST_ROWID = 1;
	enum SQLINTEGER SQL_ROWVER = 2;
	enum SQLINTEGER SQL_PC_NOT_PSEUDO = 1;
	enum SQLINTEGER SQL_QUICK = 0;
	enum SQLINTEGER SQL_ENSURE = 1;
	enum SQLINTEGER SQL_TABLE_STAT = 0;
	enum SQL_ALL_CATALOGS = "%";
	enum SQL_ALL_SCHEMAS = "%";
	enum SQL_ALL_TABLE_TYPES = "%";
	enum SQLINTEGER SQL_DRIVER_NOPROMPT = 0;
	enum SQLINTEGER SQL_DRIVER_COMPLETE = 1;
	enum SQLINTEGER SQL_DRIVER_PROMPT = 2;
	enum SQLINTEGER SQL_DRIVER_COMPLETE_REQUIRED = 3;
	enum SQLINTEGER SQL_FETCH_BOOKMARK = 8;
	enum SQLINTEGER SQL_ROW_SUCCESS = 0;
	enum SQLINTEGER SQL_ROW_DELETED = 1;
	enum SQLINTEGER SQL_ROW_UPDATED = 2;
	enum SQLINTEGER SQL_ROW_NOROW = 3;
	enum SQLINTEGER SQL_ROW_ADDED = 4;
	enum SQLINTEGER SQL_ROW_ERROR = 5;
	enum SQLINTEGER SQL_ROW_SUCCESS_WITH_INFO = 6;
	enum SQLINTEGER SQL_ROW_PROCEED = 0;
	enum SQLINTEGER SQL_ROW_IGNORE = 1;
	enum SQLINTEGER SQL_PARAM_SUCCESS = 0;
	enum SQLINTEGER SQL_PARAM_SUCCESS_WITH_INFO = 6;
	enum SQLINTEGER SQL_PARAM_ERROR = 5;
	enum SQLINTEGER SQL_PARAM_UNUSED = 7;
	enum SQLINTEGER SQL_PARAM_DIAG_UNAVAILABLE = 1;
	enum SQLINTEGER SQL_PARAM_PROCEED = 0;
	enum SQLINTEGER SQL_PARAM_IGNORE = 1;
	enum SQLINTEGER SQL_CASCADE = 0;
	enum SQLINTEGER SQL_RESTRICT = 1;
	enum SQLINTEGER SQL_SET_NULL = 2;
	enum SQLINTEGER SQL_NO_ACTION = 3;
	enum SQLINTEGER SQL_SET_DEFAULT = 4;
	enum SQLINTEGER SQL_INITIALLY_DEFERRED = 5;
	enum SQLINTEGER SQL_INITIALLY_IMMEDIATE = 6;
	enum SQLINTEGER SQL_NOT_DEFERRABLE = 7;
	enum SQLINTEGER SQL_PARAM_TYPE_UNKNOWN = 0;
	enum SQLINTEGER SQL_PARAM_INPUT = 1;
	enum SQLINTEGER SQL_PARAM_INPUT_OUTPUT = 2;
	enum SQLINTEGER SQL_RESULT_COL = 3;
	enum SQLINTEGER SQL_PARAM_OUTPUT = 4;
	enum SQLINTEGER SQL_RETURN_VALUE = 5;
	enum SQLINTEGER SQL_PARAM_INPUT_OUTPUT_STREAM = 8;
	enum SQLINTEGER SQL_PARAM_OUTPUT_STREAM = 16;
	enum SQLINTEGER SQL_PT_UNKNOWN = 0;
	enum SQLINTEGER SQL_PT_PROCEDURE = 1;
	enum SQLINTEGER SQL_PT_FUNCTION = 2;
	auto SQL_WCHAR()()
	{
		return -8;
	}
	auto SQL_WVARCHAR()()
	{
		return -9;
	}
	auto SQL_WLONGVARCHAR()()
	{
		return -10;
	}
	enum SQLINTEGER SQL_SQLSTATE_SIZEW = 10;
}
