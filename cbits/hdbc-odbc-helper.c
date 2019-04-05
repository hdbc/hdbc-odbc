#include "hdbc-odbc-helper.h"
#include <sqlext.h>
#include <stdio.h>
#include <stdlib.h>

SQLLEN nullDataHDBC = SQL_NULL_DATA;

int sqlSucceeded(SQLRETURN ret) {
  return SQL_SUCCEEDED(ret);
}

void *getSqlOvOdbc3(void) {
  return (void *)SQL_OV_ODBC3;
}

SQLRETURN simpleSqlTables(SQLHSTMT stmt) {
  return SQLTables(stmt, NULL, 0, NULL, 0, (SQLCHAR *)"%", 1, (SQLCHAR *)"TABLE", 5);
}

SQLRETURN simpleSqlColumns(SQLHSTMT stmt, SQLCHAR *tablename,
                           SQLSMALLINT tnlen) {
  return SQLColumns(stmt, NULL, 0, NULL, 0, tablename, tnlen, (SQLCHAR *)"%", 1);
}
