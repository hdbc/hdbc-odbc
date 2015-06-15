#ifndef _HDBC_ODBC_CBITS_HDBC_ODBC_HELPER_H
#define _HDBC_ODBC_CBITS_HDBC_ODBC_HELPER_H

#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__)
#include <windows.h>
#include <winnt.h>
#endif

#include <sql.h>

int sqlSucceeded(SQLRETURN ret);

SQLLEN nullDataHDBC;
void *getSqlOvOdbc3(void);

SQLRETURN simpleSqlTables(SQLHSTMT stmt);
SQLRETURN simpleSqlColumns(SQLHSTMT stmt, SQLCHAR *tablename,
                                  SQLSMALLINT tnlen);

#endif /* _HDBC_ODBC_CBITS_HDBC_ODBC_HELPER_H */
