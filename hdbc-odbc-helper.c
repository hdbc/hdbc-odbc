#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__)
#include <windows.h>
#include <winnt.h>
#endif
#include <sql.h>
#include <sqlext.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdbc-odbc-helper.h"

SQLLEN nullDataHDBC = SQL_NULL_DATA;

int sqlSucceeded(SQLRETURN ret) {
  return SQL_SUCCEEDED(ret);
}

/* Things can't finalize more than once.  
We'd like to let people call them from the app.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */

void dbc_conditional_finalizer(finalizeonce *conn);

finalizeonce *wrapobjodbc(void *obj, finalizeonce *parentobj) {
  finalizeonce *newobj;
  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "\nHDBC: could not allocate wrapper!\n");
    return NULL;
  }
  newobj->isfinalized = 0;
  newobj -> refcount = 1;
  newobj->encapobj = obj;
  newobj->extrainfo = NULL;
  newobj->parent = parentobj;
  if (parentobj != NULL)
    (parentobj->refcount)++;
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nWrapped %p at %p\n", obj, newobj);
#endif
  return newobj;
}

finalizeonce *wrapobjodbc_extra(void *obj, void *extra, finalizeonce *parentobj) {
  finalizeonce *newobj = wrapobjodbc(obj, parentobj);
  if (newobj != NULL)
    newobj->extrainfo = extra;
  return newobj;
}
  
void sqlFreeHandleSth_app(finalizeonce *res) {
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nApp cleanup of sth %p requested: %d\n", 
          res->encapobj, res->isfinalized);
#endif
  if (res->isfinalized)
    return;
  SQLCloseCursor((SQLHSTMT) (res->encapobj));
  SQLFreeHandle(SQL_HANDLE_STMT, (SQLHANDLE) (res->encapobj));
  res->isfinalized = 1;
  res->encapobj = NULL;
}

void sqlFreeHandleSth_finalizer(finalizeonce *res) {
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nFinalizer cleanup of sth %p requested: %d\n", 
          res->encapobj, res->isfinalized);
#endif
  sqlFreeHandleSth_app(res);
  (res->refcount)--;            /* Not really important since this is never a 
                                   parent */
  (res->parent->refcount)--;
  dbc_conditional_finalizer(res->parent);
  free(res);
}

SQLRETURN sqlFreeHandleDbc_app(finalizeonce *res) {
  SQLRETURN retval;
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nApp cleanup of dbc %p requested: %d\n", 
          res->encapobj, res->isfinalized);
#endif
  if (res->isfinalized)
    return 0;
  retval = SQLDisconnect((SQLHDBC) (res->encapobj));
  if (SQL_SUCCEEDED(retval)) {
    SQLFreeHandle(SQL_HANDLE_DBC, (SQLHANDLE) (res->encapobj));
    SQLFreeHandle(SQL_HANDLE_ENV, (SQLHANDLE) (res->extrainfo));
    res->isfinalized = 1;
    res->encapobj = NULL;
  }
  return retval;
}

void sqlFreeHandleDbc_finalizer(finalizeonce *res) {
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nFinalizer cleanup of dbc %p requested: %d\n", 
          res->encapobj, res->isfinalized);
#endif
  (res->refcount)--;
  dbc_conditional_finalizer(res);
}

void dbc_conditional_finalizer(finalizeonce *res) {
  if (res->refcount < 1) {
  /* Don't use sqlFreeHandleDbc_app here, because we want to clear it out
     regardless of the success or failues of SQLDisconnect. */
    if (! (res->isfinalized)) {
      SQLDisconnect((SQLHDBC) (res->encapobj));
      SQLFreeHandle(SQL_HANDLE_DBC, (SQLHANDLE) (res->encapobj));
      SQLFreeHandle(SQL_HANDLE_ENV, (SQLHANDLE) (res->extrainfo));
      res->encapobj = NULL;
      res->isfinalized = 1;
    }
    free(res);
  }
}

void *getSqlOvOdbc3(void) {
  return (void *)SQL_OV_ODBC3;
}

SQLRETURN disableAutoCommit(SQLHDBC conn) {
  return SQLSetConnectAttr(conn, SQL_ATTR_AUTOCOMMIT, 
                           (SQLPOINTER) SQL_AUTOCOMMIT_OFF,
                           SQL_IS_UINTEGER);
}

SQLRETURN simpleSqlTables(SQLHSTMT stmt) {
  return SQLTables(stmt, NULL, 0, NULL, 0, "%", 1, "TABLE", 5);
}

SQLRETURN simpleSqlColumns(SQLHSTMT stmt, SQLCHAR *tablename, 
                           SQLSMALLINT tnlen) {
  return SQLColumns(stmt, NULL, 0, NULL, 0, tablename, tnlen, "%", 1);
}
