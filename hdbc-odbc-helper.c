#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__)
#define ON_WINDOWS
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

#ifdef ON_WINDOWS
#define odbc_sync_val_compare_and_swap(ptr, oldval, newval) InterlockedCompareExchange((volatile LONG*)(ptr), (newval), (oldval))
#define odbc_atomic_increment(ptr) InterlockedIncrement((volatile LONG*)(ptr))
#define odbc_atomic_decrement(ptr) InterlockedDecrement((volatile LONG*)(ptr))
#else
#define odbc_sync_val_compare_and_swap(ptr, oldval, newval) __sync_val_compare_and_swap((ptr), (oldval), (newval))
#define odbc_atomic_increment(ptr) __sync_add_and_fetch((ptr), 1)
#define odbc_atomic_decrement(ptr) __sync_sub_and_fetch((ptr), 1)
#endif

/* Things can't finalize more than once.
We'd like to let people call them from the app.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */

void dbc_conditional_finalizer(finalizeonce *conn, int refCount);

finalizeonce *wrapobjodbc(void *obj, finalizeonce *parentobj) {
  // parentobj might not get finalized during running this function
  // from other thread because of the way it is called from Haskell
  // side.
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
  if (parentobj != NULL) {
    odbc_atomic_increment(&parentobj->refcount);
  }
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
  int isFinalized = odbc_sync_val_compare_and_swap(&res->isfinalized, 0, 1);
  if (isFinalized)
    return;
  if (!res->encapobj)
    return;
  // Microsoft SQL Server driver might deadlock if calling SQLCloseCursor on a statement that is in the
  // process of fetching data via network. So we cancel it first.
  SQLCancel((SQLHSTMT) (res->encapobj));
  SQLCloseCursor((SQLHSTMT) (res->encapobj));
  SQLFreeHandle(SQL_HANDLE_STMT, (SQLHANDLE) (res->encapobj));
  res->encapobj = NULL;
}

void sqlFreeHandleSth_finalizer(finalizeonce *res) {
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nFinalizer cleanup of sth %p requested: %d\n",
          res->encapobj, res->isfinalized);
#endif
  sqlFreeHandleSth_app(res);

  /* Not really important since this is never a parent */
  odbc_atomic_decrement(&res->refcount);

  int parentRefCount = odbc_atomic_decrement(&res->parent->refcount);
  dbc_conditional_finalizer(res->parent, parentRefCount);
  free(res);
}

SQLRETURN sqlFreeHandleDbc_app(finalizeonce *res) {
  SQLRETURN retval;
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nApp cleanup of dbc %p requested: %d\n",
          res->encapobj, res->isfinalized);
#endif
  int isFinalized = odbc_sync_val_compare_and_swap(&res->isfinalized, 0, 1);
  if (isFinalized)
    return 0;
  if (!res->encapobj)
    return SQL_SUCCESS;

  retval = SQLDisconnect((SQLHDBC) (res->encapobj));
  if (SQL_SUCCEEDED(retval)) {
    SQLFreeHandle(SQL_HANDLE_DBC, (SQLHANDLE) (res->encapobj));
    SQLFreeHandle(SQL_HANDLE_ENV, (SQLHANDLE) (res->extrainfo));
    res->encapobj = NULL;
  }
  return retval;
}

void sqlFreeHandleDbc_finalizer(finalizeonce *res) {
#ifdef HDBC_DEBUG
  fprintf(stderr, "\nFinalizer cleanup of dbc %p requested: %d\n",
          res->encapobj, res->isfinalized);
#endif
  int refCount = odbc_atomic_decrement(&res->refcount);
  dbc_conditional_finalizer(res, refCount);
}

void dbc_conditional_finalizer(finalizeonce *res, int refcount) {
  if (refcount < 1) {
  /* Don't use sqlFreeHandleDbc_app here, because we want to clear it out
     regardless of the success or failues of SQLDisconnect. */
    int isFinalized = odbc_sync_val_compare_and_swap(&res->isfinalized, 0, 1);
    if (!isFinalized && res->encapobj) {
      SQLDisconnect((SQLHDBC) (res->encapobj));
      SQLFreeHandle(SQL_HANDLE_DBC, (SQLHANDLE) (res->encapobj));
      SQLFreeHandle(SQL_HANDLE_ENV, (SQLHANDLE) (res->extrainfo));
      res->encapobj = NULL;
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
