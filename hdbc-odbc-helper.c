#include <sql.h>
#include <sqlext.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdbc-odbc-helper.h"

SQLINTEGER nullData = SQL_NULL_DATA;

int sqlSucceeded(SQLRETURN ret) {
  return SQL_SUCCEEDED(ret);
}

/*
SQLRETURN sqlFreeHandleEnv(SQLHANDLE hdl) {
  return SQLFreeHandle(SQL_HANDLE_ENV, hdl);
}
*/


/* Things can't finalize more than once.  
We'd like to let people call them from the app.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */

finalizeonce *wrapobj(void *obj) {
  finalizeonce *newobj;
  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "HDBC: could not allocate wrapper!\n");
    return NULL;
  }
  newobj->isfinalized = 0;
  newobj->encapobj = obj;
  return newobj;
}
  
void sqlFreeHandleSth_app(finalizeonce *res) {
  if (res->isfinalized)
    return;
  SQLCloseCursor((SQLHSTMT) (res->encapobj));
  SQLFreeHandle(SQL_HANDLE_STMT, (SQLHANDLE) (res->encapobj));
  res->isfinalized = 1;
}

void sqlFreeHandleSth_finalizer(finalizeonce *res) {
  sqlFreeHandleSth_app(res);
  free(res);
}

void *getSqlOvOdbc3(void) {
  return (void *)SQL_OV_ODBC3;
}
