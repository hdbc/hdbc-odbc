#include <sql.h>

extern int sqlSucceeded(SQLRETURN ret);
extern SQLRETURN sqlFreeHandleEnv(SQLHANDLE hdl);

typedef struct TAG_finalizeonce {
  void *encapobj;
  int isfinalized;
  void *extrainfo;
} finalizeonce;

extern finalizeonce *wrapobj(void *obj);
extern finalizeonce *wrapobj_extra(void *obj, void *extra);

extern void sqlFreeHandleEnv_app(finalizeonce *res);
extern void sqlFreeHandleEnv_finalizer(finalizeonce *res);

extern SQLRETURN sqlFreeHandleDbc_app(finalizeonce *res);
extern void sqlFreeHandleDbc_finalizer(finalizeonce *res);

extern void sqlFreeHandleSth_app(finalizeonce *res);
extern void sqlFreeHandleSth_finalizer(finalizeonce *res);

extern SQLINTEGER nullData;
extern void *getSqlOvOdbc3(void);

extern SQLRETURN enableAutoCommit(SQLHDBC conn);
