#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP cctools_eval_mvkde(SEXP, SEXP, SEXP);
extern SEXP cctools_lcv_mvkde_disc(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"cctools_eval_mvkde",     (DL_FUNC) &cctools_eval_mvkde,     3},
    {"cctools_lcv_mvkde_disc", (DL_FUNC) &cctools_lcv_mvkde_disc, 3},
    {NULL, NULL, 0}
};

void R_init_cctools(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
