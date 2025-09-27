#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern void calculate_shear_c(int *n, double *ws1, double *ws2, double *dir,
                              double *hl, double *hh,
                              double *records, double *wsl, double *wsh,
                              double *shear);

static const R_CMethodDef cMethods[] = {
  {"calculate_shear_c", (DL_FUNC) &calculate_shear_c, 10},
  {NULL, NULL, 0}
};

void R_init_cefiro(DllInfo *dll)
{
  R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
