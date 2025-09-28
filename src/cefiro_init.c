#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern void calculate_shear_c(int *n, double *ws1, double *ws2, double *dir,
                              double *hl, double *hh,
                              double *records, double *wsl, double *wsh,
                              double *shear);

extern void calculate_coverage_c(int *n, int *n_unique_year_month,
                                 int *year, int *month,
                                 double *signal, double *coverage);

static const R_CMethodDef cMethods[] = {
  {"calculate_shear_c", (DL_FUNC) &calculate_shear_c, 10},
  {"calculate_coverage_c", (DL_FUNC) &calculate_coverage_c, 6},
  {NULL, NULL, 0}
};

void R_init_cefiro(DllInfo *dll)
{
  R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
