#include <Rcpp.h>
#include <string>

using namespace Rcpp;


// [[Rcpp::export]]
List wibble_call(List lst){
  // Import R functions
  Function lengths("lengths");
  Function rep("rep");

  // Size params
  IntegerVector lens = lengths(lst);
  int n = max(lens);
  if (n == 0)
    return DataFrame();
  int lst_n = lst.size();

  // If empty element(s)
  int n0 = min(lens);
  if (n0 == 0) {
    for (int i=0; i < lst_n; i++) {
      if (lens[i] == 0)
        lst[i] = rep(NA_LOGICAL, n);
    }
  }

  // If single value element(s)
  IntegerVector lens2 = lengths(lst);
  int n1 = min(lens2);
  if (n > 1) {
    if (n1 == 1) {
      for (int i=0; i < lst_n; i++) {
        if (lens[i] == 1)
          lst[i] = rep(lst[i], n);
      }
    }
  }

  // Make row_names variable (if row.names exist)
  if (lst.hasAttribute("row.names")) {
    lst.push_front(
      Rcpp::as<Rcpp::CharacterVector>(lst.attr("row.names")),
      "row_names");
  }

  // Set/override row names
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);

  // Assign class
  lst.attr("class") = CharacterVector::create(
    "wbl_df", "wbl", "tbl_df", "tbl", "data.frame");
  return lst;
}
