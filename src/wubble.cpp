#include <Rcpp.h>
#include <string>

using namespace Rcpp;

// CREATING VECTORS
// List rcpp_hello_world() {
//
//     CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
//     NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
//     List z            = List::create( x, y ) ;
//
//     return z ;
// }

// USING R FUNCTIONS
// CharacterVector rcpp_https(CharacterVector url) {
//   Function f("paste0");
//   CharacterVector z = f("https://", url);
//   return z;
// }


// [[Rcpp::export]]
List path_source(std::string path) {
  List source_spec = List::create(
    path,
    Named("skip") = "0",
    Named("skip_empty_rows") = "TRUE",
    Named("comment") = "");
  Function f("source_path");
  return f(source_spec);
}



// [[Rcpp::export]]
DataFrame dots_example(List data, List dots){
  int n_data = data.size();
  int n_dots = dots.size();
  int n = n_data + n_dots;
  List lst = List(n);
  CharacterVector nms = CharacterVector(n);
  CharacterVector names_data = data.names();
  CharacterVector names_dots = dots.names();

  for (int i=0; i < n_data; i++) {
    lst[i] = data[i];
    nms[i] = names_data[i];
  }
  for (int i=0; i < n_dots; i++) {
    lst[i + n_data] = dots[i];
    nms[i + n_data] = names_dots[i];
  }
  lst.names() = nms;
  DataFrame df = Rcpp::as<Rcpp::DataFrame>(lst);
  df.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");

  return df;
}

// [[Rcpp::export]]
List lst_tbl(List lst){
  Function lengths("lengths");
  Function rep("rep");
  IntegerVector lens = lengths(lst);
  int n = max(lens);
  int lst_n = lst.size();
  for (int i=0; i < lst_n; i++) {
    if (n > 1) {
      if (lens[i] == 1) {
        lst[i] = rep(lst[i], n);
      }
    }
  }
// //  CharacterVector row_names = lst.attr("row.names");
// //  int n = row_names.size();
// //  lst.push_front(row_names, "row_names");
  lst.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
  lst.attr("class") = CharacterVector::create("wbl_df", "wbl", "tbl_df", "tbl", "data.frame");
  return lst;
}


// [[Rcpp::export]]
DataFrame add_data(DataFrame data, List lst) {
  CharacterVector nms = lst.names();
  int n = lst.size();
  for (int i=0; i < n; i++) {
    data.push_front(Rcpp::as<Rcpp::CharacterVector>(lst[i]),
      Rcpp::as<std::string>(nms[i]));
  }
  return data;
}
