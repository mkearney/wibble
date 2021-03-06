// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// read_file_call
CharacterVector read_file_call(CharacterVector path);
RcppExport SEXP _wibble_read_file_call(SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(read_file_call(path));
    return rcpp_result_gen;
END_RCPP
}
// tokenize_wrd_call
Rcpp::List tokenize_wrd_call(std::vector<std::string> strings);
RcppExport SEXP _wibble_tokenize_wrd_call(SEXP stringsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type strings(stringsSEXP);
    rcpp_result_gen = Rcpp::wrap(tokenize_wrd_call(strings));
    return rcpp_result_gen;
END_RCPP
}
// tokenize_chr_call
Rcpp::List tokenize_chr_call(std::vector<std::string> strings);
RcppExport SEXP _wibble_tokenize_chr_call(SEXP stringsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type strings(stringsSEXP);
    rcpp_result_gen = Rcpp::wrap(tokenize_chr_call(strings));
    return rcpp_result_gen;
END_RCPP
}
// wabble_call
CharacterVector wabble_call(std::string url, List query, std::string path);
RcppExport SEXP _wibble_wabble_call(SEXP urlSEXP, SEXP querySEXP, SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type url(urlSEXP);
    Rcpp::traits::input_parameter< List >::type query(querySEXP);
    Rcpp::traits::input_parameter< std::string >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(wabble_call(url, query, path));
    return rcpp_result_gen;
END_RCPP
}
// api_calls
CharacterVector api_calls(CharacterVector url, CharacterVector query, CharacterVector value, CharacterVector path);
RcppExport SEXP _wibble_api_calls(SEXP urlSEXP, SEXP querySEXP, SEXP valueSEXP, SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type url(urlSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type query(querySEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type value(valueSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(api_calls(url, query, value, path));
    return rcpp_result_gen;
END_RCPP
}
// webble_call
CharacterVector webble_call(std::string url);
RcppExport SEXP _wibble_webble_call(SEXP urlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type url(urlSEXP);
    rcpp_result_gen = Rcpp::wrap(webble_call(url));
    return rcpp_result_gen;
END_RCPP
}
// wibble_call
List wibble_call(List lst);
RcppExport SEXP _wibble_wibble_call(SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(wibble_call(lst));
    return rcpp_result_gen;
END_RCPP
}
// path_source
List path_source(std::string path);
RcppExport SEXP _wibble_path_source(SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(path_source(path));
    return rcpp_result_gen;
END_RCPP
}
// dots_example
DataFrame dots_example(List data, List dots);
RcppExport SEXP _wibble_dots_example(SEXP dataSEXP, SEXP dotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type data(dataSEXP);
    Rcpp::traits::input_parameter< List >::type dots(dotsSEXP);
    rcpp_result_gen = Rcpp::wrap(dots_example(data, dots));
    return rcpp_result_gen;
END_RCPP
}
// add_data
DataFrame add_data(DataFrame data, List lst);
RcppExport SEXP _wibble_add_data(SEXP dataSEXP, SEXP lstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< List >::type lst(lstSEXP);
    rcpp_result_gen = Rcpp::wrap(add_data(data, lst));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wibble_read_file_call", (DL_FUNC) &_wibble_read_file_call, 1},
    {"_wibble_tokenize_wrd_call", (DL_FUNC) &_wibble_tokenize_wrd_call, 1},
    {"_wibble_tokenize_chr_call", (DL_FUNC) &_wibble_tokenize_chr_call, 1},
    {"_wibble_wabble_call", (DL_FUNC) &_wibble_wabble_call, 3},
    {"_wibble_api_calls", (DL_FUNC) &_wibble_api_calls, 4},
    {"_wibble_webble_call", (DL_FUNC) &_wibble_webble_call, 1},
    {"_wibble_wibble_call", (DL_FUNC) &_wibble_wibble_call, 1},
    {"_wibble_path_source", (DL_FUNC) &_wibble_path_source, 1},
    {"_wibble_dots_example", (DL_FUNC) &_wibble_dots_example, 2},
    {"_wibble_add_data", (DL_FUNC) &_wibble_add_data, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_wibble(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
