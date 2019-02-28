#include <Rcpp.h>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;


std::vector<std::string> tokenize_wrd_string( std::string text ) {
  //boost::trim_if(text, boost::is_punct());
  std::vector<std::string> results;
  boost::split(results, text, boost::is_any_of("\f\n\r\t\v "), boost::token_compress_on);
  return results;
}

// [[Rcpp::export]]
Rcpp::List tokenize_wrd_call(std::vector<std::string> strings ) {
  int len = strings.size();
  Rcpp::List output(len);
  for ( int i=0; i < len; i++ ) {
    output[i] = tokenize_wrd_string(strings[i]);
  }
  return output;
}



std::vector<char> tokenize_chr_string( std::string s ) {
  //boost::trim_if(text, boost::is_punct());
  std::vector<char> data(s.begin(), s.end());

  return data;
}

// [[Rcpp::export]]
Rcpp::List tokenize_chr_call(std::vector<std::string> strings ) {
  int len = strings.size();
  Rcpp::List output(len);
  for ( int i=0; i < len; i++ ) {
    output[i] = tokenize_chr_string(strings[i]);
  }
  return output;
}
