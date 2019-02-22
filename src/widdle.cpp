// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <iostream>
#include <string>
#include <sstream>
#include <curl/curl.h>
#include <boost/algorithm/string/predicate.hpp>

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
CharacterVector api_call(std::string url,
                         CharacterVector query,
                         CharacterVector value,
                         std::string path = "") {
  int n = query.size();
  if (path != "") {
    if (boost::algorithm::ends_with(url, "/") == 0) {
      url = url + "/";
    }
  }
  url = url + path + "?";
  for (int i=0; i < n; i ++) {
    if (i > 0) url = url + "&";
    url = url + query[i] + "=" + value[i];
  }
  return url;
}

std::string one_api_call(std::string url,
                         CharacterVector query,
                         CharacterVector value,
                         std::string path = "") {
  int n = query.size();
  if (path != "") {
    if (boost::algorithm::ends_with(url, "/") == 0) {
      url = url + "/";
    }
  }
  url = url + path + "?";
  for (int i=0; i < n; i ++) {
    if (i > 0) url = url + "&";
    url = url + query[i] + "=" + value[i];
  }
  return url;
}

// [[Rcpp::export]]
CharacterVector api_calls(CharacterVector url,
                          CharacterVector query,
                          CharacterVector value,
                          CharacterVector path = "") {
  int n = url.size();
  int np = path.size();
  CharacterVector s = CharacterVector(n);
  CharacterVector paths = CharacterVector(n);

  // make path same length as URL
  if (np==1) {
    for (int i=0; i < n; i ++) {
      paths[i] = path[0];
    }
  } else {
    for (int i=0; i < n; i ++) {
      paths[i] = path[i];
    }
  }

  for (int i=0; i < n; i ++) {
    std::string si = Rcpp::as<std::string>(url[i]);
    std::string pi = Rcpp::as<std::string>(paths[i]);
    s[i] = one_api_call(si, query, value, pi);
  }
  return s;
}


static size_t  web_write(void *ptr,
                         size_t size, size_t nmemb,
                         std::stringstream *stream)
{
  stream->write((char*)ptr,size*nmemb);
  return size*nmemb;
}


// [[Rcpp::export]]
std::string webbler(std::string url) {
  CURL *curl = curl_easy_init();
  std::stringstream ss;
  std::string D = "";
  if(curl!=NULL) {

    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA,&ss);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, web_write);

    curl_easy_perform(curl);
    if(!ss.eof())
    {
      D = ss.str();
    }
  }
  if(curl!=NULL)
    curl_easy_cleanup(curl);
  return D;
}


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
