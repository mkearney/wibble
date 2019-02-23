// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <string>
#include <boost/algorithm/string/predicate.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector wabble_call(std::string url,
                            List query,
                            std::string path = "") {
  // Base URL
  if (path != "") {
    if (boost::algorithm::ends_with(url, "/") == 0) {
      url = url + "/";
    }
  }

  // Build query
  int n = query.size();
  std::string params = "";
  if (n > 0) {
    CharacterVector nms = query.attr("names");
    for (int i=0; i < n; i++) {
      if (i == 0) params = params + "?";
      if (i > 0) params = params + "&";
      params = params + nms[i] + "=" + Rcpp::as<std::string>(query[i]);
    }
  }

  // Combine
  url = url + path + params;
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
