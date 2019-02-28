#include <Rcpp.h>
#include <iostream>
#include <string>
#include <sstream>
#include <curl/curl.h>

using namespace Rcpp;

static size_t  web_write(void *ptr,
                         size_t size, size_t nmemb,
                         std::stringstream *stream) {
  stream->write((char*)ptr,size*nmemb);
  return size*nmemb;
}

// [[Rcpp::export]]
List webble_call(std::string url) {
  ExpressionVector exp("function(x) suppressWarnings(xml2:::read_xml.raw(charToRaw(x), 'UTF-8', as_html = TRUE, options = 289L))");
  Function f = exp.eval();

  CURL *curl = curl_easy_init();
  std::stringstream ss;
  std::string res = "";
  if (curl!=NULL) {
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA,&ss);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, web_write);
    curl_easy_perform(curl);
    if(!ss.eof()) res = ss.str();
  }
  if (curl!=NULL) curl_easy_cleanup(curl);
  return f(res);
}

