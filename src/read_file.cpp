#include <fstream>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector read_file_call(CharacterVector path) {
  Function normalize_path("normalizePath");
  std::string fname = as<std::string>(normalize_path(path));
  std::ifstream in(fname.c_str());
  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);
  in.read(&contents[0], contents.size());
  in.close();
  return contents;
}
