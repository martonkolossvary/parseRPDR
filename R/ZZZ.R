.onAttach <- function(libname, pkgname) {
  options(future.globals.maxSize = 100*1024^3)
  data.table::setDTthreads(threads = 1, restore_after_fork = FALSE)
  packageStartupMessage("

                                                        _/_/_/    _/_/_/    _/_/_/    _/_/_/
     _/_/_/      _/_/_/  _/  _/_/    _/_/_/    _/_/    _/    _/  _/    _/  _/    _/  _/    _/
    _/    _/  _/    _/  _/_/      _/_/      _/_/_/_/  _/_/_/    _/_/_/    _/    _/  _/_/_/
   _/    _/  _/    _/  _/            _/_/  _/        _/    _/  _/        _/    _/  _/    _/
  _/_/_/      _/_/_/  _/        _/_/_/      _/_/_/  _/    _/  _/        _/_/_/    _/    _/
 _/
_/

                                by: M\U00E1rton Kolossv\U00E1ry, MD PhD

Please cite:
Kolossv\U00E1ry M et al.
Deep Learning Analysis of Chest Radiographs to Triage Patients with Acute Chest Pain Syndrome.
Radiology. 2023;306(2):e221926.
DOI: 10.1148/radiol.221926
")
}
