cmake_minimum_required (VERSION 2.6)
project (VISHNew Fortran)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
       "Choose the type of build, options are: None Debug Release."
       FORCE)
endif (NOT CMAKE_BUILD_TYPE)

option (EnableHDF5 "EnableHDF5" OFF)

# FFLAGS depend on the compiler
if (EnableHDF5)
  set (CMAKE_Fortran_COMPILER "h5fc")
endif (EnableHDF5)
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -cpp -fno-align-commons")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -cpp")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -cpp -heap-arrays")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -cpp -heap-arrays")
elseif (Fortran_COMPILER_NAME MATCHES "h5fc")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -cpp -lm -lz -fno-align-commons")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -cpp")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

add_subdirectory (src)

