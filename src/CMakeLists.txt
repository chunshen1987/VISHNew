if (${HDF5_FOUND})
  set (LIBSRC Jetoutputh5.for)
  add_library (hdf5 ${LIBSRC})
  add_executable(VISHNew.e
    VISH2p1V1.10.0.for 
    PhyBdary-1.10.for 
    InputEOS-1.3.for 
    OSCARoutput.for 
    Arsenal-0.8.for 
    Initialization-1.03.for
    InputFun-1.29RC6.for 
    cornelius2.f90
    )
  target_link_libraries (VISHNew.e hdf5)
  set_target_properties (VISHNew.e PROPERTIES COMPILE_FLAGS "-DUSE_HDF5")
else (${HDF5_FOUND})
  add_executable(VISHNew.e
    VISH2p1V1.10.0.for 
    PhyBdary-1.10.for 
    InputEOS-1.3.for 
    OSCARoutput.for 
    Arsenal-0.8.for 
    Initialization-1.03.for
    InputFun-1.29RC6.for 
    cornelius2.f90
    JetOutput-0.8.for
    )
endif (${HDF5_FOUND})

install (TARGETS VISHNew.e DESTINATION ${CMAKE_HOME_DIRECTORY})
