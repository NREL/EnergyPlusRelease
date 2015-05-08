
INCLUDE_DIRECTORIES( "${CMAKE_SOURCE_DIR}/SourceCode/Expat/lib")
INCLUDE_DIRECTORIES( "${CMAKE_SOURCE_DIR}/SourceCode/")

SET(SRC 
  main.h
  main.c
  fmiFunctions.h
  fmiModelTypes.h
  fmiPlatformTypes.h
  fmumini.c
  fmumini.h
  stack.c
  stack.h
  util.c
  util.h
  xml_parser_cosim.c
  xml_parser_cosim.h
  eplusModelFunctions.h
  fmiModelFunctions.h
  fmiModelTypes.h
)

ADD_LIBRARY( fmi SHARED ${SRC} )

TARGET_LINK_LIBRARIES( fmi epexpat miniziplib )

INSTALL( TARGETS fmi 
  RUNTIME DESTINATION bin 
  LIBRARY DESTINATION lib 
)

if( UNIX )
set_target_properties(
  fmi
  PROPERTIES
  INSTALL_NAME_DIR "@executable_path/../lib"
)
endif()

