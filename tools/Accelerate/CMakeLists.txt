project(Accelerate)

# Required for accelerate-llvm-native
find_library(LIBFFI libffi)

execute_process(COMMAND "bash" "${CMAKE_CURRENT_SOURCE_DIR}/compile.sh" "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}")
