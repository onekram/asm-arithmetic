cmake_minimum_required(VERSION 2.8)

project(asm)

set(CMAKE_ASM_SOURCE_FILE_EXTENSIONS "asm")
set(CMAKE_ASM_COMPILE_OBJECT "nasm -f elf64 -g -F dwarf -o <OBJECT> <SOURCE>")
SET(CMAKE_ASM_LINK_EXECUTABLE "ld <OBJECTS> -o <TARGET>")
enable_language(ASM)

add_executable(add src/add.asm)
add_executable(sub src/sub.asm)
add_executable(mul src/mul.asm)
