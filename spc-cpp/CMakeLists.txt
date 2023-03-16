cmake_minimum_required(VERSION 3.18)

project(SolariumSPC VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(spc src/main.cpp)
