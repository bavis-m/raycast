#pragma once

#include "stdafx.h"
#include "ffi.h"

using namespace std;
using namespace glm;

bool loadTexture(string, wstring);
bool checkGLErrors(bool print = true);
char* readFile(char* name);
bool compileShader(GLint, char*);
bool linkProgram(GLuint, char*);