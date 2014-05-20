#pragma once
#include "shader.h"

class ShaderProgram
{
	Shader vertex,tesscontrol,tesseval,geometry,fragment;
public:
	unsigned int programObject;
	
	ShaderProgram();
	~ShaderProgram();
	void init(const char* vName, const char* tcName, const char* teName, const char* gName, const char* fName );
};
