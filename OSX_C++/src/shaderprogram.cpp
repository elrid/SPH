#include "shaderprogram.h"
#include <GLEW/glew.h>


#include <iostream>
using namespace std;

ShaderProgram::ShaderProgram()
{
	programObject=0;
}


ShaderProgram::~ShaderProgram()
{

	programObject=0;
}

void ShaderProgram::init(const char* vName, const char* tcName, const char* teName, const char* gName, const char* fName )
{
	GLint success; //local variable to check status
	
    //load and compile vertex shader
	success = vertex.readAndCompile(vName,GL_VERTEX_SHADER);
	if (!success)
		throw "Vertex Compilation Error";
	
    success = tesscontrol.readAndCompile(tcName,GL_TESS_CONTROL_SHADER);
	if (!success)
		throw "Tess Control Compilation Error";
	
    success = tesseval.readAndCompile(teName,GL_TESS_EVALUATION_SHADER);
	if (!success)
		throw "Tess Eval Compilation Error";
	
    success = geometry.readAndCompile(gName,GL_GEOMETRY_SHADER);
	if (!success)
		throw "Geometry Compilation Error";
	
    //load and compile fragment shader
	success = fragment.readAndCompile(fName,GL_FRAGMENT_SHADER);
	if (!success)
		throw "Fragment Compilation Error";
    
    
	//create programObject
	programObject = glCreateProgram();
	//attach shaders
	glAttachShader(programObject, vertex.shaderObject);
	glAttachShader(programObject, tesscontrol.shaderObject);
	glAttachShader(programObject, tesseval.shaderObject);
	glAttachShader(programObject, geometry.shaderObject);
	glAttachShader(programObject, fragment.shaderObject);
	
    //bind  fragment data location
    GLuint progWithFrag = programObject;
    glBindFragDataLocation(progWithFrag, 0, "colorOut");
    
    //link shaders in program
	glLinkProgram(programObject);
    
    printf("frag color binded: %d\n",glGetFragDataLocation(progWithFrag, "colorOut")+1);
	glGetProgramiv (programObject, GL_LINK_STATUS, &success);
	if (!success)
	{
		GLint maxLength = 0;
		glGetProgramiv(programObject, GL_INFO_LOG_LENGTH, &maxLength);

		char* errorLog = new char [maxLength];
		glGetProgramInfoLog(programObject, maxLength, &maxLength, &errorLog[0]);
		cout << errorLog << endl;
		delete[] errorLog;
		
		glDetachShader(programObject,vertex.shaderObject);
		glDetachShader(programObject,fragment.shaderObject);
		vertex.Release();
		fragment.Release();
		
		throw "Link Error";
	}
}
