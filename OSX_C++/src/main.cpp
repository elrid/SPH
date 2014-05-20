//standard libraries
#include <stdio.h>	  /* printf, scanf, puts, NULL */
#include <stdlib.h>	 /* srand, rand */
#include <time.h>	   /* time */
#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>
using namespace std;
//opengl headers
#include <GLEW/glew.h>
#include <GLUT/glut.h>

//opengl mathematics
#include "glm/glm.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/matrix_inverse.hpp"

//functions for shader compilation and linking
#include "shaderhelper.h"

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define SQ(x) ((x) * (x))
#define G_CONST 6.67384E-20

//struct for loading shaders
ShaderProgram shaderProgram;

//Uniforms for shader
typedef struct {
    GLuint Projection;
    GLuint Modelview;
    GLuint NormalMatrix;
    GLuint ModelMatrix;
    GLuint LightPosition;
    GLuint EyePosition;
    GLuint AmbientMaterial;
    GLuint DiffuseMaterial;
    GLuint TessLevelInner;
    GLuint TessLevelOuter;
    GLuint Time;
    GLuint GenerateNormals;
    GLuint BumpCoeff;
    
} ShaderUniforms;

//window size
int windowWidth = 800;
int windowHeight = 600;

//last mouse coordinates
int mouseX,mouseY;
int buttonDown=0;
int lastDrawCall = 0;
float timePassed = 0;
float speed = 0.00001f;
float fieldOfView = 45.0;
float aspectRatio = 1;
float zNear = 0.01;
float zFar = 1000.0;
float bound = 2.0f;
glm::vec3 dcen = glm::vec3(0.0);


float cPi = 3.1415;
float dif=1.0f; //importance clipping
float k=50.0f; //pressure coeff
float p0 = 6.0f; //outer pressure
float mu = 3.00f; //viscosity coeff
float sigma = 50.0f; //tension coeff
float bumpcoeff = 1;
const int amount=2000;
float mass= 1.0f;

//camera position
glm::vec3 eye(0,0,10.0);
glm::vec3 seye(10.0,cPi/2,cPi/2);
//reference point position
glm::vec3 *cen;
glm::vec3 real_cen;
//up vector direction (head of observer)
glm::vec3 up(0,1,0);

//matrices
glm::mat4x4 modelViewMatrix;
glm::mat4x4 projectionMatrix;
glm::mat4x4 modelViewProjectionMatrix;
glm::mat3x3 normalMatrix;

float* pData;	//pointer to object's internal data
unsigned int dataCount;

unsigned int* pIndexes;	//pointer to indexes (list of vetrices) 
unsigned int indexesCount;

GLuint vbo[3];//VertexBufferObject one for MeshVertexData, another for Indexes and last for normals
GLuint vao;//one VertexArrayObject
GLuint tex;

///defines drawing mode
bool halt = false;

//global data for dots

//texture identificator
GLuint texId;


//names of shader files. program will search for them during execution
//don't forget place it near executable
char VertexShaderName[] = "Tess.vert";
char TessControlShaderName[] = "Tess.tessctl";
char TessEvalShaderName[] = "Tess.tessevl";
char GeometryShaderName[] = "Tess.geom";
char FragmentShaderName[] = "Tess.frag";

ShaderUniforms Uniforms;


float randf(float range=1.0f, float from = 0.0f)
{
	if(range==1.0f)
		return from + float(rand())/RAND_MAX; // optimization, i suppose
	return from + range*float(rand())/RAND_MAX;
}

class dot
{
private:
    float mass;
    glm::vec3 position;
    glm::vec3 velocity;
    glm::vec3 acceleration;
public:
    float density;
    void process(float dt);
    void setForce(glm::vec3 force) { acceleration = force/mass; };
    void addForce(glm::vec3 force) { acceleration += force/mass; }
    float getMass() { return mass; } 
    glm::vec3 getPosition() { return position; }
    glm::vec3 getVelocity() { return velocity; }
    dot() {};
    dot(float mass_in, glm::vec3 pos_in, glm::vec3 vel_in=glm::vec3(0)):mass(mass_in),position(pos_in),velocity(vel_in),acceleration(glm::vec3(0)),density(mass_in) {};
};

void dot::process(float dt)
{
    acceleration.y-=9.81f;
    velocity+=dt*acceleration;
    position+=dt*velocity;
    
    if(position.x>bound)
    {
        position.x = bound - randf(0.01);
        velocity.x*=-0.3f;
    }
    if(position.x<-bound)
    {
        position.x = -bound + randf(0.01);
        velocity.x*=-0.3f;
    }
    if(position.z>bound)
    {
        position.z = bound - randf(0.01);
        velocity.z*=-0.3f;
    }
    if(position.z<-bound)
    {
        position.z = -bound + randf(0.01);
        velocity.z*=-0.3f;
    }
    if(position.y<0.0f)
    {
        position.y=0.0f + randf(0.01);
        velocity.y*=-0.1f;
    }
}

dot* liquid;
float** dist;




void printLastError()
{
    GLenum err = glGetError();
    cout<<gluErrorString(err)<<endl;
}



void step()
{
	int n = amount; 
	//should be set from outer space.
	glm::vec3 posi, posj;
	glm::vec3 powi, powj;

	int i,j;
#pragma omp parallel for num_threads(8)
	for(i=0; i<n; i++)
	{
		liquid[i].density = liquid[i].getMass();
		liquid[i].setForce(glm::vec3(0));
	}
	float tmp, sqdif = dif*dif;
	float wconst = 315 / (64 * 3.1415 * pow(dif,9)), Wpoly6d, Wpoly6dd;
#pragma omp parallel for num_threads(8)
	for (i = 0; i < n; i++)
	{
		for ( j = i; j < n; j++)
		{ 
			if(i!=j)
			{
				posi = liquid[i].getPosition();
				posj = liquid[j].getPosition();
				if(abs(posi.x-posj.x)<dif && abs(posi.y-posj.y)<dif && abs(posi.z-posj.z)<dif)
				{
					dist[i][j] = glm::length(posi-posj);
					if(dist[i][j]>dif)
						tmp=0;
					else
						tmp = (sqdif - dist[i][j] * dist[i][j]);
					tmp*=wconst*tmp*tmp;
					liquid[i].density += liquid[j].getMass() * tmp;
					liquid[j].density += liquid[i].getMass() * tmp;

				}
			}
		}
	}
#pragma omp parallel for num_threads(8)
	for (i = 0; i < n; i++)
	{
		for ( j = i; j < n; j++)
		{
			if(i!=j)
			{
				posi = liquid[i].getPosition();
				posj = liquid[j].getPosition();
				
				if(abs(posi.x-posj.x)<dif && abs(posi.y-posj.y)<dif && abs(posi.z-posj.z)<dif)
				{
					if(dist[i][j]<dif)
					{
						tmp = (sqdif - dist[i][j] * dist[i][j]);
						Wpoly6d = - wconst * 6 * dist[i][j] * tmp*tmp;
						Wpoly6dd = wconst * 6 * tmp * (-3*(tmp)+2*sqdif);

						//pressure
						tmp = k*(liquid[i].density+liquid[j].density - 2*p0)*Wpoly6d/2;
                        //tmp = p0*k * (pow(liquid[i].density/p0,7) + pow(liquid[j].density/p0,7) - 2) * Wpoly6d/7;
						powi = tmp*liquid[j].getMass()/liquid[j].density * (posj-posi);
						powj = tmp*liquid[i].getMass()/liquid[i].density * (posi-posj);

						//viscosity
						powi += mu * Wpoly6dd * liquid[j].getMass()/liquid[j].density * (liquid[j].getVelocity() - liquid[i].getVelocity());
						powj += mu * Wpoly6dd * liquid[i].getMass()/liquid[i].density * (liquid[i].getVelocity() - liquid[j].getVelocity());

						//tension
						powi -= sigma * Wpoly6dd * liquid[j].getMass()/liquid[j].density * (posi-posj);
						powj -= sigma * Wpoly6dd * liquid[i].getMass()/liquid[i].density * (posj-posi);
                        
						liquid[i].addForce(powi);
						liquid[j].addForce(powj);

					}
				}
			}
		}
	}
}

void init()
{
	//antialiasing
    
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable( GL_POLYGON_SMOOTH );
    glHint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
    glEnable(GL_MULTISAMPLE);
    // Initialize various state:
    //glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
	//initialize shader program
    
	shaderProgram.init(VertexShaderName, TessControlShaderName, TessEvalShaderName, GeometryShaderName, FragmentShaderName);
	//use this shader program
	glUseProgram(shaderProgram.programObject);
    
    //Init uniforms
    Uniforms.Projection = glGetUniformLocation(shaderProgram.programObject, "Projection");
    Uniforms.Modelview = glGetUniformLocation(shaderProgram.programObject, "Modelview");
    Uniforms.NormalMatrix = glGetUniformLocation(shaderProgram.programObject, "NormalMatrix");
    Uniforms.ModelMatrix = glGetUniformLocation(shaderProgram.programObject, "ModelMatrix");
    Uniforms.LightPosition = glGetUniformLocation(shaderProgram.programObject, "LightPosition");
    Uniforms.EyePosition = glGetUniformLocation(shaderProgram.programObject, "EyePosition");
    Uniforms.AmbientMaterial = glGetUniformLocation(shaderProgram.programObject, "AmbientMaterial");
    Uniforms.DiffuseMaterial = glGetUniformLocation(shaderProgram.programObject, "DiffuseMaterial");
    Uniforms.TessLevelInner = glGetUniformLocation(shaderProgram.programObject, "TessLevelInner");
    Uniforms.TessLevelOuter = glGetUniformLocation(shaderProgram.programObject, "TessLevelOuter");
    Uniforms.Time = glGetUniformLocation(shaderProgram.programObject, "time");
    Uniforms.GenerateNormals = glGetUniformLocation(shaderProgram.programObject, "generateNormals");
    Uniforms.BumpCoeff = glGetUniformLocation(shaderProgram.programObject, "bumpcoeff");
    
    cen = new glm::vec3(0);
    real_cen = *cen;

    liquid = new dot[amount];
	dist = new float*[amount];
    for(int i=0; i<amount;i++)
        dist[i] = new float[amount];
    for(int i=0; i<10; i++)
        for(int j=0; j<10; j++)
            for(int k=0; k<10; k++)
            {
                liquid[i + 10*j + 100*k] = dot(mass,glm::vec3(i/5.0f - 1.0f + randf(0.01),
                                                              j/5.0f + 1.0f + randf(0.01),
                                                              k/5.0f - 1.0f + randf(0.01)));
                liquid[1000+ i + 10*j + 100*k] = dot(mass,glm::vec3(i/5.0f - 1.0f + randf(0.01),
                                                                    j/5.0f + 10.0f + randf(0.01),
                                                                    k/5.0f - 1.0f + randf(0.01)));
            }
                
	//for(int i=0; i<amount; i++)
	//	liquid[i] = dot(mass,glm::vec3(randf(2.0f,-1.0f),randf(2.0f,2.0f),randf(2.0f,-1.0f)));

	unsigned int Faces[] = {
        2, 1, 0,
        3, 2, 0,
        4, 3, 0,
        5, 4, 0,
        1, 5, 0,
        
        11, 6,  7,
        11, 7,  8,
        11, 8,  9,
        11, 9,  10,
        11, 10, 6,
        
        1, 2, 6,
        2, 3, 7,
        3, 4, 8,
        4, 5, 9,
        5, 1, 10,
        
        2,  7, 6,
        3,  8, 7,
        4,  9, 8,
        5, 10, 9,
        1, 6, 10 };
    
    float Verts[] = {
        0.000,  0.000,  1.000,
        0.894,  0.000,  0.447,
        0.276,  0.851,  0.447,
        -0.724,  0.526,  0.447,
        -0.724, -0.526,  0.447,
        0.276, -0.851,  0.447,
        0.724,  0.526, -0.447,
        -0.276,  0.851, -0.447,
        -0.894,  0.000, -0.447,
        -0.276, -0.851, -0.447,
        0.724, -0.526, -0.447,
        0.000,  0.000, -1.000 };
    

	//generate flag
	dataCount = sizeof(Verts) / sizeof(Verts[0]);
	indexesCount = sizeof(Faces) / sizeof(Faces[0]);

	pData = new float [dataCount];
	pIndexes = new unsigned int [indexesCount];

	for (unsigned int i=0; i<dataCount; i++)
	{
        pData[i] = Verts[i];
	}
    
	for (unsigned int i=0; i<indexesCount; i++)
	{
        pIndexes[i] = Faces[i];
	}

    printLastError();
	glGenVertexArrays( 1, &vao );
    glBindVertexArray ( vao );
    
	glGenBuffers ( 3, (GLuint*)&vbo[0]);
    
	glBindBuffer ( GL_ARRAY_BUFFER, vbo[0] );
    printLastError();
    glBufferData ( GL_ARRAY_BUFFER, dataCount*sizeof(float), pData, GL_STATIC_DRAW );
    printLastError();
    
	glBindBuffer ( GL_ARRAY_BUFFER, vbo[2] );
    printLastError();
    glBufferData ( GL_ARRAY_BUFFER, dataCount*sizeof(float), pData, GL_STATIC_DRAW );
    printLastError();

	glBindBuffer ( GL_ELEMENT_ARRAY_BUFFER, vbo[1] );
    printLastError();
    glBufferData ( GL_ELEMENT_ARRAY_BUFFER, indexesCount*sizeof(unsigned int), pIndexes, GL_STATIC_DRAW );
    printLastError();
    

	int	loc = glGetAttribLocation(shaderProgram.programObject,"Position");
	if (loc>-1)
	{
        
        glEnableVertexAttribArray (loc);
        glVertexAttribPointer(loc, 3, GL_FLOAT, GL_FALSE, 0, 0);
        printLastError();
	}
	int loc2 = glGetAttribLocation(shaderProgram.programObject,"Normal");
	if (loc2>-1)
	{
        glEnableVertexAttribArray (loc2);
		glVertexAttribPointer(loc2, 3, GL_FLOAT, GL_FALSE, 0, 0);
        printLastError();
	}
}

///called when window size is changed
void reshape(int width, int height)
{
    windowWidth = width;
    windowHeight = height;
    //set viewport to match window size
    glViewport(0, 0, width, height);
    

    aspectRatio = float(width)/float(height);

    //set projection matrix
    projectionMatrix = glm::perspective(fieldOfView,aspectRatio,zNear,zFar);
}


glm::vec3 tospheric(glm::vec3 coords)
{
    
    glm::vec3 n = glm::normalize(coords);
    return glm::vec3(glm::length(coords),
                     (n.z < 0) * cPi + atan(n.x/n.z) + cPi/2,
                     acos (n.y) );
}

glm::vec3 fromspheric(glm::vec3 coords)
{
    float x = coords.x*sin(coords.z)*sin(coords.y),
    y = coords.x*cos(coords.z),
    z = coords.x*sin(coords.z)*cos(coords.y);
    return glm::vec3(x,y,z);
}

void display()
{

 	step();   
    
    float dt = speed*(glutGet(GLUT_ELAPSED_TIME) - lastDrawCall);
    if(!halt)
        timePassed+=dt;
    lastDrawCall = glutGet(GLUT_ELAPSED_TIME);
    
    cout<< timePassed << "	||	dt: " << dt << "	||	calctime:" << dt/speed <<"	ms"<<endl;
    //Recalculate positions

    
    glUseProgram(shaderProgram.programObject);
    //cout<<"begin draw"<<endl;
    //init buffer
    glClearColor(0.5,0.5,0.5,1);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    
    //camera matrix. camera is placed in point "eye" and looks at point "cen".
    //modelMatrix is connected with current object.
    //glm::vec3 dcennew = (real_cen - *cen)/2.0;

    

    eye = fromspheric(seye) + (real_cen);
    glm::mat4x4 viewMatrix, modelMatrix;
    //float r = glm::length(real_cen);
    //viewMatrix = glm::lookAt(eye,real_cen*sqrt(r)/r,up);
    viewMatrix = glm::lookAt(eye,real_cen,up);
    //modelViewMatrix consists of viewMatrix and modelMatrix
    
    //moving our center
    glm::vec3 lightPosition;
    lightPosition = glm::vec3(0,10,0);
    
    //draw objects
    for (int i = 0; i < amount; i++)
    {

    	liquid[i].process(dt);
    	modelMatrix = glm::translate(glm::mat4(), liquid[i].getPosition());
    	modelMatrix = glm::scale(modelMatrix,glm::vec3(0.3f));

        modelViewMatrix = viewMatrix * modelMatrix;
        //calculate normal matrix
        normalMatrix = glm::inverseTranspose(glm::mat3(modelViewMatrix));
        
        //pass variables to the shaders

        glUniform1f(Uniforms.TessLevelInner, 3.0f);
        glUniform1f(Uniforms.TessLevelOuter, 3.0f);
        glUniform1f(Uniforms.Time, float(timePassed));
        glUniform1i(Uniforms.GenerateNormals, 0);
        glUniform1f(Uniforms.BumpCoeff, float(bumpcoeff));
        glUniform3fv(Uniforms.LightPosition, 1, glm::value_ptr(lightPosition));
        glUniform3fv(Uniforms.EyePosition, 1, glm::value_ptr(eye));
        glUniformMatrix4fv(Uniforms.Modelview,1,0,glm::value_ptr(modelViewMatrix));
        glUniformMatrix3fv(Uniforms.NormalMatrix,1,0,glm::value_ptr(normalMatrix));
        glUniformMatrix4fv(Uniforms.Projection,1,0,glm::value_ptr(projectionMatrix));
        glUniformMatrix4fv(Uniforms.ModelMatrix,1,0,glm::value_ptr(modelMatrix));
        
        //glBindVertexArray ( vao );

        //glBindBuffer(GL_ARRAY_BUFFER,vbo[0]);
		//glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vbo[1]);
	    
	    //Tesselation
	    glPatchParameteri(GL_PATCH_VERTICES, 3);
        //printLastError();
        glDrawElements(GL_PATCHES, indexesCount, GL_UNSIGNED_INT, 0);
        //printLastError();
        
    }

    //end frame visualization
    glutSwapBuffers();
    
}

//////////////////////////////////////////////////////////////////////////
///IdleFunction
void update()
{
	//make animation
	glutPostRedisplay();
}


/////////////////////////////////////////////////////////////////////////
///is called when key on keyboard is pressed
///use SPACE to switch mode
///TODO: place camera transitions in this function
void keyboard(unsigned char key, int mx, int my)
{
	if (key==' ')
        halt = !halt;
    if (key=='+')
        speed*=2;
    if (key=='-')
        speed/=2;
    if (key=='b')
        bumpcoeff = 1 - bumpcoeff;
    if (key=='*')
        bound*=2.0f;
    if (key=='/')
        bound*=0.99f;
    if (key=='~')
    {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    }
    if (key=='`')
    {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    }
    
}

/////////////////////////////////////////////////////////////////////////
///is called when mouse button is pressed
///TODO: place camera rotations in this function
void mouse(int button, int mode,int posx, int posy)
{
    
    mouseX = posx;
    mouseY = posy;
	if (button==GLUT_LEFT_BUTTON)
	{
        buttonDown = 1;
	}
    
	if (button==GLUT_RIGHT_BUTTON)
	{
        buttonDown = 2;
	}
    if(mode != GLUT_DOWN)
        buttonDown = 0;
}

void moveCamera(int posx, int posy)
{
    int dx = posx - mouseX;
    int dy = posy - mouseY;
    float coeff = 3E-3;

    mouseX = posx;
    mouseY = posy;
    int dt = 30;
    if(!halt)
        dt =  glutGet(GLUT_ELAPSED_TIME) - lastDrawCall;
	if (buttonDown==2)
	{
        if(dy>0)
            
            fieldOfView*=1 + dy*coeff;
        else
            fieldOfView/=1 - dy*coeff;

        projectionMatrix = glm::perspective(fieldOfView,aspectRatio,zNear,zFar);
	}
    
	if (buttonDown==1)
	{
        
        if(dy>0)
            seye.x*= 1 + dy*coeff;
        else
            seye.x/=1 - dy*coeff;
        if(seye.x > 100.0f)
            seye.x = 100.0f;
        if(seye.x < 0.3f)
            seye.x = 0.3f;
    }
}

void rotateCamera(int x, int y)
{
    int dt = 30;
    float coeff = 0.3;
    if(!halt)
    {
        dt = glutGet(GLUT_ELAPSED_TIME) - lastDrawCall;
        
    }
    int dx = x - mouseX;
    int dy = y - mouseY;
    mouseX = x; mouseY = y;
    
    float dPhi = coeff*float(dx)/dt;
    float dTheta = coeff*float(dy)/dt;
    seye+=glm::vec3(0,dPhi,dTheta);
    if(seye.z>(cPi-0.01))
        seye.z = cPi-0.01;
    if(seye.z<0.01)
        seye.z = 0.01;
    if(seye.y>2*cPi)
        seye.y = seye.y - 2*cPi;
    if(seye.y<0.0)
        seye.y = seye.y + 2*cPi;
    
    
    
    
}


////////////////////////////////////////////////////////////////////////
///this function is used in case of InitializationError
void emptydisplay()
{
}


int main (int argc, char** argv)
{

/*	if(argc>=2)
	amount = atoi(argv[1]);
	else
		printf("Usage: lqs N [mass];\n");
	if(argc>=3)
		mass = atof(argv[2]);*/

	glutInit(&argc, argv);

    glutInitDisplayMode( GLUT_3_2_CORE_PROFILE | GLUT_RGBA | GLUT_DEPTH | GLUT_MULTISAMPLE);
    glutCreateWindow("Fluid Simulation");
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutReshapeWindow(windowWidth,windowHeight);
    glutIdleFunc(update);
    glutKeyboardFunc(keyboard);
    glutMouseFunc(mouse);
    glutPassiveMotionFunc(rotateCamera);
    glutMotionFunc(moveCamera);
    
    glewExperimental = GL_TRUE;
    glewInit();
    const char * slVer = (const char *) glGetString ( GL_SHADING_LANGUAGE_VERSION );
    const char * glVer = (const char *) glGetString ( GL_VERSION );
    cout << "GLSL Version: " << slVer << endl;
    cout << "GL Version:" << glVer << endl;
    
    //For debug attaching
    char c;
    //cin >> c;


    try
    {
        init();
    }
    catch (const char *str)
    {
        cout << "Error During Initialiation: " << str << endl;
        //start main loop with empty screen
        glutDisplayFunc(emptydisplay);
        //glutMainLoop();
        return -1;
    }
    

	

	glutMainLoop();
	return 0;
}