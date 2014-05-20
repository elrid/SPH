#version 400


uniform dmat4 Modelview;
uniform dmat3 NormalMatrix;
layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;
in dvec3 teWorldPosition[3];
in dvec3 tePosition[3];
in dvec3 teNormal[3];
out vec3 gWorldNormal;
out vec3 fragModelPosition;
out vec3 fragWorldCoord;
out mat3 toTangentSpace;

dvec3 normlze(dvec3 _in)
{
    float len = sqrt(float(_in.x*_in.x + _in.y*_in.y + _in.z*_in.z));
    return _in/double(len);
}
vec3 normalme(dvec3 _in)
{
    float len = sqrt(float(_in.x*_in.x + _in.y*_in.y + _in.z*_in.z));
    float x = float(_in.x/double(len));
    float y = float(_in.y/double(len));
    float z = float(_in.z/double(len));
    return vec3(x,y,z);
}

void main()
{
    
    double cPi = 3.1416;
    
    dmat3 deltaPos = dmat3((teWorldPosition[1] - teWorldPosition[0]),
                           (teWorldPosition[2] - teWorldPosition[1]),
                           (teWorldPosition[1] - teWorldPosition[2]));
    dvec3 tangent;
    dvec3 normal;
    dvec3 bitangent;
    
    for(int i=0; i<3; i++)
    {
        normal = teNormal[i];//!!!
        tangent = cross(deltaPos[i],normal);
        bitangent = cross(normal,tangent);
        
        
        toTangentSpace = mat3(tangent,bitangent,normal);
        fragModelPosition = vec3(tePosition[i]);
        gWorldNormal = vec3(teNormal[i]); //temporary
        fragWorldCoord = vec3(teWorldPosition[i]);
        gl_Position = gl_in[i].gl_Position;
        
        EmitVertex();
    }
    
    EndPrimitive();
}