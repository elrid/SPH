#version 400

out vec4 colorOut;
in vec3 gWorldNormal;
in vec3 gTriDistance;
in vec3 gPatchDistance;
in float gPrimitive;
in vec3 fragModelPosition;
in vec3 fragWorldCoord;
in mat3 toTangentSpace;
uniform vec3 LightPosition;
uniform vec3 EyePosition;
uniform float time;
uniform float bumpcoeff;

vec3 normlze(vec3 _in)
{
    float len = sqrt(float(_in.x*_in.x + _in.y*_in.y + _in.z*_in.z));
    return _in/float(len);
}



vec2 tospheric(vec3 coords)
{
    vec3 n = normalize(coords);
    float cPi = 3.1416;
    return vec2((sign(n.z)+1) * 0.5 * cPi + atan(n.x/n.z) + cPi/2,
                acos (n.y) );
}

void main()
{
    
    float cPi = 3.1416;
    vec2 texcoord = tospheric(fragModelPosition);
    // x = phi, y = theta
    texcoord.x = texcoord.x / ( 2 * cPi );
    texcoord.y = 1 - texcoord.y / cPi;
    mat3 NTTS = toTangentSpace;
    
    
    NTTS[2] = normalize(gWorldNormal);
    NTTS[1] = normalize(cross(NTTS[2],NTTS[0]));
    NTTS[0] = normalize(cross(NTTS[2],NTTS[1]));
    
    NTTS = transpose(NTTS);
    
    
    vec3 L = vec3(NTTS*normalize(LightPosition - fragWorldCoord));
    vec3 V = vec3(NTTS*normalize(fragWorldCoord - EyePosition));
    vec3 N;
    N = vec3(0,0,1); // normal from texture
    vec3 R = -reflect(L,N);
    vec3 Ne = vec3(0,0,1);
    
    V = normalize(V);
    L = normalize(L);
    //L = vec3(0,0,L.z);
    //V = vec3(0,0,L.z);
    float cosNL = dot(N, L);
    float cosNeL = dot(Ne, L);
    float cosNeV = dot(Ne, V);
    float sinNeV = 1 - cosNeV*cosNeV;
    float cosLV = max(dot(-V,R),0);
    sinNeV = pow(sinNeV,8);
    if(cosNL<0)
        cosNL = 0;
    if(cosNeL<0)
        cosNeL = 0;
    
    float df = cosNL;
    
    vec3 color = vec3(0.01,0.01,0.5) + cosNL*vec3(0.02,0.02,1.0) + pow(cosLV,4) * vec3(1);
    
    vec3 outv = color;//color ;//vec3(abs(dot(normalize(N), normalize(V))));
    colorOut = vec4(outv, 1 * pow(cosLV,8) + 0.1);
    
}