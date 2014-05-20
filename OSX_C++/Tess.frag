#version 400

out vec4 colorOut;
in vec3 gWorldNormal;
in vec3 gTriDistance;
in vec3 gPatchDistance;
in float gPrimitive;
in vec3 fragModelPosition;
in vec3 fragWorldCoord;
in mat3 toTangentSpace;
uniform dvec3 LightPosition;
uniform dvec3 EyePosition;
uniform float time;
uniform sampler2D textr;
uniform float bumpcoeff;
uniform int planettype;

dvec3 normlze(dvec3 _in)
{
    float len = sqrt(float(_in.x*_in.x + _in.y*_in.y + _in.z*_in.z));
    return _in/double(len);
}

vec3 textureDay(sampler2D tex, vec2 coords)
{
    
    return texture(tex,vec2( coords.x - floor(coords.x), (coords.y - floor(coords.y))/4 +0.25 )).rgb;
}

vec3 textureNight(sampler2D tex, vec2 coords)
{
    return texture(tex,vec2( coords.x - floor(coords.x), (coords.y - floor(coords.y))/4 )).rgb;
}

vec3 textureClouds(sampler2D tex, vec2 coords)
{
    return texture(tex,vec2( coords.x - floor(coords.x), (coords.y - floor(coords.y))/4 +0.5 )).rgb;
}

vec3 textureNormal(sampler2D tex, vec2 coords)
{
    return texture(tex,vec2( coords.x - floor(coords.x), (coords.y - floor(coords.y))/4 +0.75 )).rgb;
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
    if(planettype==1)
        N = normalize(textureNormal(textr,texcoord)*2.0 - vec3(1.0));
    else
        N = vec3(0,0,1);
    vec3 Ne = vec3(0,0,1);
    
    if(planettype==1)
        N = N*clamp(float(bumpcoeff),0,1) + clamp(1-float(bumpcoeff),0,1)*Ne;
    V = normalize(V);
    L = normalize(L);
    L = vec3(0,0,L.z);
    //V = vec3(0,0,L.z);
    float cosNL = dot(N, L);
    float cosNeL = dot(Ne, L);
    float cosNeV = dot(Ne, V);
    float sinNeV = 1 - cosNeV*cosNeV;
    sinNeV = pow(sinNeV,8);
    if(cosNL<0)
        cosNL = 0;
    if(cosNeL<0)
        cosNeL = 0;
    
    float df = cosNL;
    
    vec3 color;
    if(planettype == 0)
    {
        color = df * texture(textr,texcoord).rgb;
    }
    if(planettype == 1)
    {
        vec2 cloudscoord = vec2(texcoord.x+time/360,texcoord.y + sin(float(time)/60)/40);
        vec3 clouds = textureClouds(textr,cloudscoord);
        float cloudCoeff = clamp(length(clouds),0,1);
        
        
        // day || clouds || sky
        // night || clouds
        
        color = (1 - cloudCoeff) * textureDay(textr,texcoord) + clouds;
        color = (1 - sinNeV)*color*df;
        color+= (1 - df)  * (1 - cloudCoeff) * textureNight (textr,texcoord)/3.0;
        color = cosNeL * ((1 - sinNeV) * color + sinNeV*vec3(0.5,0.5,1)) + (1 - cosNeL) * color;
    }
    if(planettype == 2)
        color = texture(textr,texcoord).rgb;
    vec3 outv = color;//color ;//vec3(abs(dot(normalize(N), normalize(V))));
    colorOut = vec4(outv, 1.0);
    
}