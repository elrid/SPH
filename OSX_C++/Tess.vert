#version 400

in vec4 Position;
in vec4 Normal;
out vec3 vPosition;
out vec3 vNormal;

void main()
{
    vPosition = Position.xyz;
    vNormal = Normal.xyz;
}
