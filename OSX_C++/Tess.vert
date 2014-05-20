#version 400

in dvec4 Position;
in dvec4 Normal;
out dvec3 vPosition;
out dvec3 vNormal;

void main()
{
    vPosition = Position.xyz+dvec3(0.01,0.01,0.01);
    vNormal = Normal.xyz;
}
