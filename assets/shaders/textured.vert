#version 430 core
//version 220

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColour;
layout(location = 2) in vec2 uvCoords;
//in vec4 vPosition;
//in vec2 uvCoords;

out vec2 uv;
out vec4 co;

void main() {
   gl_Position = vPosition;
	 uv = uvCoords;
   //co = vColour;
}