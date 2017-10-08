#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColour;
layout(location = 2) in vec2 uvCoords;

// Output data ; will be interpolated for each fragment.
out vec2 uv;
out vec4 co;

void main() {
   gl_Position = vPosition;

// The color of each vertex will be interpolated
// to produce the color of each fragment
	 uv = uvCoords;
   co = vColour;
}