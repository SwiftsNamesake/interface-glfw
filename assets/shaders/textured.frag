#version 430 core

in vec2 uv;
in vec4 co;

layout (binding = 0) uniform sampler2D tex;

// Ouput data
out vec4 fColour;

void main() {
	 //fColour = co
	 fColour = vec4(texture(tex, vec2(uv.x, 1-uv.y)).rgb, 1.0);
	 //fColour = vec4((1+sin(gl_FragCoord.x/200))/2, (1+sin(gl_FragCoord.y/400))/2, 0, 1);
	 //fColour = vec4(1, 0, 0, 1);
}