#version 430 core

// Interpolated values from the vertex shaders
in vec2 uv;
in vec4 co;
//layout (binding = 0) uniform sampler2D tex;
uniform sampler2D tex;

// Ouput data
out vec4 fColor;

void main() {
	 //fColor = co
	 fColor = vec4(0.0, texture(tex, uv).gb, 1.0) + vec4(0.1, 0.1, 0.1, 0.0);
	 //fColor = vec4((1+sin(gl_FragCoord.x/200))/2, (1+sin(gl_FragCoord.y/400))/2, 0, 1);
	 //fColor = vec4(1, 0, 0, 1);
}