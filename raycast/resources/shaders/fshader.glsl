#version 400

in vec2 fragPos;
in vec2 fragUV;

uniform sampler2D tex;
out vec4 color;

void main () {
  color = texture(tex, fragUV);
}