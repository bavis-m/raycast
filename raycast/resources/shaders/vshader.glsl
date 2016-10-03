#version 400

uniform mat4 view;

in vec2 pos;
in vec2 uv;

out vec2 fragUV;
out vec2 fragPos;

void main () {
  fragPos = pos;
  fragUV = uv;
  gl_Position = view * vec4(pos, 0.0, 1.0);
}