#pragma once

typedef struct _columnSection {
	int column;
	int startY;
	int endY;
	int tex;
	float startU;
	float startV;
	float endU;
	float endV;
} columnSection;

typedef struct _renderParams {
	int width;
	int height;
} renderParams;

typedef struct _updateFrame {
	double time;
	int* keysPressed;
	int numKeysPressed;
	int* keysReleased;
	int numKeysReleased;
	int mouseRelX;
	int mouseRelY;
} updateFrame;

typedef struct _engineFrame {
	columnSection* columnMemory;
	int numColumnSections;
} engineFrame;

/*
typedef struct _renderFrame {
	int numVerts;
	int numTris;
	float* vertData;
	int* triData;
} renderFrame;
*/